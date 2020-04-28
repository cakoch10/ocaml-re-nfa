(** Deterministic finite automata *)

type state = Nfa.state
module StateSet = Nfa.StateSet
module StateMap = Map.Make(Int32)
module CharMap = Nfa.CharMap

(* val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t *)
let charmap_union (type a) (f : char -> a -> a -> a option) =
  let f k x y = match x, y with
    | None    , None    -> None
    | Some v  , None    -> Some v
    | None    , Some v  -> Some v
    | Some v1 , Some v2 -> f k v1 v2
  in CharMap.merge f

type dfa = {
  start : state;
  (** the start state *)

  finals: StateSet.t;
  (** the final (or "accept") states *)

  next: state -> state CharMap.t;
  (** the transition function, that maps a state and a character to the
      next state *)
}

let fold_states : 'a. (state -> 'a -> 'a) -> dfa -> 'a -> 'a =
  fun f dfa init ->
  let v = ref init in
  let seen = Hashtbl.create 10 in
  let rec visit state  =
    if not (Hashtbl.mem seen state) then begin
        v := f state !v;
        Hashtbl.add seen state ();
        CharMap.iter (fun _ -> visit) (dfa.next state)
      end
  in visit dfa.start; !v

let fold_transitions: 'a. (state * char * state -> 'a -> 'a) -> dfa -> 'a -> 'a =
  fun f dfa init ->
  fold_states
    (fun src v -> CharMap.fold (fun c dst -> f (src, c, dst)) (dfa.next src) v)
    dfa init

(** Add src--c-->dst to the transition set, replacing any existing src--c-->dst' *)
let add_transition (src, c, dst) trans =
  match StateMap.find src trans with
  | exception Not_found -> StateMap.add src (CharMap.singleton c dst) trans
  | cm ->                  StateMap.add src (CharMap.add c dst cm)    trans

(** Add src--c-->dst to the transition set, augmenting any existing src--c-->dst' *)
let add_transition' (src, c, dst) trans =
  match StateMap.find src trans with
  | exception Not_found ->
     StateMap.add src (CharMap.singleton c (StateSet.singleton dst)) trans
  | cm -> let dstset = match CharMap.find c cm with
                      | exception Not_found -> StateSet.singleton dst
                      | dstset              -> StateSet.add dst dstset
          in StateMap.add src (CharMap.add c dstset cm) trans

(** Build an NFA by reversing a DFA, inverting transition arrows,
   turning finals states into start states, and the start state into
   the final state *)
let reverse dfa =
  let map =
    fold_transitions (fun (s, c, t) -> add_transition' (t, c,s)) dfa StateMap.empty
  in
  { Nfa.start = dfa.finals;
    Nfa.finals = StateSet.singleton dfa.start;
    next =  fun s -> try StateMap.find s map with Not_found -> CharMap.empty }

(** Available transitions from a set of states *)
let transitions states nfa =
  StateSet.fold (fun s m ->
      let m' = nfa.Nfa.next s in
      charmap_union (fun _ s s' -> Some (StateSet.union s s')) m m')
    states
    CharMap.empty

(** Conversion to DFA via the powerset construction *)
let determinize : Nfa.nfa -> dfa =
  let module M = Map.Make(StateSet) in
  fun nfa ->
  let fresh = let r = ref 0l in fun () -> (r := Int32.succ !r; !r) in
  let rec build states (map, ts, finals) =
    match M.find states map with
    | state -> (state, map, ts, finals)
    | exception Not_found ->
       let state = fresh () in
       let finals = if not (StateSet.is_empty (StateSet.inter states nfa.Nfa.finals))
                    then StateSet.add state finals
                    else finals in
       let map = M.add states state map in
       let tsn = transitions states nfa in
       let map, ts, finals =
         CharMap.fold
           (fun c ss (map, ts, finals) ->
             let dst, map, ts, finals = build ss (map, ts, finals) in
             let ts = add_transition (state, c, dst) ts in
             (map, ts, finals))
           tsn
           (map, ts, finals)
       in 
       state, map, ts, finals
  in
  let start, _, trans, finals =
    build nfa.Nfa.start (M.empty, StateMap.empty, StateSet.empty)
  in
  let next s = try StateMap.find s trans with Not_found -> CharMap.empty in
  { start; finals; next }

(** Brzozowski's DFA minimization algorithm:
    reverse DFA to build an NFA and determinize, then do the same again *)
let minimize g = determinize (reverse (determinize (reverse g)))

let inject { start; finals; next } =
  { Nfa.start = Nfa.StateSet.singleton start;
    finals;
    next = fun s -> CharMap.map StateSet.singleton (next s) }

(** A simple DFA interpreter. *)
let accept dfa inp =
  let rec step cur = function
    | [] -> StateSet.mem cur dfa.finals
    | c :: cs ->
       match CharMap.find c (dfa.next cur) with
       | exception Not_found -> false
       | s -> step s cs in
  step dfa.start inp



let rec search_states dfa (state_map, word_map, queue) =
  match queue with
  | [] -> state_map, word_map
  | h :: t -> 
    (* need to explore h *)
    (* for c in char_set, check next h c *)
    let w = List.assoc h state_map in
    let update c s (s_map, w_map, q) = 
      if List.mem_assoc s s_map then (s_map, w_map, q) else
      (* need to add s to q and update maps *)
      let new_w = w ^ (Char.escaped c) in
      ((s, new_w)::s_map, (new_w, StateSet.mem s dfa.finals)::w_map, s::q)
      in
    search_states dfa (CharMap.fold update (dfa.next h) (state_map, word_map, t))


let get_accept_strings dfa =
  (* let state_map = [(dfa.start, "")] in
  let word_map = [("", StateSet.mem dfa.start dfa.finals)] in
  let q = [dfa.start] in *)
  let (_, word_map) = search_states dfa ([(dfa.start, "")], 
                                                 [("", StateSet.mem dfa.start dfa.finals)], 
                                                 [dfa.start])  in
  (* format for alpharegex here *)
  let pos, neg = List.partition (fun (_, b) -> b) word_map in
  let pos = List.split pos |> fst in 
  let neg = List.split neg |> fst in 
  let pos_str = String.concat "\n" pos in
  let neg_str = String.concat "\n" neg in
  let output = "++\n" ^ pos_str ^ ("\n--\n") ^ neg_str in
  let f = open_out "dfa_access_strings.txt" in
  let _ = Printf.fprintf f "%s" output; close_out f in
  pos, neg
