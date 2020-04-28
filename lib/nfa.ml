type state = int32
module StateSet = Set.Make(Int32)
module CharMap = Map.Make(Char)
type transitions = StateSet.t CharMap.t

type nfa = {
  start : StateSet.t;
  (** the start states *)

  finals: StateSet.t;
  (** the final (or "accept") states *)

  next: state -> transitions;
  (** the transition function, that maps a state and a character to a
      set of states *)
}

let find_states sym nfa m =
  try CharMap.find sym (nfa.next m)
  with Not_found -> StateSet.empty

let flat_map f ss = StateSet.fold (fun s -> StateSet.union (f s)) ss StateSet.empty
let nextss curs sym nfa = flat_map (find_states sym nfa) curs

(** A simple NFA interpreter. *)
let accept nfa inp =
  (** cur is the set of all the current states -- i.e. those states at
      which we have arrived by examining the input up to this point.
      Since the automaton is non-deterministic, encountering a character
      in a given state can cause transitions to multiple different
      states *)
  let rec step cur = function
    | [] -> StateSet.(not (is_empty (inter cur nfa.finals)))
    | c :: cs -> step (nextss cur c nfa) cs
  in step nfa.start inp


(* let get_accept_strings nfa = *)



(*
state_words = {self.minDFA.startstate : Automata.epsilon}
        words_accepting = {Automata.epsilon : (self.minDFA.startstate in self.minDFA.finalstates)}

        state_queue = set()
        state_queue.add(self.minDFA.startstate)
        
        # for i in self.minDFA.language:
        #     for s in self.minDFA.gettransitions(self.minDFA.startstate, i):
        #         state_queue.add(s)

        # state_queue = [self.minDFA.startstate]
        while len(state_queue) > 0:
            state = state_queue.pop()
            word = state_words[state]
            if word == Automata.epsilon:
                word = ""

            # need to check all state -> state' transitions
            for a in self.minDFA.language:
                for s in self.minDFA.gettransitions(state, a):
                    # need to check if s has already been seen
                    if not s in state_words:
                        new_word = word + a
                        state_words[s] = new_word
                        words_accepting[new_word] = (s in self.minDFA.finalstates)
                        state_queue.add(s)
        
        # remove epsilon
        words_accepting[""] = words_accepting[Automata.epsilon]
        del words_accepting[Automata.epsilon]

        return words_accepting
*)