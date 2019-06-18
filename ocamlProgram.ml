
let rec subset a b = match a with
 | x::y -> if List.mem x b then subset y b else false
 | [] -> true;;

let equal_sets a b =
 if subset a b && subset b a then true else false;;
(*
let rec equal_set a b = match a with
 | x::y -> if List.exists x b then equal_set y b else false
 | [] -> if b = [] then true else false;; 
 *)

let set_union a b = a @ b;;


let rec set_intersection a b = match a with
 | [] -> []
 | x::y -> if (List.mem x b) then List.sort_uniq compare (x::set_intersection y b) else set_intersection y b;;

let rec set_diff a b = match a with
 | [] -> []
 | x::y -> if List.mem x b then set_diff y b else List.sort_uniq compare (x::set_diff y b);;

let rec computed_fixed_point eq f x = if eq (f(x)) x then x else
computed_fixed_point eq f(f(x));;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let check_symbol symbol rRules =
match rRules with
|T _ -> false 
|N nonterm -> if symbol = nonterm then true else false;;

let rec check_nt_symbol symbol rsRules =
match rsRules with
 |[] -> false
 | rule::rest -> if check_symbol symbol rule then true else check_nt_symbol symbol rest;;

let rec isSymPresent symbol rules =
match rules with
 |[] -> false
 | head::tail -> let start = (head) in
if (check_nt_symbol symbol (snd start)) then true else (isSymPresent symbol tail);;


let rec checkRules rulesList newList start =
match rulesList with
|[] -> newList
| head::tail -> if ((isSymPresent (fst head) newList) || (fst head) = start)
then (checkRules tail (head::newList) start) else (checkRules tail newList start);;

let makeListofRules rulesList start =
 let lookThrough = checkRules rulesList [] start in
  computed_fixed_point (equal_sets) (fun x -> (checkRules rulesList x start)) lookThrough

let keepOrder rulesList start =
let returninglisRules = (makeListofRules rulesList start) in
List.filter (fun x -> List.mem x (returninglisRules)) (rulesList)


let rec filter_reachable g =
  (fst g, keepOrder (snd g) (fst g));;

