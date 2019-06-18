let message = "All bool functions should have all true with 1 false being the final test case";;
message;;
let subset_test1 = subset [1;2] [1;2;3];;
let subset_test2 = subset [3;1] [1;2;3];;
let subset_test3 = subset [] [1];;
let subset_test4 = subset [1] [2;3];;

let equal_setstest1 = equal_sets [1;2] [1;2];;
let equal_setstest2 = equal_sets [3] [3];;
let equal_setstest3 = equal_sets [1;2] [3;4];;

let uniontest1 = set_union [1] [2];;
let uniontest2 = set_union [1;2] [3;4];;

let interTest1 = set_intersection [1;2] [1;3];;
let interTest2 = set_intersection [1;2] [3;4];;

let diffTest1 = set_diff [1;2] [1;2];;
let diffTest2 = set_diff [1;2] [3;4];;

let fixed_test = computed_fixed_point (=) (fun x -> x / 5) 5 = 0;;


type myListofRules_nonterminals =

| Alpha | Beta | Delta | Gamma | Epsilon | Iota | Upsilon | Omega



let myListofRules =

[Alpha, [T"This"];

Alpha, [N Beta; N Gamma; N Epsilon];

Beta, [N Iota; N Upsilon; N Alpha];

Beta, [T"Check?"];

Delta, [N Beta; N Gamma; N Omega];

Gamma, [T"randomStuff"];

Epsilon, [T"*;)*"];

Iota, [T"Data"];

Upsilon, [T"moaaarStuff"];

Alpha, [N Iota];

Alpha, [T"moaarrstuff2.."];

Omega, [T"lastone"]]



let reachableTest =

filter_reachable (Alpha, myListofRules) =

(Alpha,

 [Alpha, [T "This"];

 Alpha, [N Beta; N Gamma; N Epsilon];

 Beta, [N Iota; N Upsilon; N Alpha];

 Beta, [T "Check?"];

 Gamma, [T "randomStuff"];

 Epsilon, [T "*;)*"];

 Iota, [T "Data"];

 Upsilon, [T "moaaarStuff"];

 Alpha, [N Iota];

 Alpha, [T "moaarrstuff2.."]])

;;             
