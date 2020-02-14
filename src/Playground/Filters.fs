module Filters

let applyFilters fs i =
  let rec apply = function
    | f :: t -> if f i then apply t else false
    | [] -> true
  apply fs

let isDivBy d i = (i % d) = 0
let isDivs = List.map isDivBy [ 3; 5 ]

let maybeDivBy d i = if (i % d) = 0 then Some i else None
let maybeDivs =
    [ 3; 5 ]
    |> List.map maybeDivBy
    |> List.reduce (fun f1 f2 -> f1 >> Option.bind f2)

let emptyBind f a =
    match a with
    | [e] -> f e
    | _ -> []

let eleDivBy d i = if (i % d) = 0 then [i] else []
let eleDivs =
    [ 3; 5 ]
    |> List.map eleDivBy
    |> List.reduce (fun f1 f2 -> f1 >> emptyBind f2)

// fastest, ~800ms for 10M integers
let testBoolFilter l =
    List.filter (applyFilters isDivs) l

// twice as slow as BoolFilter (~1.6s)
let testMaybeCollect l =
    l |> List.map maybeDivs |> List.collect Option.toList

// only marginally worse than BoolFilter (~900ms)
let testMaybeChoose l = List.choose maybeDivs l

// very close to MaybeChoose
let testEleCollect l = List.collect eleDivs l

// Looks like with the composed Option filters -> List.choose
// is the winner. Only a little slower than the bool filer with
// no recursion. I think that it is properly tail recurrsing though,
// so should be fine as is? Less GC pressure at the least.
