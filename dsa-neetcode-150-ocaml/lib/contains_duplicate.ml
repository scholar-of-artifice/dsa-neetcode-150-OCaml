open Core

module Sorting = struct
    let solution nums = 
        (* sort the list *)
        let nums_sorted = List.sort nums ~compare:Int.compare in 
        (* check adjacent elements *) 
        let rec check_adjacent_elements = function
            | [] -> false (* empty array *)
            | [_] -> false (* single element *)
            | x :: y  :: rest -> 
                if Int.equal x y then true
                else check_adjacent_elements ( y :: rest)
        in
        check_adjacent_elements nums_sorted
end

module Hashing = struct

    let solution nums = 
        (*use 'in' after local variable definitions*)
        (*start with an empty set*)
        let set_observed = Set.of_list (module Int) nums in

        (* compare the size of each data structure *)
        (* .cardinal is how you get the size of a set *)
        (* <> is the not equals operation *)
        List.length nums <> Set.length set_observed
end

(*--- Tests ---*)

let%expect_test "contains_duplicate -> sorting" =
    let result = Sorting.solution [1; 2; 3; 1] in
    Printf.printf "%b" result;
    [%expect {|true|}]
