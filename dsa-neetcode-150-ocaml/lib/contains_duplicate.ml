
module Sorting = struct
    let solution nums = 
        (* sort the list *)
        let nums_sorted = List.sort compare nums in 
        (* check adjacent elements *) 
        let rec check_adjacent_elements = function
            | [] -> false (* empty array *)
            | [_] -> false (* single element *)
            | x :: y  :: rest -> 
                if x = y then true else check_adjacent_elements ( y :: rest)
        in
        check_adjacent_elements nums_sorted
end

module Hashing = struct
    module IntSet = Set.Make(Int)

    let solution nums = 
        (*use 'in' after local variable definitions*)
        (*start with an empty set*)
        let set_observed = IntSet.of_list nums in

        (* compare the size of each data structure *)
        (* .cardinal is how you get the size of a set *)
        (* <> is the not equals operation *)
        List.length nums <> IntSet.cardinal set_observed
end

