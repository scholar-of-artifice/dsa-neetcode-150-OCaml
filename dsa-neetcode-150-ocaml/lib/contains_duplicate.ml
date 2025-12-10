
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