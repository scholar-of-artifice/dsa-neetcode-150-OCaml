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

let%expect_test "contains_duplicate -> sorting -> true" =
    (*define a helper that runs one case at a time*)
    let check_case input = 
        let result = Sorting.solution input in
        print_s [%message "Testing" (input : int list) (result : bool)]
    in
    (*GIVEN these test cases*)
    let test_cases = [
        [1; 1];
        [1; 2; 1];
        [1; 2; 2];
        [1; 2; 3; 1];
        [1; 2; 3; 2];
        [1; 2; 3; 3];
    ] in

    (*WHEN the function is called*)
    List.iter test_cases ~f:check_case;
    
    (*THEN it returns true*)
    [%expect {|
      (Testing (input (1 1)) (result true))
      (Testing (input (1 2 1)) (result true))
      (Testing (input (1 2 2)) (result true))
      (Testing (input (1 2 3 1)) (result true))
      (Testing (input (1 2 3 2)) (result true))
      (Testing (input (1 2 3 3)) (result true))
      |}]


let%expect_test "contains_duplicate -> sorting -> false" =
    (*define a helper that runs one case at a time*)
    let check_case input = 
        let result = Sorting.solution input in
        print_s [%message "Testing" (input : int list) (result : bool)]
    in
    (*GIVEN these test cases*)
    let test_cases = [
        [1; 2];
        [1; 2; 3];
        [3; 2; 1];
        [1; 2; 3; 4];
        [1; 4; 3; 2];
        [1; 2; 4; 3];
    ] in

    (*WHEN the function is called*)
    List.iter test_cases ~f:check_case;
    
    (*THEN it returns false*)
    [%expect {|
      (Testing (input (1 2)) (result false))
      (Testing (input (1 2 3)) (result false))
      (Testing (input (3 2 1)) (result false))
      (Testing (input (1 2 3 4)) (result false))
      (Testing (input (1 4 3 2)) (result false))
      (Testing (input (1 2 4 3)) (result false))
      |}]
