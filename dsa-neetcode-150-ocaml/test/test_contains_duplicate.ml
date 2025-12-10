
open Dsa_neetcode_150_ocaml

let check_solution name solve_func = 
    print_endline("Testing " ^ name ^ "...");
    assert ( solve_func [] = false);
    assert ( solve_func [   1   ] = false);
    assert ( solve_func [   1; 2; 3; 4  ] = false);
    assert ( solve_func [   1; 1; 3; 4  ] = true);
    assert ( solve_func [   1; 2; 3; 1  ] = true);
    assert ( solve_func [   0; 0; 0; 0; 0   ] = true);
    assert ( solve_func [   1; 2; 3; 4; 5   ] = false);
    assert ( solve_func [   1; 2; 3; 4; 5;
                            1; 2; 3; 4; 5;
                            1; 2; 3; 4; 5;
                            1; 2; 3; 4; 5;
                            1; 2; 3; 4; 5;
                            1; 2; 3; 4; 5;  ] = true);
    assert ( solve_func [   1; 2; 3; 4; 5;
                            11; 12; 13; 14; 15;
                            21; 22; 23; 24; 25;
                            31; 32; 33; 34; 35;
                            41; 42; 43; 44; 45;
                            51; 52; 53; 54; 55;  ] = false);
    print_endline "[OK] Passed"
    
let run() = 
    print_endline "--- Contains Duplicate ---";
    check_solution "Sorting" Contains_duplicate.Sorting.solution