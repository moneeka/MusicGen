open Core.Std
open matrix

let main_matrix = Mus_matrix.music_matrix

let increment_matrix_val (row:float) (col:float) : unit =
    let n = main_matrix.(row).(col) in
    main_matrix.(row).(col) <- n +. 1.

let rec note_counter (notes_list : int list) : float array array =
    match notes_list with
    | empty -> raise (Failure "No notes recieved.")
    | hd1::hd2::tl -> increment_matrix_val hd1 hd2; note_counter (hd2::tl) 




