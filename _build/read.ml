open Core.Std
open Matrix

let main_matrix = Mus_matrix.music_matrix

let increment_matrix_val (row:int) (col:int) : unit =
    let n = main_matrix.(row).(col) in
    main_matrix.(row).(col) <- n +. 1.

let rec note_counter (notes_list : int list) : float array array =
    match notes_list with
    | [] -> raise (Failure "No notes recieved.")
    | [hd] -> main_matrix
    | hd1::hd2::tl -> increment_matrix_val hd1 hd2; note_counter (hd2::tl) 




