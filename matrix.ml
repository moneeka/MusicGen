open Core.Std


module type MUS_MATRIX = 
    sig
    val music_matrix : float array array
    val sum_row : float array array -> int -> float
    val get_elt : float array array -> int -> int -> float
    val change_values_based_on_divide : float array array -> int -> float -> float array 
    end


module Mus_matrix : MUS_MATRIX =
   struct
    let music_matrix = Array.make_matrix 12 12 0.0
    
    let sum_row arr row =
     if row < 0 || row > 12 then
        raise (Failure "row out of bounds")
     else 
        let returnrow = arr.(row) in
         let sum = ref 0.0 in
         for i = 0 to Array.length returnrow - 1 do
            sum := !sum +. returnrow.(i)
        done; !sum
        
     let get_elt mat row col = 
        mat.(row).(col)
        
     let change_values_based_on_divide mat row value = 
        let sum = sum_row mat row in
            let r = mat.(row) in
            Array.map (fun _ -> value /. sum) r
     
    end 
    
