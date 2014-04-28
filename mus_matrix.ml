module type MUS_MATRIX =
sig
    val mus_matrix : 'a array array
    val sum_row : array -> int -> float
    val print_matrix : 'a array array
    val change_values_based_on_divide : float -> 'a array
end
