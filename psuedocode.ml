Array.make_matrix: xint: int -> yint: int -> 'a -> 'a array array
	initialize every cell in this 12 X 12 matrix to 0

midi_converter: 
	takes a midi file : returns a list of ints that represent notes

note_counter: recursive function that counts up all the instances of a note occurring after another 
	Note_counter takes (a midi_converted list of ints) : returns an array array (matrix) = 
	match midi_list with 
	| empty -> raiseException
	| hd1::hd2::tl -> add hd1, hd2 pair to matrix as an occurrence in matrix by increasing counter by 1; note_counter (hd::tl) 

sum helper function:
	sum_helper takes (array array) : returns int = 

prob_matrix: function that calculates probabilities of the matrix created in note_counter
	prob_matrix takes (note_counter output of array array matrix) : returns array array matrix = 
 	Divide every entry in the note_counter matrix by (sum_helper note_counter matrix) to create new matrix with correct probabilities



