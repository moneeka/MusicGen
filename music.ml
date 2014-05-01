open Core.Std
open Matrix

exception InvalidHex
exception InvalidPitch

(** Type definitions and Constants **)
(* emotions arrays accounting for both upper and lowercase *)
let emotions = ["sad"; "upbeat"]

(* standard volume *)
let v = 12

(* standard octave *)
let o = 4

type p = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab | REST
type pitch = p * int

type event = Tone of float * pitch * int | Stop of float * pitch

type obj = Note of pitch * float * int | Rest of float

type song = {emotion : string;
	     pitches : p list;
	     lengths : float list }

(** SONGS **)
let gymnopedie = { emotion = "sad"; 
pitches = [A; G; F; E; D; E; F; E; D; C; E; G; G; D; D; REST; C; F; G; A; D; E; F; B; A; G; A; D; E; F; G; F; E; D; E; D; C; B; C; B; A; A; G; F; E; D; E; F; E; D; C; E; G; G; C; B; A; B; C; D; E; G; A; D; E; F; B; A; G; A; D; E; F; G; F; E; D; E; D; C; B; C; B; A];
lengths = [0.75; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75; 0.5; 0.25; 1.5; 0.75; 0.25; 0.25; 0.25; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75; 1.5; 0.75; 0.25; 0.25; 0.25; 0.25; 0.25; 1.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75; 0.75; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75; 0.5; 0.25; 0.25; 0.25; 0.25; 0.5; 0.25; 0.75; 0.75; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75; 1.5; 0.75; 0.25; 0.25; 0.25; 0.25; 0.25; 1.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.75] }

let swan_lake = { emotion = "sad";
pitches = [E; A; B; C; D; E; C; E; C; E; A; C; A; E; C; A; REST; D; C; B; E; A; B; C; D; E; C; E; C; E; A; C; A; E; C; A; REST; A; B; C; D; E; F; G; F; E; F; G; A; G; F; G; A; B; A; E; C; B; A; 
B; C; D; E; F; G; F; E; F; G; A; G; F; G; A; Bb; F; D; F; Bb; B; Gb; B; E];
lengths = [0.5; 0.125; 0.125; 0.125; 0.125; 0.375; 0.125; 0.375; 0.125; 0.375; 0.125; 0.125; 0.125; 0.125; 0.125; 0.5; 0.125; 0.125; 0.125; 0.125; 0.5; 0.125; 0.125; 0.125; 0.125; 0.375; 0.125; 0.375; 0.125; 0.375; 0.125; 0.125; 0.125; 0.125; 0.125; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.125; 0.125; 0.375; 0.125; 0.25; 0.125; 0.125; 0.375; 0.125; 0.25; 0.125; 0.125; 0.375; 0.125; 0.125; 0.125; 0.125; 0.125; 0.25; 0.25; 0.25; 0.125; 0.125; 0.375; 0.125; 0.25; 0.125; 0.125; 0.375; 0.125; 0.25; 0.125; 0.125; 0.375; 0.125; 0.25; 0.125; 0.125; 0.375; 0.125; 0.375; 0.125] }

let fur_elise = { emotion = "sad";
pitches = [E; Eb; E; Eb; E; B; D; C; A; REST; C; E; A; B; REST; E; Ab; B; C; REST; E; E; Eb; E; Eb; E; B; D; C; A; REST; C; E; A; B; REST; E; C; B; A; REST; B; C; D; E; G; F; E; D; F; E; D; C; E; D; C; B; REST; E; E; REST; E; E; REST; Eb; E; REST; Eb; E; Eb; E; Eb; E; B; D; C; A; REST; C; E; A; B; REST; E; Ab; B; C; REST; E; E; Eb; E; Eb; E; B; D; C; A; REST; C; E; A; B; REST; E; C; B; A];
lengths = [0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.375; 0.125; 0.125; 0.125; 0.375; 0.125; 0.125; 0.125; 0.375; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.25; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.25; 0.125; 0.125; 0.125; 0.125; 0.50] }
		
let ode_to_joy = { emotion = "upbeat";
pitches = [E; E; F; G; G; F ; E; D; C; C; D; E; E; D; D; E; E; F; G; G; F; E; D; C; C; D; E; D; C; C; D; D; E; C; D; E; F; E; C; D; E; F; E; D; C; D; REST; E; E; F; G; G; F; E; D; C; C; D; E; D; C; C]; 
lengths = [0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.375; 0.125; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.375; 0.125; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.125; 0.125; 0.25; 0.25; 0.25; 0.125; 0.125; 0.25; 0.25; 0.25; 0.25; 0.25; 0.5; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.25; 0.375; 0.125; 0.5] }
 
let sonata = { emotion = "upbeat";
pitches = [C; E; G; B; C; D; C; REST; A; G; C; G; F; E; F; E; REST];
lengths = [0.5; 0.25; 0.25; 0.375; 0.0625; 0.0625; 0.25; 0.25; 0.5; 0.25; 0.25; 0.25; 0.125; 0.0625; 0.0625; 0.25] }

let entertainer = { emotion = "upbeat";
pitches = [D; Eb; E; C; E; C; E; C; REST; C; D; Eb; E; C; D; E; B; D; C; REST; D; Eb; E; C; E; C; E; C; REST; C; D; Eb; E; C; D; E; B; D; C; REST; A; G; Gb; A; C; E; D; C; A; D];
lengths = [0.0625; 0.0625; 0.0625; 0.125; 0.0625; 0.125; 0.0625; 0.3125; 0.0625; 0.0625; 0.0625; 0.0625; 0.0625; 0.0625; 0.0625; 0.125; 0.0625; 0.125; 0.25; 0.0625; 0.0625; 0.0625; 0.0625; 0.125; 0.0625; 0.125; 0.0625; 0.3125; 0.0625; 0.0625; 0.0625; 0.0625; 0.0625; 0.0625; 0.125; 0.0625; 0.0625; 0.0625; 0.25] }

(* list containing the songs *)
let songs = [gymnopedie; swan_lake; fur_elise; ode_to_joy; sonata; entertainer]

(***** Streams Code *****)
type 'a stream = unit -> 'a str
and 'a str = Cons of 'a * 'a stream ;;

let head (s: 'a stream) : 'a =
  let Cons(v, _) = s () in v
;;

let tail (s: 'a stream) : 'a stream =
  let Cons(_, s') = s () in s'
;;

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  fun () -> Cons(f (head s), map f (tail s))
;;

(******* Music code ********)
let shift (by : float) (e : event) =
  match e with
    | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
    | Stop (time, pit) -> Stop (time +. by, pit)

let shift_start (by : float) (str : event stream) =
  let Cons (e, t) = str () in
    fun () -> Cons(shift by e, t)

let p_to_int p =
  match p with | C -> 0 | Db -> 1 | D -> 2 | Eb -> 3 | E -> 4 | F -> 5
    | Gb -> 6 | G -> 7 | Ab -> 8 | A -> 9 | Bb -> 10 | B -> 11 | REST -> 12

let length_to_int n =
  match n with | 0.0625 -> 0 | 0.125 -> 1 | 0.25 -> 2 | 0.3125 -> 3 
    | 0.375 -> 4 | 0.5 -> 5 | 0.625 -> 6 | 0.75 -> 7 | 0.875 -> 8 
    | 1.0 -> 9 | 1.25 -> 10 | 1.5 -> 11 | 1.75 -> 12 

let int_to_p n =
  if (n < 0) || (n > 12) then raise InvalidPitch else
    let pitches = [C;Db;D;Eb;E;F;Gb;G;Ab;A;Bb;B;REST] in
  List.nth_exn pitches n

let int_to_length n =
  if (n < 0) || (n > 12) then raise InvalidPitch else
    let lengths = [0.0625;0.125;0.25;0.3125;0.375;0.5;0.625;0.75;0.875;1.0;1.25;1.5;1.75] in
  List.nth_exn lengths n

(***** MIDI Output Code *****)
let hex_to_int hex = int_of_string ("0x"^hex)

let int_to_hex n = Printf.sprintf "%02x" n

let rec output_hex outchan hex =
  let len = String.length hex in
  if (len = 0) then ()
  else (if (len < 2) then raise InvalidHex
  else (output_byte outchan
    (hex_to_int (String.sub hex ~pos:0 ~len:2)));
        (output_hex outchan (String.sub hex ~pos:2 ~len:(len - 2))))

let ticks_per_q = 32

let header = "4D546864000000060001000100"^(int_to_hex ticks_per_q)^"4D54726B"
let footer = "00FF2F00"

let pitch_to_hex pitch =
  let (p, oct) = pitch in int_to_hex ((oct+1)*12+(p_to_int p))

let time_to_hex time =
  let measure = ticks_per_q * 4 in
  let itime = Float.to_int (time *. (float measure)) in
  if itime < measure then (int_to_hex itime)
  else "8"^(string_of_int (itime / measure))^
    (Printf.sprintf "%02x" (itime mod measure))

let rec insts playing pitch =
  match playing with
    | [] -> (0, [])
    | (pitch2, n)::t -> if pitch2 = pitch then (n, playing) else
        let (n2, p2) = insts t pitch in (n2, (pitch2, n)::p2)

let shift (by : float) (e : event) =
  match e with
    | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
    | Stop (time, pit) -> Stop (time +. by, pit)

let shift_start (by : float) (str : event stream) =
  let Cons (e, t) = str () in
    fun () -> Cons(shift by e, t)

let stream_to_hex (n : int) (str : event stream) =
  let rec sthr n str playing =
  if n = 0 then "" else
  match str () with
    | Cons(Tone (t, pitch, vol), tl) ->
        let (i, np) = insts playing pitch in
          (time_to_hex t)^"90"^(pitch_to_hex pitch)^(int_to_hex vol)^
            (sthr (n-1) tl ((pitch, i+1)::np))
    | Cons(Stop (t, pitch), tl) ->
        let (i, np) = insts playing pitch in
          if i>1 then sthr (n-1) (shift_start t tl) ((pitch, i-1)::np)
          else (time_to_hex t)^(pitch_to_hex pitch)^"00"^
          (sthr (n-1) tl ((pitch, i-1)::np))
  in sthr n str []

let output_midi filename n str =
  let hex = stream_to_hex n str in
  let outchan = open_out_bin filename in
  output_hex outchan header;
  output_binary_int outchan ((String.length hex) / 2 + 4);
  output_hex outchan hex;
  output_hex outchan footer;
  flush outchan;
  Out_channel.close outchan

(** User Input **)
(* returns true if key is an element in a string list *)
let rec member (key : string) (lst : string list) : bool = 
    match lst with
    |[] -> false
    |hd::tl -> if key = hd then true else member key tl

(* prints a string list as one string *)
let rec listprint (lst: string list) : string = 
    match lst with
    |[] -> ""
    |hd::tl -> hd ^  " " ^ (listprint tl)

let userinput = ref ""
(* prompts for user input *)
(** instead of returning unit... maybe store the input and return it as a string so it can be used later? **)
let prompt () : string  = 
     let words = "Please pick an emotion from this list" ^ " " ^ listprint emotions ^ ":" in 
     let line =  print_string words in
     let input = read_line line in
        if (member (String.lowercase input) emotions) then 
        print_string "Your song is being generated...!"; input;;            
        (*else 
            let () = print_string "Please try again : Not a valid emotion" in 
            let () = print_newline () in  
            print_newline ();;*)

(** Generates Probability Matrices **)
(* converts p list to int list *)            
let rec convert_plist_to_intlist (notes : p list) (intlist : int list) : int list =
     match notes with
     |[] -> List.rev intlist
     |hd::tl-> convert_plist_to_intlist tl ((p_to_int hd)::intlist)      

(* converts length (float) list to int list *)
let rec convert_lengthlist_to_intlist (lengths : float list) (intlist : int list) : int list =
     match lengths with
     |[] -> List.rev intlist
     |hd::tl-> convert_lengthlist_to_intlist tl ((length_to_int hd)::intlist)          

(* initialize 13x13 notes matrix *)            
let notes_matrix = Mus_matrix.music_matrix

(* initialize 13x13 lengths matrix *)
let lengths_matrix = Mus_matrix.music_matrix

(* given row and column numbers, will increment that element in the matrix *)
let increment_matrix_val (matrix : float array array) (row : int) (col : int) : unit =
    let n = matrix.(row).(col) in
    matrix.(row).(col) <- n +. 1.

(* reads int lists and increments matrix, creating probability matrix *)
let rec note_counter (vals_list : int list) (matrix : float array array) : float array array =
    match vals_list with
    | [] -> raise (Failure "No values recieved.")
    | [hd] -> matrix
    | hd1::hd2::tl -> increment_matrix_val matrix hd1 hd2; note_counter (hd2::tl) matrix

(* creates probability matrix for notes *)
let rec notes_probability (input : string) (song_list : song list) : float array array =
  let rec notes_list (input : string) (song_list : song list) : int list =
    (match song_list with
    | [] -> []
    | hd :: tl ->
       (if hd.emotion = input
	then (convert_plist_to_intlist hd.pitches []) @ (notes_list input tl)
	else notes_list input tl))
  in note_counter (notes_list input song_list) notes_matrix 

(* creates probability matrix for lengths *)
let rec lengths_probability (input : string) (song_list : song list) : float array array =
  let rec lengths_list (input : string) (song_list : song list) : int list =
    (match song_list with
     | [] -> []
     | hd :: tl ->
	(if hd.emotion = input
	 then (convert_lengthlist_to_intlist hd.lengths []) @ (lengths_list input tl)
	 else lengths_list input tl))
  in note_counter (lengths_list input song_list) lengths_matrix

let next_note_helper (index : int) : float array = 
    let new_array = [|0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.|] in
        new_array.(index) <- 1.; new_array

(*This function takes in the current probability vector (which has already been multiplied with the probability matrix) and the sum of the row to return the next probability vector. We will generate a random integer from 0 to the number of instances in the row (aka sum of row) and then use that number to determine the next vector.  *)
let next_note (current_prob : float array) (sum_of_rows : float) : float array = 
  let rand = Random.int (Float.to_int sum_of_rows) in
  let prob = (Int.to_float rand) in 
    let ans = ref 0 in 
    let sum = ref 0.0 in
    let index = ref 0 in 
        while !sum < prob do 
        sum:= !sum +. current_prob.(!index);
        ans:= !index;
        index:= !index + 1
        done ; next_note_helper !ans

let rec build_notes (n : int) (initial_note : float array) (song : float array list) : float array list =
  let probability_matrix = notes_probability "upbeat" songs in
  match n with
  | 0 -> List.rev song
  | x ->
     (let prob = Mus_matrix.vector_mult probability_matrix initial_note in
      let new_note = next_note prob (Mus_matrix.sum_vector prob) in
      build_notes (n - 1) new_note (initial_note :: song))
(*
let rec build_notes (initial_note : float array) (song : float array list) : float array list =
  let length = ref 50 in
  let probability_matrix = notes_probability "upbeat" songs in
  while !length > 0 do
  (* prob is a float array that contains the probabilities of the next note *)
  let prob = Mus_matrix.vector_mult probability_matrix initial_note in
  let new_note = next_note prob (Mus_matrix.sum_vector prob) in
  length := (!length - 1); List.rev (new_note :: song)
  done 
 *) 
let rec build_lengths (n : int) (initial_length : float array) (song : float array list) : float array list =
  let probability_matrix = lengths_probability "upbeat" songs in
  match n with
  | 0 -> List.rev song
  | x ->
     (let prob = Mus_matrix.vector_mult probability_matrix initial_length in
      let new_length = next_note prob (Mus_matrix.sum_vector prob) in
      build_lengths (n - 1) new_length (initial_length :: song))

(** converts float array list of notes to an event stream **)
let rec find_one (vector : float array) : int = 
    let ans =  ref 0 in
        for i = 0 to 12 do 
            if vector.(i) = 1.0 then ans:= i else ()
        done; !ans

let rec list_to_stream (notes : float array list) (lengths : float array list) : event stream =
  let rec list_to_stream_rec (nlist : float array list) (llist : float array list) : event stream =
    (match nlist, llist with
    | [], [] -> list_to_stream notes lengths
    | hd1 :: tl1, hd2 :: tl2 ->
       let p = int_to_p (find_one hd1) in
       let d = int_to_length (find_one hd2) in
       if p = REST
       then shift_start d (list_to_stream_rec tl1 tl2)
       else
       (fun () -> Cons(Tone(0., (p, o), v),
		      fun () -> Cons(Stop(d, (p, o)), list_to_stream_rec tl1 tl2))))
  in list_to_stream_rec notes lengths
		
(* arbitrary initial note picker and length picker.. can use for both notes and lengths! *)	       
let initial_note () : float array = 
  let rand = Random.int 12 in
  next_note_helper rand

let output : unit =
 let stream =
  (let notes = build_notes 50 (initial_note ()) [] in
  let lengths = build_lengths 50 (initial_note ()) [] in
  list_to_stream notes lengths) in  
 let filename = "gen" ^ "fdajfdklasj" ^ ".mid" in 
 output_midi filename 176 stream

(*
(*>* Problem 3.1 *>*)
(* Write a function list_to_stream that builds a music stream from a finite
 * list of musical objects. The stream should repeat this music forever.
 * Hint: Use a recursive helper function as defined, which will change the
 * list but keep the original list around as lst. Both need to be recursive,
 * since you will call both the inner and outer functions at some point. *)  

let rec list_to_stream (lst : obj list) : event stream =
  let rec list_to_stream_rec nlst =
    match nlst with
    |[] -> list_to_stream lst
    |hd::tl -> match hd with
               |Note(p,f,i) -> fun ()-> Cons(Tone(f,p,i),
                               fun ()->(Cons(Stop(f,p),(list_to_stream_rec tl))))
               |Rest(f) -> shift_start f (list_to_stream_rec tl) in
               
  list_to_stream_rec lst

(* You might find this small helper function, well... helpful. *)
let time_of_event (e : event) : float =
  match e with
    | Tone (time, _, _) -> time
    | Stop (time, _) -> time

(*>* Problem 3.2 *>*)
(* Write a function pair that merges two event streams. Events that happen
 * earlier in time should appear earlier in the merged stream. *)
let rec pair (a : event stream) (b : event stream) : event stream =
  let head1 = head a in
  let head2 = head b in 
  
  let time1 = time_of_event head1 in
  let time2 = time_of_event head2 in
 
  if time1 < time2 then
    fun () -> Cons(head2, pair (shift_start (-1. *. time1) a) (tail b))
  else
    fun () -> Cons(head1, pair (tail a) (shift_start (-1. *. time2) b))

(*>* Problem 3.3 *>*)
(* Write a function transpose that takes an event stream and moves each pitch
 * up by half_steps pitches. Note that half_steps can be negative, but
 * this case is particularly difficult to reason about so we've implemented
 * it for you. *)

let transpose_pitch (p, oct) half_steps =
  let newp = (p_to_int p) + half_steps in
    if newp < 0 then
      if (newp mod 12) = 0 then (C, oct + (newp / 12))
      else (int_to_p ((newp mod 12) + 12), oct - 1 + (newp / 12))
    else (int_to_p (newp mod 12), oct + (newp / 12))

let transpose (str : event stream) (half_steps : int) : event stream =
    let func = fun elem -> match elem with
                        |Tone(f,p,i) -> Tone(f,transpose_pitch p half_steps, i)
                        |Stop(f,p) -> Stop(f, transpose_pitch p half_steps)
    in
    map func str

(* Some functions for convenience. *)
let quarter pt = Note(pt,0.25,60);;

let eighth pt = Note(pt,0.125,60);;

(* Now look what we can do. Uncomment these lines when you're done implementing
 * the functions above. *)
(* Start off with some scales. We've done these for you.*)

let scale1 = list_to_stream (List.map ~f:quarter [(C,3);(D,3);(E,3);(F,3);(G,3);
                                            (A,3);(B,3);(C,4)]);;

let scale2 = transpose scale1 7;;

let scales = pair scale1 scale2;;

output_midi "scale.mid" 32 scales;;

(*>* Problem 3.4 *>*)
(* Then with just three lists ... *)

let bass = list_to_stream (List.map ~f:quarter [(D,3);(A,2);(B,2);(Gb,2);(G,2);
                                             (D,2);(G,2);(A,2)]);;

let slow = [(Gb,4);(E,4);(D,4);(Db,4);(B,3);(A,3);(B,3);(Db,4);(D,4);
            (Db,4);(B,3);(A,3);(G,3);(Gb,3);(G,3);(E,3)];;
let fast = [(D,3);(Gb,3);(A,3);(G,3);(Gb,3);(D,3);(Gb,3);(E,3);(D,3);(B,2);
            (D,3);(A,3);(G,3);(B,3);(A,3);(G,3)];;

let melody = list_to_stream ((List.map ~f:quarter slow) @
                (List.map ~f:eighth fast));;


(* ...and the functions we defined, produce (a small part of) a great piece of
 * music. The piece should be four streams merged: one should be the bass
 * playing continuously from the beginning. The other three should be the
 * melody, starting 2, 4 and 6 measures from the beginning, respectively. *)

(* Define a stream for this piece here using the above component streams
 * bass and melody. Uncomment the definitions above and the lines below when
 * you're done. Run the program to hear the beautiful music. *)
   




let canon = (pair bass (shift_start 2.0 (pair (shift_start 4.0 melody)  (pair melody (shift_start 2.0 melody)))));;


(* Some other musical parts for you to play with. *)

let part1 = list_to_stream [Rest 0.5; Note((D,4),0.75,60); Note((E,4),0.375,60);
                            Note((D,4),0.125,60); Note((B,3),0.25,60);
                            Note((Gb,3),0.1875,60); Note((G,3),0.0625,60)];;

let part2 = list_to_stream [Note((G,3),0.1875,60); Note((A,3),0.0625,60);
                            Note((B,3),0.375,60); Note((A,3),0.1875,60);
                            Note((B,3),0.0625,60); Note((C,4),0.5,60);
                            Note((B,3),0.5, 60)];;

let part3 = list_to_stream [Note((G,3),1.,60); Note((G,3),0.5,60);
                            Note((E,3),0.1875,60);
                            Note((Gb,3),0.0625,60); Note((G,3),0.25, 60);
                            Note((E,3),0.25,60)];;

let part4 = list_to_stream [Rest(0.25); Note((G,3),0.25,60);
                            Note((Gb,3),0.25,60); Note((E,3),0.375,60);
                            Note((D,3),0.125,60); Note((C,3),0.125,60);
                            Note((B,2),0.125,60); Note((A,2),0.25,60);
                            Note((E,3),0.375,60); Note((D,3),0.125,60)];;


 *)            


