open Core.Std
open Matrix

exception InvalidHex
exception InvalidPitch

(***** Type definitions *****)
type p = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
type pitch = p * int

type event = Tone of float * pitch * int | Stop of float * pitch

type obj = Note of pitch * float * int | Rest of float

let p_to_int p =
  match p with | C -> 0 | Db -> 1 | D -> 2 | Eb -> 3 | E -> 4 | F -> 5
    | Gb -> 6 | G -> 7 | Ab -> 8 | A -> 9 | Bb -> 10 | B -> 11

(*let int_to_t n =
  match t with | 0 -> |...*)

let int_to_p n =
  if (n < 0) || (n > 11) then raise InvalidPitch else
    let pitches = [C;Db;D;Eb;E;F;Gb;G;Ab;A;Bb;B] in
  List.nth_exn pitches n

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

(******* Music code ********)
let shift (by : float) (e : event) =
  match e with
    | Tone (time, pit, vol) -> Tone (time +. by, pit, vol)
    | Stop (time, pit) -> Stop (time +. by, pit)

let shift_start (by : float) (str : event stream) =
  let Cons (e, t) = str () in
    fun () -> Cons(shift by e, t)

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

(* emotions arrays accounting for both upper and lowercase *)

let emotions = ["angry"; "upbeat"]

let rec member (key : string) (lst : string list) : bool = 
    match lst with
    |[] -> false
    |hd::tl -> if key = hd then true else member key tl

let rec listprint (lst: string list) : string = 
    match lst with
    |[] -> ""
    |hd::tl -> hd ^  " " ^ (listprint tl)

let prompt : unit  = 
     let words = "Please pick an emotion from this list" ^ " " ^ listprint emotions ^ ":" in 
     let line =  print_string words in
     let input  = read_line line in
        if (member (String.lowercase input) emotions) then 
                    let filename = "gen" ^ input ^ ".mid" 
                    in output_midi filename  176 canon
        else 
            let () = print_string "Please try again : Not a valid emotion" in 
            let () = print_newline () in  
            print_newline ()
            
let rec convert_plist_to_intlist (notes : 'a list) (intlist : int list) : int list=
     match notes with
     |[] -> List.rev intlist
     |hd::tl-> convert_plist_to_intlist tl ((p_to_int hd)::intlist)          
            
let main_matrix = Mus_matrix.music_matrix

let increment_matrix_val (row:int) (col:int) : unit =
    let n = main_matrix.(row).(col) in
    main_matrix.(row).(col) <- n +. 1.

let rec note_counter (notes_list : int list) : float array array =
    match notes_list with
    | [] -> raise (Failure "No notes recieved.")
    | [hd] -> main_matrix
    | hd1::hd2::tl -> increment_matrix_val hd1 hd2; note_counter (hd2::tl)             

let next_note_helper (index : int) : float array = 
    let new_array = [|0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.|] in
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

let rec build_song (note : float array) (song : float array list) : float array list =
  let length = ref 50 in
  while !length > 0 do
  (* prob is a float array that contains the probabilities of the next note *)
  let prob = Mus_matrix.vector_mult (*probability matrix*) note in
  let new_note = next_note prob (sum_vector prob) in
  length := !length - 1; song @ new_note
  done 
 

(** float array list **)
let rec list_to_stream (lst : obj list) : event stream =
  let rec list_to_stream_rec (nlst : obj list) : event stream =
    match nlst with
    | [] -> list_to_stream lst
    | Note(p,d,v) :: tl ->
       fun () -> Cons(Tone(0.,p,v),
                      fun () -> Cons(Stop(d,p), list_to_stream_rec tl))
    | Rest(d) :: tl -> shift_start d (list_to_stream_rec tl)
  in list_to_stream_rec lst

(* standard volume *)
let v = 12

let rec find_one (vector : float array) : int = 
    let ans =  ref 0 in
        for i = 0 to 12 do 
            if vector.(i) = 1.0 then ans:= i else ()
        done; !ans

let rec list_to_stream (notes : float array list) (lengths : float array list) : event stream =
  let rec list_to_stream_rec (nlist : float array list) (llist : float array list) : event stream =
    match nlist, llist with
    | [], [] -> list_to_stream notes lengths
    | hd1 :: tl1, hd2 :: tl2 ->
       let p = find_one hd1 in
       let d = find_one hd2 in
       if p = 12
       then shift_start d (list_to_stream_rec tl1 tl2)
       else
       (fun () -> Cons(Tone(0., p, v),
		      fun () -> Cons(Stop(d, p), list_to_stream_rec tl1 tl2)))
       
       



																										    

(* take in current note (a float array)
generate next note (multiply note by matrix to generate probabilities, then use next_note)
add current note to list
substract length of note from thirty
then run the function on the next note
returns the list
prompts for user input *)


