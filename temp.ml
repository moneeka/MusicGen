		
(* arbitrary initial note picker and length picker.. can use for both notes and lengths! *)	       
let initial_note () : float array = 
  let rand = Random.int 12 in
  next_note_helper rand
