(* Implementation simulating parallelism sequentially. ************************)

open Kahn
open Miscellaneous
open Unix
open List



(* Main ***********************************************************************)

module S : S =
struct
  type 'a process = ('a -> unit) -> unit 
  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t
  
  
  type 'a reponse = REPONSE of 'a 
  let () = Random.self_init () 
  
  let new_channel () =
    trace "new_channel";
    let q = Queue.create () in
      q, q
  
  let put v c f =
    trace "put";
    Queue.push v c;
    f ()
  
  let send_result_to_f_and_do_some_work_at_the_same (proc:'a process) (f:'a->'b) =
    let l = ref([]) in
    let fill_and_continue (v:'a) = l:= [REPONSE(v)] in
    proc fill_and_continue;
    match (hd(!l)) with
    |REPONSE (v) -> f v
    
(* missing part: do some work *)    
     
  
  
  let rec get c (f:('a->unit)) = 
    trace "get";
    try
      let v = Queue.pop c in
      f v       
    with
    Queue.Empty ->
    send_result_to_f_and_do_some_work_at_the_same (get c) f 

  let rec doco (l:(unit process list)) (g:(unit -> unit)) = (* doco très imparfait *)
    trace "doco";
    match l with
    | [] -> failwith "doco on an empty list"
    | [f] -> f g
    | a::q -> a (fun f -> (doco q) g )        



  let return v (f:('a->unit))=
    trace "return";
    f v
  
  let run (e:'a process)  =
    trace "run";
    let l = ref([]) in
    let fill (v:'a) = l:= [REPONSE(v)] in   
        e ((fun v -> fill v) );
       match (hd(!l)) with
    |REPONSE (v) -> v

  
  let bind (e:'a process) (e':('a -> 'b process)) =
    trace "bind";
    (* e' (run e) f     bon typage mais marche pas *)
    (fun f -> send_result_to_f_and_do_some_work_at_the_same e e' f) 
    
    
  
  
end
