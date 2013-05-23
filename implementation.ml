(* Implementation simulating parallelism sequentially. ************************)

open Kahn
open Miscellaneous
open Unix
open List
exception Stop of (unit -> unit) 


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
    raise (Stop (fun () -> f ()))
  
  let rec get c f = 
    trace "get";
    try
      let v = Queue.pop c in
      raise (Stop (fun () -> f v))
       
    with
    Queue.Empty ->
      raise (Stop (fun () -> get c f))


  let rec doco l = 
    trace "doco";
    match l with
    | [] -> failwith "doco on an empty list"
    | [f] -> f
    | a::q -> 
                 (fun s -> 
		       
		       try 
			  a (fun () -> ());
				    (doco q) s
		      with
			Stop (g)->	    (doco (q@[fun f -> g ()])) s
               
		)
  let return v (f:('a->unit))=
    trace "return";
    f v
  
  
  let bind (e:'a process) (e':('a -> 'b process)) =
    trace "bind";
    (fun f -> e (fun v -> let p = e' v in p f))

  
  let run e =
    trace "run";
    let l = ref([]) in
    let fill v = l:= [REPONSE(v)] in
    let rec loop_until_success g =
      try 
        g ()
      with Stop f -> loop_until_success f
      in
      loop_until_success (fun () -> e fill);
       match (hd(!l)) with
    |REPONSE (v) ->  v
end