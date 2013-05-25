(* Implementation simulating parallelism sequentially. ************************)

open Kahn
open Miscellaneous
open Unix
open List
exception Stop


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
  
  let rec get c f = (* problème si get est bloquant ? *)
    trace "get";
    try
      let v = Queue.pop c in
      f v
       
    with
    Queue.Empty ->
      raise Stop


  let rec doco l = (* doco très imparfait *)
    trace "doco";
    match l with
    | [] -> failwith "doco on an empty list"
    | [f] -> f
    | a::q -> 
                 (fun s -> 
		       let l = ref([]) in
		       let doco_fill v = l:= [REPONSE (v)] in
		       try 
			  a doco_fill;
			  match (hd(!l)) with
				    |REPONSE (v) -> s v;
				    (doco q) s
		      with
			Stop ->	    (doco (q@[a])) s
               
		)
  let return v (f:('a->unit))=
    trace "return";
    f v
  
  
  let bind (e:'a process) (e':('a -> 'b process)) =
    trace "bind";
    let rec next = ref(fun f -> let l = ref([])in 
		let fill v = l:= [REPONSE (v)] in trace "a";
		e fill; trace "b";
		match (hd(!l)) with
		|REPONSE (v) -> next:=e' v;raise Stop) in
    (fun f -> !next f)
    
    
    
    (*let first = ref (true) and next = ref(fun f -> assert false) in
    (fun f -> if (!first)
	    then
	    let l = ref([])in 
		let fill v = l:= [REPONSE (v)] in trace "a";
		e fill; trace "b";first:=false;
		match (hd(!l)) with
		|REPONSE (v) -> next:=e' v;raise Stop;
	   else !next f)
    *)
    
    (*let rec fy = ref (fun () ->
    
    let rec calc_e = ref (fun f ->
		let l = ref([])in 
		let fill v = l:= [REPONSE (v)] ;f v in trace "a";
		e fill; trace "b";
		match (hd(!l)) with
		|REPONSE (v) -> fy:=(fun () -> trace "i";e' v);raise Stop;e' v f)  in
    (fun f -> let p = (!calc_e) in p f; calc_e:= p)
    ) in (fun f -> let p = (!fy) () in p (fun arg -> fy:= (fun () -> trace "g";p);f arg) ) 
     *)
  
  let run e =
    trace "run";
    let l = ref([]) in
    let fill v = l:= [REPONSE(v)] in
    let rec loop_until_success () =
      try 
        e fill
      with Stop -> trace "s";loop_until_success()
      in
      loop_until_success();
       match (hd(!l)) with
    |REPONSE (v) ->  v
end
