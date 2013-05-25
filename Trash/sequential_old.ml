(* Implementation simulating parallelism sequentially. ************************)

open Unix
open Kahn
open List
exception Stop 
(* Constant *******************************************************************)

let trace_enable = false


(* Auxilliar function *********************************************************)

let trace =
  let i = ref 0 in
  fun s ->
    if (trace_enable)
    then
      begin
      incr i;
      Format.printf "// l: %d  pid: %d  f: %s@." !i (getpid ()) s
      end
    else ()
    

(* Main ***********************************************************************)

module S : S =
struct
  type 'a answer = SUCCESS | FAILURE
  type 'a process = ('a -> unit) -> 'a answer
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
    f ();
    SUCCESS
  
  let rec get c f = (* problème si get est bloquant ? *)
    trace "get";
    try
      let v = Queue.pop c in
      f v;
      SUCCESS 
    with
    Queue.Empty ->
      FAILURE


  let rec doco l = (* doco très imparfait *)
    trace "doco";
    match l with
    | [] -> failwith "doco on an empty list"
    | [f] -> f
    | a::q -> 
                 (fun s -> 
		       let l = ref([]) in
		       let doco_fill v = l:= [REPONSE (v)] in
		       match (a doco_fill) with
		       | SUCCESS -> (match (hd(!l)) with
				    |REPONSE (v) -> s v;
				    (doco q) s)
		       | FAILURE -> (doco (q@[a])) s
               
		)
  let return v (f:('a->unit))=
    trace "return";
    f v;
    SUCCESS
  
  
  let bind (e:'a process) (e':('a -> 'b process)) =
    trace "bind";
    let rec calc_e = ref (fun () ->
		let l = ref([])in
		let fill v = l:= [REPONSE (v)] in
		let rec loop_until_success () =
		match (e fill) with
		|SUCCESS -> ()
		|FAILURE -> loop_until_success() 
		in
		loop_until_success(); 
		match (hd(!l)) with
		|REPONSE (v) -> calc_e:=(fun () -> e' v);raise Stop;e' v)  in
    (
      fun f -> 
      try 
        let p = ((!calc_e)()) in 
        let k = p f in
        calc_e:=(fun () -> p);
        k
      with Stop -> FAILURE )
     
  
  let run e =
    trace "run";
    let l = ref([]) in
    let fill v = l:= [REPONSE(v)] in
    let rec loop_until_success () =
      match(e fill) with
      |SUCCESS -> ()
      |FAILURE -> loop_until_success() 
      in
      loop_until_success();
       match (hd(!l)) with
    |REPONSE (v) ->  v
end
