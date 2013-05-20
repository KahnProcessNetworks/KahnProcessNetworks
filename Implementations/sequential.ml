(* Implementation simulating parallelism sequentially. ************************)

open Unix
open Kahn
open List
exception Stop 
(* Constant *******************************************************************)

let trace_enable = true


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
      
      (*
      
      * idée d'amélioration: 
actuellement les fonctions données à put et get sont les fonctions fill données par bind et run.
doco pourrait appeller get et put avec une autre fonction, qui en cas de succès de get/put appelle la fonction de départ (fill) avec le bon argument
et si get échoue, ne pas appeler la fonction de départ, mais réappeler get plus tard.

TODO pour cette méthode: remplacer l'implémentation des channel par c une fonction qui rempli/dépile une liste, comme ça on peut tester si c'est vide au lieu de bloquer
Je n'ai pas le temps là, peut-être plus tard.

*)  



  let rec doco l = (* doco très imparfait *)
    trace "doco";
    match l with
    | [] -> failwith "doco on an empty list"
    | [f] -> trace "fin doco";f
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
  
  
  let bind (e:'a process) e'=
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
		|REPONSE (v) -> calc_e:=(fun () -> v);raise Stop;v) in
    (
      fun f -> 
      try 
        let v = ((!calc_e)()) in 
        e' v f
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
