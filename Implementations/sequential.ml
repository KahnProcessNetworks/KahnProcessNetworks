(* Implementation simulating parallelism sequentially. ************************)

open Unix
open Kahn
open List

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
  type 'a process = ('a -> unit) -> unit
  type 'a in_port = in_channel
  type 'a out_port = out_channel
  
  type 'a reponse = REPONSE of 'a 
  let () = Random.self_init () 
  
  let new_channel () =
    trace "new_channel";
    let (fd_in, fd_out) = pipe () in
    (in_channel_of_descr fd_in, out_channel_of_descr fd_out)
  
  let put v c f =
    trace "put";
    Marshal.to_channel c (v : 'a) [ Marshal.Closures ];
    f ()
  
  let rec get c f = (* problème si get est bloquant *)
    trace "get";
    let v = ((Marshal.from_channel c) : 'a) in
    f v
  
(* idée d'amélioration: 
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
    | [f] -> f
    | a::q -> if(Random.bool ())                  
               then  (fun v -> a v;(doco q) v)
               else  (fun v -> (doco q) v; a v)
  
  let return v f=
    trace "return";
    f v
    
  
  let bind e e' f=
    trace "bind";
    let l = ref([])in
    let fill v = l:= [REPONSE (v)] in
    e fill;
    match (hd(!l)) with
    |REPONSE (v) -> e' v f (* allow to have v = () *)
  
  let run e =
    trace "run";
    let l = ref([]) in
    let fill v = l:= [v] in
    e fill;
    hd (!l)
       
end
