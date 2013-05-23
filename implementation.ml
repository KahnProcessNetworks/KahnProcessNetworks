(* Implementation with processes communicating through the network. ***********)

open Kahn
open Miscellaneous
open Unix
<<<<<<< HEAD

=======
open List
exception Stop of (unit -> unit) 
>>>>>>> f211540b53da447d7bcf0811d6a7b7db40dd6102

(* Auxilliar functions ********************************************************)

let get_addr () =
    trace "get_addr";
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    host_entry.h_addr_list.(0)

<<<<<<< HEAD
let make_sockaddr port =
    trace "make_sockaddr";
    let addr = get_addr () in
    ADDR_INET (addr, port)

(*
Par défaut, on utilise l'adresse internet de la machine hôte uniquement. On
pourrait imaginer un fichier de configuration contenant les adresses disponibles
pour les sockets mais il faudrait savoir créer et utiliser des RPC en OCaml pour
établir à distance les serveurs du réseau. 
Pour mémoire :
type sockaddr =
    | ADDR_UNIX of string
    | ADDR_INET of inet_addr * int
*)


(* Module *********************************************************************)

module S : S =
struct
    type 'a process = unit -> 'a
    type 'a port =
    {
        addr: sockaddr;
        mutable channel: 'a;
        mutable is_not_active: bool
    }
    type 'a in_port = in_channel port
    type 'a out_port = out_channel port
    
    let new_port addr channel is_not_active =
        trace "new_port";
        {
            addr = addr;
            channel = channel;
            is_not_active = is_not_active;
        }
    
    let new_channel =
        let port = ref 1399 in
        fun () ->
            trace "new_channel";
            incr port;
            let sockaddr = make_sockaddr !port in
            let in_channel = in_channel_of_descr stdin in
            let in_port = new_port sockaddr in_channel true in
            let out_channel = out_channel_of_descr stdout in
            let out_port = new_port sockaddr out_channel true in
            (in_port, out_port)
    
    let new_socket addr =
        let sockdomain = domain_of_sockaddr addr in
        let socktype = SOCK_STREAM in
        socket sockdomain socktype 0
    
    let rec rec_connect sock addr =
        try
            connect sock addr
        with
            _ -> rec_connect sock addr
    
    let retransmit in_channel out_channel =
        while true do
            let v = ((Marshal.from_channel in_channel) : 'a) in
            Marshal.to_channel out_channel (v : 'a) [ Marshal.Closures ]
        done

    let put (v : 'a) p () =
        trace "put";
        if (p.is_not_active)
        then
            begin
            let (in_file_descr, out_file_descr) = pipe () in
            match Unix.fork () with
            | 0 ->
                let in_channel = in_channel_of_descr in_file_descr in
                close out_file_descr;
                let sock = new_socket p.addr in
                rec_connect sock p.addr;
                let out_channel = out_channel_of_descr sock in
                retransmit in_channel out_channel
            | _ ->
                close in_file_descr;
                let out_channel = out_channel_of_descr out_file_descr in
                p.channel <- out_channel;
                p.is_not_active <- false;
            end;
        Marshal.to_channel p.channel (v : 'a) [ Marshal.Closures ]
    
    let rec get p ()=
        trace "get ";
        if (p.is_not_active)
        then
            begin
            let (in_file_descr, out_file_descr) = pipe () in
            match Unix.fork () with
            | 0 ->
                close in_file_descr;
                let out_channel = out_channel_of_descr out_file_descr in
                let sock = new_socket p.addr in
                bind sock p.addr;
                listen sock 10;
                let (file_descr, _) = accept sock in
                let in_channel = in_channel_of_descr file_descr in
                retransmit in_channel out_channel
            | _ ->
                let in_channel = in_channel_of_descr in_file_descr in
                close out_file_descr;
                p.channel <- in_channel;
                p.is_not_active <- false;
            end;
        ((Marshal.from_channel p.channel) : 'a)
    
    let rec doco l () =
        trace "doco";
        match l with
        | [] -> ()
        | hd :: tl ->
            match fork () with
            | 0 ->
                trace "fork (child)";
                hd ()
            | pid ->
                trace "fork (father)";
                doco tl ();
                ignore (waitpid [] pid)
    
    let return v =
        trace "return";
        fun () -> v
    
    let bind e e' () =
        trace "bind";
        let v = e () in
        e' v ()
    
    let run e =
        trace "run";
        e ()    
end
=======
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
>>>>>>> f211540b53da447d7bcf0811d6a7b7db40dd6102
