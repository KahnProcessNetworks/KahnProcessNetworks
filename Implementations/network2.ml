(* Implementation with processes communicating through the network. ***********)

open Kahn
open Miscellaneous
open Unix


(* Auxilliar functions ********************************************************)

let get_addr () =
    trace "get_addr";
    let host_name = gethostname () in
    let host_entry = gethostbyname host_name in
    host_entry.h_addr_list.(0)

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
    
    let put (v : 'a) p () =
        trace "put";
        if (p.is_not_active)
        then
            begin
            let sockdomain = domain_of_sockaddr p.addr in
            let socktype = SOCK_STREAM in
            let sock = socket sockdomain socktype 0 in
            connect sock p.addr;
            let out_channel = out_channel_of_descr sock in
            p.channel <- out_channel;
            p.is_not_active <- false;
            end;
        Marshal.to_channel p.channel (v : 'a) [ Marshal.Closures ]
    
    let rec get p ()=
        trace "get ";
        if (p.is_not_active)
        then
            begin
            let sockdomain = domain_of_sockaddr p.addr in
            let socktype = SOCK_STREAM in
            let sock = socket sockdomain socktype 0 in
            bind sock p.addr;
            listen sock 10;
            let (file_descr, _) = accept sock in
            let in_channel = in_channel_of_descr file_descr in
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
                let _ = waitpid [] pid in
                ()
    
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
