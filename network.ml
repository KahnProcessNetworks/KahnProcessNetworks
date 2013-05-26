(* Implementation with processes communicating through the network. ***********)

open Format
open Marshal
open Server
open Sys
open Unix

let config = "network.config"
let father_computer = ref ""
let current_computer = ref ""


(* Auxilliar functions ********************************************************)

let retransmit in_file_descr out_file_descr : 'a =
    let in_chan = in_channel_of_descr in_file_descr in
    let out_chan = out_channel_of_descr out_file_descr in
    while (true) do
        let v = (from_channel in_chan : 'a) in
        to_channel out_chan (v : 'a) [Closures];
        flush out_chan;
    done;
    (from_channel in_chan : 'a)

let find_computers () =
    let computers = ref [] in
    let in_channel = open_in config in
    try
        while true do
            let computer = input_line in_channel in
            computers := computer :: !computers
        done;
        !computers
    with
    | End_of_file -> !computers

let choose_computers n =
    let computers = find_computers () in
    let rec choose n computers =
        match n with
        | 0 -> []
        | _ ->
            match computers with
            | [] -> failwith "empty network"
            | _ ->
                let p _ = Random.bool () in
                let (l1, l2) = List.partition p computers in
                let computers = l1 in
                let l3 = choose (n - 1) computers in
                match computers with
                | [] -> failwith "empty network"
                | computer :: tl ->
                    try
                        (computer, inet_addr_of_string computer) :: l3
                    with
                    | _ ->
                        try
                            let host_entry = gethostbyname computer in
                            (computer, host_entry.h_addr_list.(0)) :: l3
                        with
                        | _ -> choose n tl
    in
    choose n computers


(* Module *********************************************************************)

type 'a process = unit -> 'a
type 'a port =
{
    id: int;
    mutable chan: 'a;
    mutable is_active: bool;
}
type 'a in_port = in_channel port
type 'a out_port = out_channel port
type 'a request = string * string * 'a process

let new_port id chan is_active =
{
    id = id;
    chan = chan;
    is_active = is_active;
}

let new_channel =
    let id = ref 0 in
    fun () ->
        incr id;
        let in_chan = in_channel_of_descr stdin in
        let in_port = new_port !id in_chan false in
        let out_chan = out_channel_of_descr stdout in
        let out_port = new_port !id out_chan false in
        (in_port, out_port)

let put (v : 'a) p () =
    if (p.is_active)
    then
        begin
        to_channel p.chan (v : 'a) [Closures];
        flush p.chan
    else
        let (in_file_descr, out_file_descr) = pipe () in
        (** TODO: Changer en double fork pour les performances **)
        match fork () with
        | 0 ->
            (* Establish a relay *)
            close out_file_descr;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            connect sock addr;
            retransmit in_file_descr sock
        | _ ->
            (* Continue with put to relay *)
            close in_file_descr;
            p.chan <- out_channel_of_descr out_file_descr;
            p.is_active <- true;
            to_channel p.chan (v : 'a) [Closures];
            flush p.chan

let rec get p () : 'a =
    if (p.is_active)
    then ((from_channel p.chan) : 'a)
    else
        let (in_file_descr, out_file_descr) = pipe () in
        (** TODO: Changer en double fork pour les performances **)
        match fork () with
        | 0 ->
            (* Establish a relay *)
            close in_file_descr;
            let host = gethostbyname !father_computer in
            let addr = ADDR_INET (host.h_addr_list.(0), 1401) in
            let sock = socket PF_INET SOCK_STREAM 0 in
            connect sock addr;
            (retransmit sock out_file_descr : 'a)
        | _ ->
            (* Continue with put to relay *)
            close out_file_descr;
            p.chan <- in_channel_of_descr in_file_descr;
            p.is_active <- true;
            (from_channel p.chan : 'a)

let distribute (l : unit process list) : unit =
    (* Select computers over the network *)
    let n = List.length l in
    let computers = choose_computers n in
    let l = List.combine l computers in
    match fork () with
    | 0 ->
        (* Launch the service information about channels *)
        let mem = Hashtbl.create 50 in
        let rec service server_sock =
            let (client_sock, client_addr1) = accept server_sock in
            let in_chan = in_channel_of_descr client_sock in
            let out_chan1 = out_channel_of_descr client_sock in
            let id = (from_channel in_chan : int) in
            try
                let (client_addr2, out_chan2) = Hashtbl.find mem id in
                to_channel out_chan1 client_addr2 [Marshal.Closures];
                flush out_chan1;
                to_channel out_chan2 client_addr1 [Marshal.Closures];
                flush out_chan2;
                close_out out_chan1;
                close_out out_chan2
            with
            | Not_found ->
                Hashtbl.add mem id (client_addr1, out_chan1);
                service server_sock
        in
        Server.launch Sequential 1401 service
    | _ ->
        (* Request runs over the network *)
        (** TODO: Configurer les interruptions par signaux **)
        let rec send l =
            match l with
            | [] -> ()
            | (e, (computer, addr_inet)) :: tl ->
                match fork () with
                | 0 ->
                    (* Send the name of the computer and the process to run. *)
                    let addr = ADDR_INET (addr_inet, 1400) in
                    let sock = socket PF_INET SOCK_STREAM 0 in
                    connect sock addr;
                    let out_chan = out_channel_of_descr sock in
                    let m = (!current_computer, computer, e) in
                    to_channel out_chan (m : unit request) [Closures];
                    flush out_chan;
                    (* Wait for acknowledgment. *)
                    let in_chan = in_channel_of_descr sock in
                    ignore(from_channel in_chan : unit);
                    close sock;
                    exit 0
                | pid ->
                    send tl;
                    ignore (waitpid [] pid)
        in
        send l

let rec doco l () =
    match l with
    | [] -> ()
    | _ -> distribute l

let return v =
    fun () -> v

let bind e e' () =
    let v = e () in
    e' v ()

let rec wait () : 'a =
    let service client_sock =
        let in_chan = in_channel_of_descr client_sock in
        let m = ((from_channel in_chan) : 'a request) in
        let (father, computer, e') = m in
        close client_sock;
        father_computer := father;
        current_computer := computer;
        (** TODO: Rediriger stdin, stdout et sterr vers le père **)
        (** TODO: Rajouter le waitpid **)
        match fork () with
        | 0 -> (wait () : 'a)
        | _ -> (e' () : 'a)
    in
    (Server.launch Fork 1400 service : 'a)

let run e =
    let n = Array.length argv in
    if ("-wait" = argv.(n - 1))
    then
        (* Wait until a doco over the network requests a run *)
        (wait () : 'a)
    else
        (* Fork to wait for a doco and run the process *)
        (** TODO:    Rajouter le waitpid **)
        match fork () with
        | 0 -> (wait () : 'a)
        | _ -> (e () : 'a)
