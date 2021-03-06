(* Implementation with processes communicating through the network. ***********)


(* TODO: handle the errors than can (and will) occurs:
Example:
Fatal error: exception Unix.Unix_error(15, "accept", "")
Fatal error: exception Unix.Unix_error(63, "connect", "")
Fatal error: exception Sys_error("Connection reset by peer")
*)

open Unix
open Kahn


(* Constant *******************************************************************)

let trace_enable = false


(* auxiliary functions *)

let trace =
  let i = ref 0 in
  fun s ->
    if (trace_enable)
    then
      begin
      incr i;
      Format.printf "// l: %d  pid: %d  f: %s@.%!" !i (getpid ()) s
      end
    else ()

let trace_error f x =
  try
    f x
  with
    Unix_error (error, function_name, arguments) ->
      trace (function_name ^ ": " ^ (error_message error) ^ ", " ^ arguments);
      raise Exit

let make_addr port = 
  let ip_addr = inet_addr_loopback in
    ADDR_INET (ip_addr,port)
    
let run_client f addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt_optint s SO_LINGER None;
  connect s addr;
  let answ = f s in
  Unix.close s;
  answ
  

let run_server service addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;
  setsockopt_optint s SO_LINGER (Some (0));
  bind s addr;
  listen s 100;
  trace "server ready";
  while true do
    let (fd_client, addr_client) = accept s in
    service fd_client addr_client;
    Unix.close fd_client;
  done
  
  

(* module *)



module S : S =
struct
  type 'a process = unit -> 'a
  type 'a in_port = Unix.sockaddr
  type 'a out_port = Unix.sockaddr
  
  type 'a request = PUT of 'a | GET
  type 'a answer = SUCCESS of 'a | FAILURE
  
  let port_min = ref(10000)
  let port_max = ref(9999) (* last port given *)
  
  
  let rec create_servers l =
    trace "create servers for channels";
    match l with
    | [] -> ()
    | hd :: tl ->
      match fork () with
      |  0 ->
        trace "fork (child)";
        trace_error hd ()
      |  pid ->
        trace "fork (father)";
        trace_error create_servers tl
  
  let server_channel port = (* a server that turns everytime to receive requests and send answers if needed *)
  (*represents the channel: we can send put and get commands*)
    let lv = ((ref []) :'a list ref) in
    let respond out_ch answ = (* sens an answer *)
      Marshal.to_channel out_ch answ [ Marshal.Closures ];flush out_ch (* flushing is important *)
    in
    let service fd addr_client =
        let out_ch = out_channel_of_descr fd in
      let in_ch = in_channel_of_descr fd in
        match ((Marshal.from_channel in_ch): 'a request) with
        | PUT(v) ->
        lv := !lv @ [v];
        trace ("server_channel " ^ (string_of_int port) ^ " has put a new value:" ^(string_of_int v));
        | GET ->
        match !lv with
          | [] ->
          trace ("server_channel " ^ (string_of_int port) ^ " get unsuccessful");
          respond out_ch FAILURE
          | v::q ->
          trace ("server_channel " ^ (string_of_int port) ^ " get successful:"^(string_of_int v));
          lv := q;
          respond out_ch (SUCCESS v)
    in
    trace "server_channel";
    run_server (service) (make_addr port)
  
  let new_channel () =
    trace "new_channel";
    port_max := !port_max + 1;
    let c = make_addr !port_max in
    create_servers [(fun () -> server_channel !port_max)];
    (c, c)
    
  
  
  let put (v : 'a) c () =
    let f s = 
      let out_ch = out_channel_of_descr s in
      Marshal.to_channel out_ch (PUT(v)) [ Marshal.Closures ];
      flush out_ch;
    in
    trace "put";
    run_client f c (* we sent a put request to the server representing the channel *)
  
  let rec get c ()=
    let rec loop_until_success () =
    let send_request_and_receive s = 
      let out_ch = out_channel_of_descr s and in_ch = in_channel_of_descr s in 
        Marshal.to_channel out_ch (GET) [ Marshal.Closures ]; 
        flush out_ch; (*send a GET request *)
        ((Marshal.from_channel in_ch) : 'a answer) (*get the answer *)
    in
    let answ = run_client (send_request_and_receive) c in 
        (*for i =0 to 100000 do let () = () in ();done;*)
        match answ with
          |SUCCESS v -> v
          |FAILURE -> loop_until_success ()
    in
    trace "get ";
    loop_until_success ()
    
  let rec doco l () =
    trace "doco";
    match l with
    | [] -> ()
    | hd :: tl ->
      match fork () with
      |  0 ->
        trace "fork (child)";
        trace_error hd ()
      |  pid ->
        trace "fork (father)";
        trace_error (doco tl) ();
        let _ = wait () in
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
