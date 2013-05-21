(*
Use the command
	netstat -lp
to get a list of running servers, along with their pid.
*)   

(* Auxilliar Functions *)

let p s = Format.eprintf "%s@." s

let s () =
	let s = String.make 80 '-' in
	p s


(* Server *********************************************************************)

let make_sockaddr addr port = Unix.ADDR_INET (addr, port)
(* let make_sockaddr name = Unix.ADDR_UNIX (name) *)

let establish_server server_fun sockaddr =
	let sockdomain = Unix.domain_of_sockaddr sockaddr in
	let socktype = Unix.SOCK_STREAM in
	(* let socktype = Unix.SOCK_DGRAM in *)
	(* let socktype = Unix.SOCK_RAW in *)
	(* let socktype = Unix.SOCK_SEQPACKET in *)
	let sock = Unix.socket sockdomain socktype 0 in
	Unix.bind sock sockaddr;
	Unix.listen sock 3;
	let (file_descr, sockaddr) = Unix.accept sock in
	match Unix.fork () with
	| 0 ->
		let in_channel = Unix.in_channel_of_descr file_descr in
		let out_channel = Unix.out_channel_of_descr file_descr in
		server_fun in_channel out_channel;
		Unix.close sock;
		exit 0
	| pid ->
		let _ = Unix.waitpid [] pid in
		Unix.close sock;
		()

let get_my_addr () =
	let host_name = Unix.gethostname () in
	let host_entry = Unix.gethostbyname host_name in
	host_entry.Unix.h_addr_list.(0)

let main_server port server_fun =
	let addr = get_my_addr () in
	let sockaddr = make_sockaddr addr port in
	establish_server server_fun sockaddr

let server_fun in_channel out_channel =
	try
		while true do
			let s = input_line in_channel in 
			let r = String.uppercase s in
			Format.printf "%s@." r;
(** **)
			if (r = "END")
			then raise Exit;
(** **)
		done
	with
	| _ -> ()

let run_server port =
	Unix.handle_unix_error (main_server port) server_fun


(* Client *********************************************************************)

let open_connection sockaddr =
	let sockdomain = Unix.domain_of_sockaddr sockaddr in
	let socktype = Unix.SOCK_STREAM in
	(* let socktype = Unix.SOCK_DGRAM in *)
	(* let socktype = Unix.SOCK_RAW in *)
	(* let socktype = Unix.SOCK_SEQPACKET in *)
	let sock = Unix.socket sockdomain socktype 0 in
	try
		Unix.connect sock sockaddr;
		let in_channel = Unix.in_channel_of_descr sock in
		let out_channel = Unix.out_channel_of_descr sock in
		(in_channel, out_channel)
	with
	| exn ->
		Unix.close sock;
		raise exn

let shutdown_connection in_channel =
	let file_descr = Unix.descr_of_in_channel in_channel in
	(* let shutdown_command = Unix.SHUTDOWN_ALL in *)
	(* let shutdown_command = Unix.SHUTDOWN_RECEIVE in *)
	let shutdown_command = Unix.SHUTDOWN_SEND in
	Unix.shutdown file_descr shutdown_command

let main_client server port client_fun  =
	let server_addr =
		try
			(Unix.gethostbyname server).Unix.h_addr_list.(0) 
		with
		| Not_found ->
			try
				Format.printf "main_client warning: unknown server name %s@." server;
				Unix.inet_addr_of_string server 
			with
			| Failure("inet_addr_of_string") ->
				Format.printf "main_client error: unknown internet address %s@." server;
				exit 2
	in
	let sockaddr = Unix.ADDR_INET (server_addr, port) in 
	let (in_channel, out_channel) = open_connection sockaddr in
	client_fun in_channel out_channel;
	exit 0

let client_fun in_channel out_channel =
	try
		while true do
			let s = (input_line stdin) ^ "\n" in
			output_string out_channel s;
			flush out_channel;
			if (s = "end\n")
			then
				begin
				shutdown_connection in_channel;
				raise Exit
				end;
		done
	with
	| Exit ->
		()
	| exn ->
		shutdown_connection in_channel;
		raise exn

let run_client server port =
	Unix.handle_unix_error (main_client server port) client_fun


let () =
	let server = Unix.gethostname () in
	let port = 1400 in
	s ();
	match Unix.fork () with
	| 0 ->
		Unix.sleep 1;
		p "main information client running";
		run_client server port
	| pid ->
		p "main information: server running";
		run_server port;
		let _ = Unix.waitpid [] pid in
		s ();
		exit 0
