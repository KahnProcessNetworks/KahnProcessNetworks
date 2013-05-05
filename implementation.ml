(* Implementation with processes communicating through pipes. *****************)

open Unix
open Kahn


module S : S =
struct
	type 'a process = (unit -> 'a)
	type 'a channel = file_descr
	type 'a in_port = 'a channel
	type 'a out_port = 'a channel
	
	let new_channel =
		let i = ref (-1) in
		fun () ->
			let () = incr i in
			let name = string_of_int !i in
			let in_fd = openfile name [O_RDWR] 0o640 in
			let out_fd = dup in_fd in
			(in_fd, out_fd)
	
	let put (v : 'a) (p : 'a out_port) () =
		let c = out_channel_of_descr p in
		(** To improve **)
		Marshal.to_channel c (v : 'a) [ Marshal.Closures ];
		close_out c
	
	let rec get (p : 'a in_port) () =
		let c = in_channel_of_descr p in
		try
			(** To improve **)
			let v = ((Marshal.from_channel c) : 'a) in
			close_in c;
			v
		with
			_ -> get p ()
	
	let doco l =
		failwith "doco not implemented"
	
	let return v =
		fun () -> v
	
	let bind e e' =
		failwith "bind not implemented"
	
	let run e =
		e ()
end
