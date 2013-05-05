(* Implementation simulating parallelism sequentially. ************************)

open Unix
open Kahn


module S : S =
struct
	type 'a process = unit
	type 'a in_port = unit
	type 'a out_port = unit
	
	let new_channel () =
		failwith "new_channel not implemented"
	
	let put v c =
		failwith "put not implemented"
	
	let rec get c =
		failwith "get not implemented"
	
	let doco l =
		failwith "doco not implemented"
	
	let return v =
		failwith "return not implemented"
	
	let bind e e' =
		failwith "bind not implemented"
	
	let run e =
		failwith "run not implemented"
end
