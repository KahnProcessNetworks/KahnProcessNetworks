module type S =
sig
    type 'a process
    type 'a in_port
    type 'a out_port
    
    val new_channel: unit -> 'a in_port * 'a out_port
    val put: 'a -> 'a out_port -> unit process
    val get: 'a in_port -> 'a process
    val doco: unit process list -> unit process
    val return: 'a -> 'a process
    val bind: 'a process -> ('a -> 'b process) -> 'b process
    val run: 'a process -> 'a
end


module Th : S =
struct
    type 'a process = 'a Th.process
    type 'a in_port = 'a Th.in_port
    type 'a out_port = 'a Th.out_port
    
    let new_channel = Th.new_channel
    let put = Th.put
    let get = Th.get
    let doco = Th.doco
    let return = Th.return
    let bind = Th.bind
    let run = Th.run
end

module Pipe : S =
struct
    type 'a process = 'a Pipe.process
    type 'a in_port = 'a Pipe.in_port
    type 'a out_port = 'a Pipe.out_port
    
    let new_channel = Pipe.new_channel
    let put = Pipe.put
    let get = Pipe.get
    let doco = Pipe.doco
    let return = Pipe.return
    let bind = Pipe.bind
    let run = Pipe.run
end

module Socket : S =
struct
    type 'a process = 'a Network.process
    type 'a in_port = 'a Network.in_port
    type 'a out_port = 'a Network.out_port
    
    let new_channel = Network.new_channel
    let put = Network.put
    let get = Network.get
    let doco = Network.doco
    let return = Network.return
    let bind = Network.bind
    let run = Network.run
end

module Seq : S =
struct
    type 'a process = 'a Sequential.process
    type 'a in_port = 'a Sequential.in_port
    type 'a out_port = 'a Sequential.out_port
    
    let new_channel = Sequential.new_channel
    let put = Sequential.put
    let get = Sequential.get
    let doco = Sequential.doco
    let return = Sequential.return
    let bind = Sequential.bind
    let run = Sequential.run
end

module Best : S =
struct
    type 'a process = 'a Th.process
    type 'a in_port = 'a Th.in_port
    type 'a out_port = 'a Th.out_port
    
    let new_channel = Th.new_channel
    let put = Th.put
    let get = Th.get
    let doco = Th.doco
    let return = Th.return
    let bind = Th.bind
    let run = Th.run
end
