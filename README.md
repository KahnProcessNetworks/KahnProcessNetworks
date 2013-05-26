# KahnProcessNetworks
KahnProcessNetworks is a set of OCaml's implementations for Kahn process networks, developped by Axel Davy and Baptiste Lefebvre. Processes communicate through threads, pipes or the network depending of the implementation. A last implementation simulate paralellism sequentially.

## Quick start
* Clone the repo:
	`git clone git://github.com/KahnProcessNetworks/KahnProcessNetworks.git`
* Change the directory:
	`cd KahnProcessNetworks`
* Compile the program:
	- `make thread`
	- `make pipe`
	- `make network`
	- `make sequential`
* Execute the program:
	`./a.out`

## Report

### Technical choice
We chose to use the following OCaml modules:
* `Marshal`
* `Thread`
* `Unix`

### Difficulties
We don't really know what is the appropriate implementation of the Kahn process networks with communication through the network. For instance we have aligned sockets with pipes and servers with processes but we are unable to establish server in a different location than the host machine. RPC could be the way to do so.

### Unrealized items
We have made the implementation without taking care about `Unix` errors. For example interruptions with signals are not handle.

## Note
* Marshal functional values is possible only if the two sides (sender and receiver) are the same program.
* Use a double fork instead of one could be a very good idea to avoid cluttering the memory.

## Authors
- [**Axel Davy**](http://github.com/axeldavy)
- [**Baptiste Lefebvre**](http://github.com/BaptisteLefebvre)
