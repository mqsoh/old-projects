# The most basic web server.

This is mostly a straight copy of the example in the `gen_tcp` man page. I
think I only changed the name of some variables when I converted it to LFE's
syntax.

This file is here mainly for reference. I'm going to add features in
[myweb.knot.md](myweb.knot.md).

This is the layout of the module. Aside from the `client`, it's all linear:
`start` uses `start-servers` uses `server` uses `loop`.

###### file:src/simple.lfe
```lisp
(defmodule simple
  (import
    ###### imports
    )
  (export
    ###### exports
    ))

###### start a server
###### spawn processes that accept connections
###### accept and delegate to request/response cycle
###### request/response
###### a simple client
```


The start function is the API for this module. It opens the listen socket and
delegates to other functions to start the acceptors.

###### exports
```lisp
(start 2)
```
###### imports
```lisp
(from gen_tcp (listen 2))
(from inet (port 1))
```
###### start a server
```lisp
(defun start [num-acceptors port-num]
  (case (listen port-num '(#(active false) #(packet 2)))
    ((tuple 'ok listen-socket)
      (start-servers num-acceptors listen-socket)
      (let (((tuple 'ok port) (port listen-socket)))
        port))

    ((tuple 'error reason)
      (tuple 'error reason))))
```


The `start-servers` function just spawns the specified number of acceptors.

###### imports
```lisp
(from erlang (spawn 3))
```
###### spawn processes that accept connections
```lisp
(defun start-servers
  ((0 _) 'ok)
  ((num-acceptors listen-socket)
    (spawn (MODULE) 'server (list listen-socket))
    (start-servers (- num-acceptors 1) listen-socket)))
```


A `server` is a process that waits for an incoming connection and delegates to
`loop` for the request/response cycle.

###### imports
```lisp
(from gen_tcp (accept 1))
(from io (format 2))
(rename io ((format 2) print))
```
###### accept and delegate to request/response cycle
```lisp
(defun server [listen-socket]
  (case (accept listen-socket)
    ((tuple 'ok accepted-socket) (loop accepted-socket)
                                ; recurse
                                (server listen-socket))
    (other (print "accept returned ~w - goodbye!~n" (list other)))))
```

Interestingly, `(fun spawn 3)` requires the function to be exported, so I'll
add it.

###### exports
```lisp
(server 1)
```


Now the loop handles the request/response cycle. For now I will just make it
echo the data it receives.

###### imports
```lisp
(from inet (setopts 2))
(from gen_tcp (send 2))
```
###### request/response
```lisp
(defun loop [accepted-socket]
  (setopts accepted-socket '(#(active once)))
  (receive
    ((tuple 'tcp accepted-socket data)
      (send accepted-socket data)
      ; recurse
      (loop accepted-socket))

    ((tuple 'tcp_closed accepted-socket)
      (print "Socket ~w closed [~w]~n" (list accepted-socket (self)))
      'ok)))
```


And now we can echo messages with this simple client.

###### imports
```lisp
(from gen_tcp (close 1)
              (connect 3)
              (recv 2))
```
###### exports
```lisp
(client 2)
```
###### a simple client
```lisp
(defun client [port-num message]
  (let (((tuple 'ok socket) (connect "localhost" port-num '(#(active false) #(packet 2)))))
    (send socket message)

    (let ((response (recv socket 0)))
      (close socket)
      response)))
```
