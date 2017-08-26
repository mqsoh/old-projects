# Messing around with gen_server.

I think I need to make use of the Erlang OTP facilities for even this simple
web server I'm working on. This is building off of
[less_simple.knot.md](less_simple.knot.md) (which is building off of
[simple.knot.md](simple.knot.md).

I've found some of the OTP stuff hard to follow, but I think it's because it's
the most useful feature of Erlang. I think I just need to get familiar with it.

In Erlang, when you use the preprocessor directive `-behavior(supervisor).`,
that module is a `callback module` for the `supervisor behavior`. The
supervisor behavior is defined in core Erlang; you interact with your callback
module *through* the behavior module. The callback module process is started by
giving it as an argument to `supervisor:start_link`.

The child processes (acceptors) are created through `supervisor:start_child`.
They, in turn, are `gen_server callback modules` and are interacted with
through the `gen_server` module.

What I intend to do is to provide a custom behavior module. My supervisor and
gen_server callback modules will both hook into an actual implementation. I'm
going to call this the `myserver` behavior. The myserver behavior will have the
following functions.

- `(server-init server-name) -> (tuple 'ok port-number number-of-acceptors)`
  You can use the server name to decide which port to use or the number of
  acceptors. The usage I have in mind is running multiple instances for which
  you'll need to define different ports. Of course, you can always return `0`
  as a port number and be assigned any open port.

- `(acceptor-init server-name port-number number-of-acceptors) -> (tuple 'ok state)`
  When an acceptor is initialized, you'll get a server name, port number (in
  case you gave `0` in server-init), and the number of acceptors (just because
  why not?). The state you return will be given to...

- `(handle server-name socket state) -> (tuple 'close new-state)`
  For every request, this is the stuff you get and you need to return a new
  state. When the implementation returns `'close`, the connection will be
  closed and the acceptor process will accept a new connection. I have it in
  mind to implement persistent connections, so I think I'll support other
  return values eventually.



# A short break.

I'll probably need to print stuff for debugging. Here's a couple dumb functions
that look nicer to me.

```{.lisp name="print functions"}
(defun print [message]
  (lfe_io:format message '()))

(defun print [message data]
  (lfe_io:format message data))
```



# Supervisor

This is just boilerplate. This is what a module looks like.

###### file:src/myserver.lfe
```{.lisp name="file:src/myserver.lfe"}
(defmodule myserver
  (export
    <<exports>>
  ))

<<print functions>>
<<supervisor functions>>
<<testing helpers>>
```

Let's wrap the starting of a supervisor to make it a bit simpler for the user.
They only need to provide an implementation module and a server name and
they'll get back the port number of the listen socket.

###### supervisor functions
```{name="supervisor functions"}
<<server start link>>
```
###### exports
```{.lisp name="exports"}
(start-link 2)
```
```{.lisp name="server start link"}
(defun start-link [implementation-module server-name]
  "Starts a server using `implementation-module` as the myserver callback module."
  (let (((tuple 'ok pid) (supervisor:start_link (tuple 'local server-name)
                                                (MODULE)
                                                (list (self) implementation-module server-name))))
    (receive
      ((tuple 'port port)
        (tuple 'ok pid port)))))
```

The supervisor behavior will execute our callback module's init function. This
will also be our first call to the user's implementation module.

###### supervisor functions
```{name="supervisor functions"}
<<server initialization>>
```
###### exports
```{.lisp name="exports"}
(init 1)
```
```{.lisp name="server initialization"}
(defun init
  ([(list caller-pid implementation-module server-name)]
    (let* (((tuple 'ok port-num num-acceptors)
            (call implementation-module 'server-init server-name))

           ((tuple 'ok listen-socket)
            (gen_tcp:listen port-num '(binary #(active false))))

           ((tuple 'ok actual-port) (inet:port listen-socket)))

      (! caller-pid (tuple 'port actual-port))

      <<spawn acceptor processes>>
      <<supervisor init return value>>
    )))
```

We'll look at the `supervisor init return value` in a minute because it's
complicated.

This function establishes that:

> The implementation module's `server-init/1` fuction will return `(tuple 'ok
> port-num num-acceptors)`.

We have to check the port number on the socket because the user can provide `0`
as the port number and Erlang will select any available port. This is used in
the return value to give the port number to the child process.

###### supervisor init return value
```{.lisp name="supervisor init return value"}
(tuple
  'ok
  (tuple
    ; restart strategy and frequency
    (tuple 'simple_one_for_one 60 3600)

    ; A list of child specs, but we only have one.
    (list
      ; child spec
      (tuple
        ; id
        'myserver-acceptor

        ; child process MFA.
        (tuple 'myserver-acceptor 'start-link (list listen-socket
                                                 implementation-module
                                                 server-name
                                                 num-acceptors
                                                 actual-port))
        ; restart type
        'permanent

        ; shutdown timeout
        1000

        ; type of child (worker or supervisor)
        'worker

        ; modules
        (list 'myserver-acceptor)))))
```

This is a mind-numbing structure. I've come back to it a few times now and it's
starting to make sense, but...cheeses.

- The `simple_one_for_one` restart strategy allows us to define the module,
  function, and arguments to the child processes in this return value. That's
  what makes it simple -- all the children are the same. If the processes are
  restarted more than `60` times in `3600` seconds, then all children are
  killed and then the supervisor will kill itself.

- The `id` is arbitrary.

- The `child process MFA` is the module, function and arguments that will
  represent children of this supervisor. I'll have to write a modle named
  `myserver-acceptor` with a `start-link` function taking those arguments.

- The `permanent` restart type means that the child process will always be
  restarted. I think that's what we want out of a web server, but I'm pretty
  sure I saw an example where the restart type was `temporary`. I may not
  understand the man page.

- The `shutdown timeout` is the length of time, in seconds, that the supervisor
  will wait after calling `(erlang:exit child 'shutdown)`. It will then do a
  brutal kill with `(erlang:exit child 'kill)`.

- A child is a worker or another supervisor -- ours are workers.

- I understand [the instructions][] for `modules`, but I don't understand what
  the 'release handler' does, so this seems bizarre. Since the child processes
  will be gen_servers, we have to specify `'myserver-acceptor` here.

[the instructions]: http://www.erlang.org/doc/man/supervisor.html


###### spawn acceptor processes
```{.list name="spawn acceptor processes"}
(spawn_link (MODULE) 'spawn-children (list server-name num-acceptors))
```

The supervisor init function has to spawn the children, but the children (in our
case) are blocking processes (ultimately due to `gen_tcp:accept`. The
`spawn_link` here starts a transient, non-blocking process: `spawn-children`.

This implementation is based on the socket server in the Learn You Some Erlang
book, [Buckets of Sockets][].

[buckets of sockets]: http://learnyousomeerlang.com/buckets-of-sockets

###### exports
```{.lisp name="exports"}
(spawn-children 2)
```
###### supervisor functions
```{name="supervisor functions"}
<<spawn children>>
```
```{.lisp name="spawn children"}
(defun spawn-children
  ([_ 0] 'ok)
  ([server-name num]
    ; No arguments are given here, but they will get the listen socket from the
    ; child spec given in `init/2`.
    (supervisor:start_child server-name '())
    (spawn-children server-name (- num 1))))
```

When `supervisor:start_child` is called, the message to start the child is
queued up for the supervisor. It is this level of indirection that enables the
supervisors init function to exit. Then the `simple_one_for_one` child spec
takes over; the MFA (module, function, and arguments) defined in the supervisor
init's return value are then executed.



# Workers

The workers are processes that will accept incoming connections. The number of
them is defined by the `myserver:spawn-children` loop which ultimately get the
number from our custom callback module's `server-init/1`.

The acceptor process will be a `gen_server`, so we'll need a module definition
like this:

###### file:src/myserver-acceptor.lfe
```{.lisp name="file:src/myserver-acceptor.lfe"}
(defmodule myserver-acceptor
  (export
    ; Called from supervisor.
    (start-link 5)

    ; Required gen_server exports.
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
    (format_status 2)))


; Some unused gen_server functions.
(defun handle_call [_request _from state]
  (tuple 'noreply state))
(defun handle_info [_info state]
  (tuple 'noreply state))
(defun terminate [_reason _state]
  'ok)
(defun code_change [_old-version state _extra]
  (tuple 'ok state))
(defun format_status
  ([_opt (list _pdict state)]
    ; This format is recommended by the man page.
    '(#('data '(#("State" "I'm running, I guess."))))))


<<print functions>>
<<acceptor functions>>
```

The first thing we can establish is the `start_link` function. The
`myserver:init` function provides the arguments in the child spec, so that we
can expect the following arguments from `supervisor init return value` code
block:

- a listen socket,
- an implementation module name,
- a server name,
- the number of workers, and
- the port number of the server.

In [the Erlang documentation for gen_server][], it says that
`gen_server:start_link` must be called, directly or indirectly, by the
supervisor. We're doing it indirectly becase the supervisor is calling our
callback module's `start-link` and this function is then calling
`gen_server:start_link`. I'm not sure what happens...

[the Erlang documentation for gen_server]: http://www.erlang.org/doc/man/gen_server.html

**TODO**: Look in to the internals of supervision trees. How are the links made?

So, `start-link` takes all the arguments from the supervisor's child spec and,
through `gen_server:start_link`, forwards it on to the acceptor module's `init`
function.

###### acceptor functions
```{name="acceptor functions"}
<<acceptor start link>>
```
```{.lisp name="acceptor start link"}
(defun start-link [listen-socket implementation-module server-name num-acceptors port-number]
    (gen_server:start_link (MODULE)
                           (list listen-socket
                                 implementation-module
                                 server-name
                                 num-acceptors
                                 port-number)
                           '()))
```

When the acceptor is inited by the gen_server behavior module, we'll establish
the second convention for the implementation module.

**TODO**: How do I want to link to, and or reference the stuff at the top of
the module?

###### acceptor functions
```{name="acceptor functions"}
<<acceptor init>>
```
```{.lisp name="acceptor init"}
(defun init
  ([(list listen-socket implementation-module server-name num-acceptors port-number)]
    (let (((tuple 'ok implementation-state) (call implementation-module
                                                  'acceptor-init
                                                  server-name
                                                  port-number
                                                  num-acceptors)))
      (gen_server:cast (self) 'accept)

      (tuple 'ok (map 'listen-socket listen-socket
                      'implementation-module implementation-module
                      'server-name server-name
                      'num-acceptors num-acceptors
                      'port-number port-number
                      'implementation-state implementation-state)))))
```

The `cast` here is the same level of indirection as the spawning of children in
the supervisor. (Calls to `gen_tcp:accept` block so this lets init return.)

Now we can establish the third convention for the implementation module. This
is where the server accepts the connection, gives the socket to the
implementation module, and closes the connection. `handle` in the
implementation module is free to write to the socket.

```{name="acceptor functions"}
<<accept connections>>
```
```{.lisp name="accept connections"}
(defun handle_cast
  (['accept state]
    (let* ((listen-socket (map-get state 'listen-socket))
           (implementation-module (map-get state 'implementation-module))
           (implementation-state (map-get state 'implementation-state))
           (server-name (map-get state 'server-name))

           ((tuple 'ok accepted-socket) (gen_tcp:accept listen-socket))
           ((tuple 'close new-state) (call implementation-module
                                           'handle
                                           server-name
                                           accepted-socket
                                           implementation-state)))
      (gen_tcp:close accepted-socket)
      (handle_cast 'accept (map-set state 'implementation-state new-state)))))
```



# Testing it.

I've written a lot of code, but I haven't tested a bit of it! Let's create a
test implementation module.

###### file:test/myserver-implementation.lfe
```{.lisp name="file:test/myserver-implementation.lfe"}
(defmodule myserver-implementation
  (export all))

<<print functions>>

<<test implementation of server-init>>
<<test implementation of acceptor-init>>
<<test implementation of handle>>

<<test function>>
```

I'll use pattern matching in the implementation functions to assert the proper
values. That means that when running the unit tests, I only need to start the
server up and send a test request. We'll expect a response of `"I am alive."`.

###### test function
```{.lisp name="test function"}
(defun server_test []
  (let* (((tuple 'ok pid port) (myserver:start-link 'myserver-implementation 'my-server-name))
         ((tuple 'ok connection) (gen_tcp:connect "localhost" port '(list))))

    (gen_tcp:send connection "Are you alive?")

    (receive
      ((tuple 'tcp connection "I am alive."))
      (after 1000))

    (gen_tcp:close connection)))
```

The supervisor will now call this implementation module's server-init. And we
need to give it: `(tuple 'ok port-number number-of-acceptors)`. I want to
assert that the `'my-server-name` atom is passed through, so I'm using LFE's
pattern matching version of `defun`.

###### test implementation of server-init
```{.lisp name="test implementation of server-init"}
(defun server-init
  (['my-server-name]
    (tuple 'ok 0 1)))
```

The supervisor starts a gen_server and the `myserver-acceptor` module calls our
`acceptor-init`.

###### test implementation of acceptor-init
```{.lisp name="test implementation of acceptor-init"}
(defun acceptor-init
  (['my-server-name port-number 1]
    (tuple 'ok 'my-state)))
```

The test function will have sent a request: `"Are you alive?"` and we need to
respond with `"I am alive."`. We can also make an assertion about the server
name and the state given in `acceptor-init/3`.

###### test implementation of handle
```{.lisp name="test implementation of handle"}
(defun handle
  (['my-server-name socket 'my-state]
    (inet:setopts socket '(#(active once)))
    (receive
      ((tuple 'tcp socket "Are you alive?")
        (gen_tcp:send socket "I am alive."))
      (after 1000))))
```

The test will pass, but an error is printed.

    =ERROR REPORT==== 10-Apr-2015::19:20:51 ===
    Error in process <0.63.0> with exit value: {undef,[{'myserver','spawn-children',['my-server-name',1],[]}]}

I'm pretty sure this is caused by the listen socket being shut down while the
blocking `gen_tcp:accept` is running. Safe to ignore it, I think.



# Testing helpers.

When writing an implementation module, I'll write directly to the socket. When
I write unit tests, I'll want to test this side effect.

Web frameworks usually abstract out the the sockets. The response text is
buffered and sent to the client at the end of the program logic. Then, when
someone needs to stream data back to the client, they add a feature for
streaming. I've long wondered if there was a maintainable way of giving the web
application the socket to do with as it pleases. I'll do that here because I'm
wondering what it will look like.

To create the mock server:

- create a listen socket on any open port,
- create a client connection to the listen socket,
- accept the connection, and
- return `(tuple listen-socket accepted-socket client-socket)`

To get the data sent to the client, `destroy-mock-server` accepts that response
as input and returns the data sent to the client.

###### exports
```{.lisp name="exports"}
(make-mock-server 0)
(destroy-mock-server 1)
```
###### testing helpers
```{.lisp name="testing helpers"}
(defun make-mock-server []
  "Creates a few sockets for testing side effects of the web server."
  (let* (((tuple 'ok listen-socket) (gen_tcp:listen 0 '(binary #(active false))))
         ((tuple 'ok actual-port) (inet:port listen-socket))
         ((tuple 'ok client) (gen_tcp:connect "localhost" actual-port '(binary #(active false))))
         ((tuple 'ok accepted-socket) (gen_tcp:accept listen-socket)))
    (tuple listen-socket accepted-socket client)))

(defun destroy-mock-server
  "Closes all the mock server sockets and returns the data sent to the client."
  ([(tuple listen-socket accepted-socket client)]
    (let ((data (case (gen_tcp:recv client 0 500)
                      ; No data was given to the client, so just return an
                      ; empty bitstring.
                      ((tuple 'error 'timeout)
                        #B())
                      ((tuple 'ok data)
                        data))))
      (gen_tcp:close client)
      (gen_tcp:close accepted-socket)
      (gen_tcp:close listen-socket)
      data)))
```

This is how I tested these mock server functions in the LFE shell:

```{name="lkasdjfa"}
> (set mock-server (myserver:make-mock-server))
#(#Port<0.2755> #Port<0.2758> #Port<0.2757>)
> (set (tuple _ accepted-socket _) mock-server)
#(#Port<0.2755> #Port<0.2758> #Port<0.2757>)
> (gen_tcp:send accepted-socket "Hi, world.")
ok
> (set response (myserver:destroy-mock-server mock-server))
#B(72 105 44 32 119 111 114 108 100 46)
> (lfe_io:format "~s~n" (list response))
Hi, world.
ok
>
```



# Farewell

The test implementation module is working. The OTP behaviors are simple,
especially when Joe Armstrong explains them in his book, but I had some mental
block with it. After writing this, I feel like I understand it.

I intend to use this to write a game with a REST API. This server doesn't deal
directly with HTTP. When I looked into Clojure, the ring server really inspired
me in that a fully featured web server can be implemented as a sequence of
middle ware. In this case, even the HTTP-ness of the server will be middle
ware.

Maybe I could use this bit of code to create some other kind of server. :)
Anyway, I'm going to go do that now.
