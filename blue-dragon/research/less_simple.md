# My web server.

This will be improvements to the server outlined in
[simple.knot.md](simple.knot.md), which was just a transliteration of the
example in the Erlang `gen_tcp` man page into LFE.

I'm going to provide a Markdown file with API documentation.

```{.markdown name="file:API.md"}
# Myweb documentation.

This file is generated from the program contained in [myweb.knot.md](myweb.knot.md).

## API
<<api>>
```

I initially tried to build this server from the top down. It wasn't working for
me. Even small changes were causing little refactors throughout the file. It
irritates me when I have to do that. The thing I got hung up on is request
parsing. Erlang has an `erlang:decode_packet` function that will process HTTP
requests. It's nice because it assumes that it will be given segments of data.
It will respond with `(tuple 'more length)` when it needs something more. Let's
start with that.



# HTTP request parser.

The request parser will populate an `request` record. The `headers` will be a
`map`, but everything else will be a string (their defaults are `'undefined`).

```{.lisp name="records"}
(defrecord request
  method
  uri
  version
  (headers (map))
  (body ""))
```

As a request is being processed I need to maintain the current state. The
socket will return data as binaries, so the buffer will default to an empty
binary. (However, by default, `erlang:decode_packet` returns strings.)

```{.lisp name="records"}
(defrecord request-state
  (data #B())
  (request (make-request)))
```

A `request parser` will return one of three things:

- `(tuple 'more updated-state)` when more data is needed.
- `(tuple 'ok request-record)` when the request is complete.
- `(tuple 'error reason)` when the request is malformed.



## Tests for the HTTP request parser.

Let's start with some tests. Except for some of the debugging macros, eunit
works in LFE without any changes. (Anything using the FILE macro raises an
error; I think I saw this acknowleged by Robert Virding in the news group.)

What doesn't work in LFE is the `-ifdef(TEST).` compiler directive, so it's
usual to keep tests in a separate file. Here is `myweb-tests.lfe`.

```{.lisp name="file:test/myweb-tests.lfe"}
(defmodule myweb-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

<<records>>

<<tests>>
```

This is the bare minimum of a successful request and we would expect the http
request parser to return an `ok` response.

```{name="bare minimum http request"}
GET /foobar?a=b HTTP/1.1\r\n
Host: example.com\r\n
\r\n
```
```{.lisp name="tests"}
(defun bare-minimum-request_test []
  (assertEqual
    (tuple 'ok (make-request method 'GET
                             uri "/foobar?a=b"
                             version (tuple 1 1)
                             headers (map 'Host "example.com")
                             body ""))

    (myweb:request-parser
      (erlang:iolist_to_binary (list
        #B("<<bare minimum http request>>")
        ))
      (make-request-state))))
```

When we're given only a portion of the request line, the parser will request
more data and return an updated state.

```{.lisp name="tests"}
(defun incomplete-request-line_test []
  (assertEqual
    (tuple 'more (make-request-state data #B("GET /foobar")
                                     request (make-request)))
    (myweb:request-parser #B("GET /foobar")
                          (make-request-state))))
```

Similarly, given a complete request line and partial headers, the parser will
still need more data.

```{.lisp name="tests"}
(defun incomplete-headers_test []
  (assertEqual
    (tuple 'more (make-request-state data #B("Host: ex")
                                     request (make-request method 'GET
                                                                  uri "/foobar?a=b"
                                                                  version (tuple 1 1))))
    (myweb:request-parser #B("GET /foobar?a=b HTTP/1.1\r\nHost: ex")
                          (make-request-state))))
```

An HTTP request body is demarcated by an empty line, so I want to test this
boundary. In this test the `Host` header is only followed by one line break. It
seems to me that `erlang:decode_packet` could return the header since it's
followed by a line break. It doesn't. It waits until either the next header
starts or the headers end and the body starts.

```{.lisp name="tests"}
(defun header-boundary_test []
  (assertEqual
    (tuple 'more (make-request-state data #B("Host: example.com\r\n")
                                     request (make-request method 'GET
                                                           uri "/foobar?a=b"
                                                           version (tuple 1 1)
                                                           headers (map))))
    (myweb:request-parser #B("GET /foobar?a=b HTTP/1.1\r\nHost: example.com\r\n")
                          (make-request-state))))
```



## HTTP request parser implementation.

Now I can implement the first myweb function, the request parser. Here's the
module definition.

```{.lisp name="file:src/myweb.lfe"}
(defmodule myweb
  (import
    <<imports>>
    )
  (export
    <<exports>>
    ))

<<records>>

<<functions>>
```

The `Host` header is required in HTTP/1.1 so every request will have at least
one header. Because `erlang:decode_packet` requires differentiating request
line versus headers, we have to do a little wrangling.

What's common to both steps is that they

1. need to concat the new data with what's in the buffer, and
1. respond to `'more` and `'error` responses from `erlang:decode_packet`.

The only other response type is `'ok` which is something I can pattern match
on. So, the question is how do I determine whether or not we need to give
`'http` or `'httph` to `erlang:decode_packet`?

Let's try pattern matching on the request state's request record. If I haven't
processed the request line yet, the request `method` will be `'undefined`. I
initially thought that this would be function level pattern matching, but the
implementations are the same otherwise so I'll do this in a case statement and
match on any type of response from `erlang:decode_packet`.

Therefore, the request parser will initially

1. determine whether to give `erlang:decode_packet` `'http` or `'httph`, and
1. concatenate the new data with buffered data.

```{name="functions"}
<<request-parser>>
```
```{name="exports"}
(request-parser 2)
```
```{.lisp name="request-parser"}
(defun request-parser [data state]
  (let ((position (case (request-method (request-state-request state))
                    ('undefined 'http)
                    (_ 'httph)))
        (new-data (erlang:iolist_to_binary (list data (request-state-data state))))
        ; Storing the request record avoids verbose calls later in the
        ; function.
        (request (request-state-request state)))

    <<Match against decode_packet return values.>>
  ))
```

The `position` variable will now contain the atom that I need for the first
argument, so now I can treat all the responses equally in one case statement.

###### Match against decode_packet return values.
```{.lisp name="Match against decode_packet return values."}
(case (erlang:decode_packet position new-data '())
  ; Any error is just returned.
  ((= (tuple 'error _) error)
    error)

  ; If `erlang:decode_packet` needs more data, just store it in the buffer and
  ; ask for more data, too.
  ((tuple 'more _)
    (tuple 'more (set-request-state state data new-data)))

  ; If we get back `'http_eoh` the request has been fully processed (I'm not
  ; concerned about the body right now).
  ((tuple 'ok 'http_eoh _)
    (tuple 'ok request))

  <<Handle a request line.>>
  <<Handle a request header.>>
  )
```

The pattern matching for the request line and headers gets kind of hairy, so
I'll enumerate those separately. The structures nest around three levels, so
check the `erlang` man page (`erl -man erlang`) and focus.

I might have the entirety of the request all at once, but
`erlang:decode_packet` doesn't recurse, so for both types of responses I will
need to do so.

###### Handle a request line.
```{.lisp name="Handle a request line."}
((tuple 'ok (tuple 'http_request method (tuple 'abs_path uri) version) rest)
  (request-parser rest
                  (set-request-state state data #B()
                                           request (set-request request
                                                                method method
                                                                uri uri
                                                                version version))))
```

###### Handle a request header.
```{.lisp name="Handle a request header."}
((tuple 'ok (tuple 'http_header _ field _ value) rest)
  (request-parser rest
                  (set-request-state state
                                     data #B()
                                     request (set-request request
                                                          headers (maps:put field
                                                                            value
                                                                            (request-headers request))))))
```

Why is my code so ugly? :( I think I'm doing the lispy indentation, but it's
too deep or maybe my names are too long. Maybe `reqstate` and `req` are
preferable.

Anyway, let's get back to the server. Now that I'm able to successfully parse a
request, the rest is easy. It's just a reimplementation of the [simplest
server](simple.knot.md).



# Starting a server.

The server will need to maintain some kind of state and the `serve` function
will initialize it.

```{.lisp name="records"}
(defrecord server-state
  name
  port
  num-acceptors
  responder
  acceptors)
```

Many of the fields in the server state are input parameters, so the user of the
function could easily track them. However, it costs us very little to maintain
them and it might be convenient for them.

```{.markdown name="api"}
# serve

Arguments:
- **process-name**: The server process will be registered with this name.
- **port-num**: The port on which to serve; 0 will be for any available port.
- **num-acceptors**: The number of processes to use to accept incoming
  connections (max clients).
- **responder**: A function that accepts two arguments: an open socket and a
  request record.

Returns:
- **port number**: If you gave `0`, then you might need to know this.
```

```{.lisp name="exports"}
(serve 4)
```
```{.lisp name="functions"}
<<serve>>
```
```{.lisp name="serve"}
(defun serve [process-name port-num num-acceptors responder]
  (let* (((tuple 'ok listen-socket) (gen_tcp:listen port-num '(binary #(active false))))
         ; If the user passes in 0 for the port number it will be dynamically
         ; assigned, so we need to inspect it.
         ((tuple 'ok actual-port) (inet:port listen-socket))
         (initial-state (make-server-state name process-name
                                           port actual-port
                                           num-acceptors num-acceptors
                                           responder responder
                                           acceptors (list))))
    ; Server spawned here.
    (erlang:register process-name (spawn (lambda [] (server listen-socket initial-state))))
    ; Acceptor processes started here.
    (start-acceptors process-name
                     listen-socket
                     responder
                     num-acceptors)
    actual-port))
```



# Server process.

The server process will be notified when acceptors start and it can be queried
for the PIDs of the running acceptors. There's no real use for this at the
moment. In [Joe Armstrong's web server][], he has the acceptor processes send a
message to the server process to notify it when they have started so that he
can enforce the max clients. (I'm just going to use an acceptor pool like the
example server in `gen_tcp`, though.)

[Joe Armstrong's web server]: https://www.sics.se/~joe/tutorials/web_server/web_server.html

```{name="functions"}
<<server process>>
```
```{.lisp name="server process"}
(defun server [socket state]
  "A process that provides information about running acceptors."

  (receive
    ((tuple 'acceptor-started pid)
      (let* ((new-acceptors (cons pid (server-state-acceptors state)))
             (new-state (set-server-state-acceptors state new-acceptors)))
      (server socket new-state)))

    ((tuple 'acceptors pid)
      (! pid (tuple 'ok (server-state-acceptors state)))
      (server socket state))

    (other
      (lfe_io:format "Server ~s got an unknown message: ~p~n"
                     (list (map-get state 'name) other))
      (server socket state))))
```



# Acceptor process.

The acceptor will accept incoming connections and delegate to `http-handler`
for the request/response cylcle.

```{name="functions"}
<<acceptor process>>
```
```{.lisp name="acceptor process"}
(defun start-acceptors
  ([_ _ _ 0] 'ok)
  ([server listen-socket responder num]
    (spawn (lambda []
      (! server (tuple 'acceptor-started (self)))
      (acceptor server listen-socket responder)))
    (start-acceptors server listen-socket responder (- num 1))))

(defun acceptor [server listen-socket responder]
  (let (((tuple 'ok accepted-socket) (gen_tcp:accept listen-socket)))
    (http-handler accepted-socket responder)
    (gen_tcp:close accepted-socket)
    ; This is where the acceptor becomes available for another request.
    (acceptor server listen-socket responder)))
```



# HTTP handler.

The `http-handler` takes an accepted socket and receives its data by setting
`#(active once)`, processing a chunk, and then looping. I think this is where
one would implement checks to prevent the server from being flooded. I'm not
going to do anything like that right now.

I've done a little testing and when I hit this server with a browser, the
entire request comes in as one chunk. The `gen_tcp` and `erlang:decode_packet`
assumes that this isn't necessarily the case. Maybe if I set up a test with a
file upload I would see different behavior. Anyway, here we go:

```{name="functions"}
<<http handler>>
```
```{.lisp name="http handler"}
(defun http-handler [accepted-socket responder]
  (http-handler accepted-socket responder (make-request-state)))

(defun http-handler [accepted-socket responder parser-state]
  (inet:setopts accepted-socket '(#(active once)))
  (receive
    ((tuple 'tcp accepted-socket data)
      (case (request-parser data parser-state)
        ((tuple 'more new-state)
          (http-handler accepted-socket responder new-state))
        ((tuple 'ok request)
          (funcall responder accepted-socket request))))

    (other
      (lfe_io:format "Unhandled socket message (~p).~n" (list other)))))
```



# Testing the server.

That's a lot of code and I haven't tested any of it yet. I'll need to create a
simple client for that.

I'm wondering if the first matched `(tuple 'tcp accepted-socket data)` always
contains the entire request.  What if the body has a file?

I sent a bunch of data (held down paste from lipsum.com) and it sent it all in
one chunk. I wonder if it has to do with the options given to the socket...

```{name="functions"}
<<client>>
```
```{.lisp name="exports"}
(client 2)
```
```{.lisp name="client"}
(defun client [port message]
  (let (((tuple 'ok connection) (gen_tcp:connect "localhost" port '(binary #(active false)))))
    (gen_tcp:send connection message)
    (let ((response (gen_tcp:recv connection 0)))
      ;(gen_tcp:close connection)
      response)))
```



Since the `serve` function takes a function for a responder, I can create the
responses to the tests here. I'm going to have it return a `200 OK` with some
text for the URI and user agent.

```{.lisp name="tests"}
(defun simple-responder [socket request]
  (let (((match-request uri uri headers headers) request))
    (gen_tcp:send socket
      (io_lib:format
        "HTTP/1.1 200 OK\r\nServer: myweb\r\n\r\nHello. I can't serve ~s now. Your user agent is ~s."
        (list uri (map-get headers 'User-Agent))))))
```

And now I'll test something like a normal browser request.

```{.lisp name="tests"}
(defun browserish_test []
  (let* ((port (myweb:serve 'browserish_test 0 1 #'simple-responder/2)))
    (assertEqual
      (tuple 'ok #B("HTTP/1.1 200 OK\r\nServer: myweb\r\n\r\nHello. I can't serve /foobar?a=b now. Your user agent is a long string of crap."))
      (myweb:client port #B("GET /foobar?a=b HTTP/1.1\r\nHost: example.com\r\nUser-Agent: a long string of crap\r\n\r\n")))))
```
