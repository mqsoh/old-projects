# Parsing HTTP requests.

The parsing of HTTP requests from an accepted socket required a surprising
amount of exposition on my part, so I'm making it its own document. From this I
will generate one module with one function.

    (http-request:run accepted-socket) -> request

    request = (map 'method method
                   'uri string
                   'get map
                   'version (tuple integer integer)
                   'headers map)

    method = 'GET
           | 'HEAD
           | 'POST
           | 'PUT
           | 'DELETE
           | 'TRACE
           | 'OPTIONS
           | 'CONNECT
           | 'PATCH

The `'get` map will contain query parameters.

The keys in the `'headers` map are of the `HttpField` type definitions in [the
`erlang:decode_packet` man page][]. There's a set of headers that will be an
atom. If it's not in that list it's a string or binary. (That's kind of
strange. Cowboy normalizes the headers to lowercase strings. I may do the same,
but maybe not.)

[the `erlang:decode_packet` man page]: http://www.erlang.org/doc/man/erlang.html#decode_packet-3

In the tests I've done with a browser, all the requests were delivered in one
chunk from from the socket. However, the API for `gen_tcp` and
`erlang:decode_packet` assume that data will be given in segments. For that
reason, I'll need to maintain a state:

    (tuple buffer needs request)

    buffer = binary

    needs = 'request-line
          | 'headers
          | 'body


The `needs` represents which stage the parsing is at. In previous iterations I
inspected the `request` that has been built so far to determine this
implicitly. When I add the parsing of the body of the request I needed additional
context since I may or may not be passing off the buffered content off to
`erlang:decode_packet`.

Here are the stubs for the module and the unit tests module.

###### file:src/http-request.lfe
```{.lisp name="file:src/http-request.lfe"}
(defmodule http-request
  (export all))

<<API>>
<<Run implementation>>
<<Parser implementation>>
<<URL decoding>>
<<URL params>>
```

###### file:test/http-request-tests.lfe
```{.lisp name="file:test/http-request-tests.lfe"}
(defmodule http-request-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

<<Tests>>
```



# API

The public interface for this module is `(http-request:run accepted-socket)`.

```{.lisp name="API"}
(defun run [socket]
  ; Initialize the parser state and request map.
  (run socket (map 'buffer #B()
                   'needs 'request-line
                   'request 'nil)))
```

It only initializes a request state and delegates to the `Run implementation`.



# Run implementation

I can get the `run` implementation out of the way immediately. However, since
it uses an open socket and delegates to the `Parser implementation` it is not
easily tested right now. It's a simple loop, though.

```{.lisp name="Run implementation"}
(defun run [socket state]
  (inet:setopts socket '(#(active once)))
  (receive
    ((tuple 'tcp socket data)
      (case (parse data state)

        ((tuple 'more new-state)
          ; Loop for more data.
          (run socket new-state))

        ((tuple 'ok request)
          ; Return parsed request.
          request)))))
```

It seems self-evident now that I've done it, but when I first looked at this
code, I found it difficult to resolve the segmented data from the socket with
the segmented input to decode_packet with the varying return values. I wish I
would have written more exposition as I was working it out but the answer is to
maintain the `'more` / `'ok` contract established by `erlang:decode_packet`.

This has established the signature of the `parse` function that I will need to
write.

    (parse data state) -> (tuple 'ok request) | (tuple 'more new-state)



# Tests

Let's start with some tests. Except for some of the debugging macros, eunit
works in LFE without any changes. (Anything using the FILE macro raises an
error; I think I saw this acknowleged by Robert Virding in the newsgroup.)

The order of arguments to the assertions in the tests will be: the expected
value and then the function call. Also, you may notice repetition in my tests;
I always do that because I don't want any abstractions in my tests. It
might save on typing or file size, but I don't want to have to check the return
values of abstracted functions.

This is the bare minimum of a successful request and we would expect the http
request parser to return an `ok` response.

```{.lisp name="Tests"}
(defun bare_minimum_request_test []
  (assertEqual
    (tuple 'ok (map 'method 'GET
                    'uri "/foobar"
                    'get (map "a" "b")
                    'version (tuple 1 1)
                    'headers (map 'Host "example.com")))

    (http-request:parse
     #B("GET /foobar?a=b HTTP/1.1\r\nHost: example.com\r\n\r\n")
     (map 'buffer #B()
          'needs 'request-line
          'request (map)))))
```

When we're given only a portion of the request line, the parser will request
more data and return an updated state.

```{.lisp name="Tests"}
(defun incomplete_request_line_test []
  (assertEqual
   (tuple 'more (map 'buffer #B("GET /foobar")
                     'needs 'request-line
                     'request (map)))

   (http-request:parse #B("GET /foobar")
                       (map 'buffer #B()
                            'needs 'request-line
                            'request (map)))))
```

Similarly, given a complete request line and partial headers, the parser will
still need more data.

```{.lisp name="Tests"}
(defun incomplete_headers_test []
  (assertEqual
   (tuple 'more (map 'buffer #B("Host: ex")
                     'needs 'headers
                     'request (map 'method 'GET
                                   'uri "/foobar"
                                   'get (map "a" "b")
                                   'version (tuple 1 1)
                                   'headers (map))))

   (http-request:parse #B("GET /foobar?a=b HTTP/1.1\r\nHost: ex")
                       (map 'buffer #B()
                            'needs 'request-line
                            'request (map)))))
```

An HTTP request body is demarcated by an empty line, so I want to test this
boundary. In this test the `Host` header is only followed by one line break. It
seems to me that `erlang:decode_packet` could return the header since it's
followed by a line break. It doesn't. It waits until either the next header
starts or the headers end and the body starts.

```{.lisp name="Tests"}
(defun header_boundary_test []
  (assertEqual
   (tuple 'more (map 'buffer #B("Host: example.com\r\n")
                     'needs 'headers
                     'request (map 'method 'GET
                                   'uri "/foobar"
                                   'get (map "a" "b")
                                   'version (tuple 1 1)
                                   'headers (map))))

   (http-request:parse #B("GET /foobar?a=b HTTP/1.1\r\nHost: example.com\r\n")
                       (map 'buffer #B()
                            'needs 'request-line
                            'request (map)))))
```

When I originally wrote this I thought I wouldn't need to support a request
body because the game I intended to write wouldn't need it. However, big
surprise, I wouldn't mind getting the HTTP body. 😏

I ran a test with a curl command. In the LFE shell:

    > (set (tuple 'ok listener) (gen_tcp:listen 9000 '(#(active false))))
    #(ok #Port<0.629>)
    ; In another shell, I ran:
    ; curl -Ss -D - -d foo=bar -X POST 'localhost:9000'
    > (set (tuple 'ok accepted) (gen_tcp:accept listener))
    #(ok #Port<0.630>)
    > (set (tuple 'ok data) (gen_tcp:recv accepted 0))
    #(ok
      "POST / HTTP/1.1\r\nUser-Agent: curl/7.37.1\r\nHost: localhost:9000\r\nAccept: */*\r\nContent-Length: 7\r\nContent-Type: application/x-www-form-urlencoded\r\n\r\nfoo=bar")
    > (gen_tcp:close accepted)

The `Content-Length` header is used to indicate the number of bytes in the
response body. That's how I'll know if the entire request has been processed.
I'll also convert the body to a map if `Content-Type:
application/x-www-form-urlencoded` header is set. Here are the tests for this
behavior.

```{.lisp name="Tests"}
(defun incomplete_body_test []
  (assertEqual
   (tuple 'more (map 'buffer #B("foo")
                     'needs 'body
                     'request (map 'method 'POST
                                   'uri "/foobar"
                                   'get (map)
                                   'version #(1 1)
                                   'headers (map 'Content-Length "7"))))

   (http-request:parse #B("Content-Length: 7\r\n\r\nfoo")
                       (map 'buffer #B()
                            'needs 'headers
                            'request (map 'method 'POST
                                          'uri "/foobar"
                                          'get (map)
                                          'version #(1 1)
                                          'headers (map))))))

(defun complete_body_test []
  (assertEqual
   (tuple 'ok (map 'method 'POST
                   'uri "/foobar"
                   'get (map)
                   'version #(1 1)
                   'headers (map 'Content-Length "7")
                   'body #B("foo=bar")))

   (http-request:parse #B("Content-Length: 7\r\n\r\nfoo=bar")
                       (map 'buffer #B()
                            'needs 'headers
                            'request (map 'method 'POST
                                          'uri "/foobar"
                                          'get (map)
                                          'version #(1 1)
                                          'headers (map))))))

(defun form_test []
  (assertEqual
   (tuple 'ok (map 'method 'POST
                   'uri "/foobar"
                   'get (map)
                   'version #(1 1)
                   'headers (map 'Content-Length "7"
                                 'Content-Type "application/x-www-form-urlencoded")
                   'body #B("foo=bar")
                   'post (map "foo" "bar")))

   (http-request:parse #B("Content-Type: application/x-www-form-urlencoded\r\n\r\nfoo=bar")
                       (map 'buffer #B()
                            'needs 'headers
                            'request (map 'method 'POST
                                          'uri "/foobar"
                                          'get (map)
                                          'version #(1 1)
                                          'headers (map 'Content-Length "7"))))))
```



# Parser implementation

The parser will need to

- concatenate the buffer with the new data,
- determine whether it's processing the request line, headers, or body, and
- return `'ok` or `'more` responses.

Regardless of what stage the parser is in we'll have to concatenate data, so
I'll use `parse` to do that and delegate to `parse-stage`.

```{.lisp name="Parser implementation"}
(defun parse
  ([data (= state (map 'buffer buffer))]
    (let ((new-data (erlang:iolist_to_binary (list buffer data))))

      (parse-stage new-data
                   (map-set state 'buffer #B())))))
```

`parse-stage` can then use function level pattern matching to distinguish the
steps we need to take. When more data is expected, we need to return the
`(tuple 'more new-state)`. If more data is leftover after this stage, I need to
recurse.

The `defun` macro in LFE takes two different forms. The simplest is:

    (defun foo [arg]
      (io:format "~p~n" (list arg)))

There is also a pattern matching form that looks like:

    (defun foo
      (['ok]
        (io:format "OK~n"))
      ([arg]
        (io:format "~p~n" (list arg))))

Each of the subsections of `parse-stage` use the pattern matching definition.

```{.lisp name="Parser implementation"}
(defun parse-stage

  <<Parse request line>>
  <<Parse headers>>
  <<Parse body>>
  )
```



## Parse request line

```{.lisp name="Parse request line"}
([data (= state (map 'needs 'request-line))]
  (case (erlang:decode_packet 'http data '())

    ; Need more data for the request line.
    ((tuple 'more _)
      (tuple 'more (map-set state 'buffer data)))

    ; Found the request line. Recurse to check for headers.
    ((tuple 'ok (tuple 'http_request method (tuple 'abs_path abs_path) version) rest)
      (let* (((tuple uri get) (case (string:tokens abs_path "?")
                                    ((list uri get-string)
                                     (tuple uri (url-params get-string)))
                                    ((list uri)
                                     (tuple uri (map)))
                                    (_
                                     (tuple abs_path (map)))))
             (new-request (map 'method method
                               'uri uri
                               'get get
                               'version version
                               'headers (map)))
             (new-state (map-set state
                                 'needs 'headers
                                 'request new-request)))

        (parse-stage rest new-state)))))
```

The request line parsing only needs to convert the structure returned by
`erlang:decode_packet` into my request map. The `'headers` are initialized so
that the next section can assume a preexisting map.



## Parse headers

```{.lisp name="Parse headers"}
([data (= state (map 'needs 'headers
                     'request (= request (map 'headers headers))))]
  (case (erlang:decode_packet 'httph data '())

    ; Need more data to complete this header.
    ((tuple 'more _)
      (tuple 'more (map-set state 'buffer data)))

    ; Found a header. Recurse to check for more headers.
    ((tuple 'ok (tuple 'http_header _ field _ value) rest)
      ; Must use maps:put because field is parameterized.
      (let* ((new-headers (maps:put field value headers))
             (new-request (map-set request 'headers new-headers))
             (new-state (map-set state 'request new-request)))

        (parse-stage rest new-state)))

    ; Found the end of the headers. Recurse to check for the body.
    ((tuple 'ok 'http_eoh rest)
      (let ((new-state (map-set state
                                'needs 'body)))

        (parse-stage rest new-state)))))
```

Headers are returned one at a time and I get an `'http_eoh` when I've reached
the end of the headers at which point I need to recurse to expect the body.



## Parse body

```{.lisp name="Parse body"}
([data (= state (map 'needs 'body
                     'request (= request
                                 (map 'headers headers))))]
  (case (maps:get 'Content-Length headers "0")
    ; An empty body.
    ("0"
      (tuple 'ok request))

    (body-length-string
      (let ((body-length (erlang:list_to_integer body-length-string))
            (data-length (erlang:byte_size data)))
        (case (< data-length body-length)
          ; Need more data in the body.
          ('true
            (tuple 'more (map-set state 'buffer data)))

          ; Body complete.
          ('false
            (case (maps:get 'Content-Type headers 'nil)
              ; Convert body to a map.
              ("application/x-www-form-urlencoded"
                (tuple 'ok (map-set request
                                    'body data
                                    'post (url-params data))))

              ; Only store the body.
              (_
                (tuple 'ok (map-set request
                                    'body data))))))))))
```

The body is the terminating condition even if it's empty. I expect a
`Content-Length` header with the number of bytes in the body. Once the data is
greater than or equal to the number of bytes in the header, the request is
returned. This means it's technically possible to return a body that is longer
than the header. Since my goal is to process correct data correctly, this is
fine.

If the request has `Content-Type: application/x-www-form-urlencoded`, the body
will be converted to a map and stored in the request's `'post`. The
`url-params` function needs to be implemented now, but first I will implement
URL decoding.



# URL decoding

I need to decode the encoded characters as described in section 2.2 of
[RFC1738][]. I could extract the code from some other library, but, I kind of
want to implement this off the spec. 

[RFC1738]: http://tools.ietf.org/html/rfc1738

To decode the URL parameters, I need to

- split up the key value pairs (by splitting first on `&` and then on `=`),
- decoded the keys and values, and
- convert them to a map.

First I want to decode strings per [RFC1738][]. The unsafe characters are
characters outside US-ASCII, control characters, space, and the following: <,
>, ", #, %, {, }, |, \, ^, ~, [, ], \`.

In the `reserved-characters` below, I used uppercase letters for the hex values
because both cases are supported.

(The unsafe backslash isn't a normal string because there's [a bug in the LFE
tokenizer][] that breaks on `"\\"`. As a workaround, I used a list with the
literal character: `'(#\\)`.)

[a bug in the LFE tokenizer]: https://groups.google.com/forum/#!topic/lisp-flavoured-erlang/RM_rIM7eUe4

```{.lisp name="Tests"}
(defun url_decode_test []
  (let ((unsafe-characters
         '(#("\v"    #B("%0B")) ; Vertical tab, a sample control character.
           #(" "     #B("+"))
           #("<"     #B("%3c"))
           #(">"     #B("%3e"))
           #("\""    #B("%22"))
           #("#"     #B("%23"))
           #("%"     #B("%25"))
           #("{"     #B("%7b"))
           #("}"     #B("%7d"))
           #("|"     #B("%7c"))
           #((#\\)   #B("%5c"))
           #("^"     #B("%5e"))
           #("~"     #B("%7e"))
           #("["     #B("%5b"))
           #("]"     #B("%5d"))
           #("`"     #B("%60"))))

        (reserved-characters
         '(#(";" #B("%3B"))
           #("/" #B("%2F"))
           #("?" #B("%3F"))
           #(":" #B("%3A"))
           #("@" #B("%40"))
           #("=" #B("%3D"))
           #("&" #B("%26"))))

        (test-char (match-lambda ([(tuple literal encoded)]
                                  (assertEqual literal (http-request:url-decode encoded))))))

    (lists:map test-char unsafe-characters)
    (lists:map test-char reserved-characters)))

(defun url_decode_unicode_test []
 (assertEqual "😔" (http-request:url-decode (binary ("😔" utf8)))))
```

This is the idiomatic Erlang list processing routine. An accumulator is
initialized in `url-decode 1` and `url-decode 2` has the terminating and
fallback cases. The important stuff is handled in `Match characters` below.

```{.lisp name="URL decoding"}
(defun url-decode [encoded-binary]
  (url-decode encoded-binary (list)))

(defun url-decode
  ([(binary) acc]
   (let* ((reversed (lists:reverse acc))
          (binaried (erlang:list_to_binary reversed)))
    (unicode:characters_to_list binaried)))

  <<Match characters>>

  ([(binary char (rest binary)) acc]
    (url-decode rest (cons char acc))))
```

## Match characters

Spaces are easy. If I get a `+`, emit a space.

```{.lisp name="Match characters"}
([(binary #\+ (rest binary)) acc]
 (url-decode rest (cons #\  acc)))
```

When I get a percent encoded character, the next two characters are hex digits.
They only need to be converted to an integer. Unicode values like `😔` end up as
a sequence of characters in the bitstring and are encoded as a sequence of
percent-encoded characters.

    > (binary ("😔" utf8))
    #B(240 159 152 148)

```{.lisp name="Match characters"}
([(binary #\% high low (rest binary)) acc]
 (let (((tuple 'ok (list char) '()) (io_lib:fread "~16u" (list high low))))
  (url-decode rest (cons char acc))))
```

## Working with binaries.

The `url-decode` function takes a bitstring, however, the accumulator is a
list. In the terminating condition, the accumulator is reversed as usual and
then converted back into a bitstring. This is because I couldn't find an easy
way to cons or to reverse binaries. However, in hind sight, it doesn't seem
necessary to do that. The input and accumulator don't have to be the same type.

I also return a unicode string instead of the processed bitstring, so this code
assumes UTF-8 bitstring input.



# URL params

Now that I can properly decode URL parameters, all that's left is to do the
conversion from `foo=bar&baz=buzz` into `(map "foo" "bar" "baz" "buzz")`. I'm
calling this the `url-params` function, though I'm not entirely happy with the
name or distinguishing it from `url-decode` since I probably will never use
`url-decode` on its own. However, let us get the job done.

```{.lisp name="URL params"}
(defun url-params [encoded-binary]
 (let-function [(split (lambda [subject sep]
                        (re:split subject sep '(#(return binary)))))]

  (maps:from_list
   (lists:map (lambda [pair]
               (let [((list key value) (split pair "="))]
                (tuple (url-decode key) (url-decode value))))
              (split encoded-binary "&")))))
```

I try to write my tests first, but I accidentally implemented this first
because it's so simple.

- Split the input on `"&"`,
- split the key value pairs on `"="`, and
- apply `url-decode` to both the key and value.

The `maps:from_list` takes a `proplist`, so the pairs are returned as a tuple
instead of a list.

```{.lisp name="Tests"}
(defun url_params_test []
 (assertEqual
  (map "foo" "bar" "baz" "buzz")
  (http-request:url-params #b("foo=bar&baz=buzz")))

 (assertEqual
  (map "mood" "😀" "😀" "happy")
  (http-request:url-params (binary ("mood=😀&😀=happy" utf8))))

 (assertEqual
  (map "good?" "100% true")
  (http-request:url-params #b("good%3f=100%25+true"))))
```
