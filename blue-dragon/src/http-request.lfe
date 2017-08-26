(defmodule http-request
  (export all))

(defun run [socket]
  ; Initialize the parser state and request map.
  (run socket (map 'buffer #B()
                   'needs 'request-line
                   'request 'nil)))
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
(defun parse
  ([data (= state (map 'buffer buffer))]
    (let ((new-data (erlang:iolist_to_binary (list buffer data))))

      (parse-stage new-data
                   (map-set state 'buffer #B())))))
(defun parse-stage

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
  )
(defun url-decode [encoded-binary]
  (url-decode encoded-binary (list)))

(defun url-decode
  ([(binary) acc]
   (let* ((reversed (lists:reverse acc))
          (binaried (erlang:list_to_binary reversed)))
    (unicode:characters_to_list binaried)))

  ([(binary #\+ (rest binary)) acc]
   (url-decode rest (cons #\  acc)))
  ([(binary #\% high low (rest binary)) acc]
   (let (((tuple 'ok (list char) '()) (io_lib:fread "~16u" (list high low))))
    (url-decode rest (cons char acc))))

  ([(binary char (rest binary)) acc]
    (url-decode rest (cons char acc))))
(defun url-params [encoded-binary]
 (let-function [(split (lambda [subject sep]
                        (re:split subject sep '(#(return binary)))))]

  (maps:from_list
   (lists:map (lambda [pair]
               (let [((list key value) (split pair "="))]
                (tuple (url-decode key) (url-decode value))))
              (split encoded-binary "&")))))