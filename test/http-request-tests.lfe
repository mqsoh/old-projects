(defmodule http-request-tests
  (export all))

(include-lib "eunit/include/eunit.hrl")

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
(defun incomplete_request_line_test []
  (assertEqual
   (tuple 'more (map 'buffer #B("GET /foobar")
                     'needs 'request-line
                     'request (map)))

   (http-request:parse #B("GET /foobar")
                       (map 'buffer #B()
                            'needs 'request-line
                            'request (map)))))
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