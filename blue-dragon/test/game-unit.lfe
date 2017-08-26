(defmodule game-unit
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun setup []
  (application:start 'game)
  (let ((dbconn (game:configured-database-connection)))
    (game:create-schema dbconn)
    dbconn))

(defun cleanup [dbconn]
  (game:destroy-schema dbconn)
  (epgsql:close dbconn))

(defun instantiator [dbconn]
  (lists:map (lambda [test]
              (io:format "Calling ~p~n" (list test))
              (funcall test dbconn))
             (list
              (lambda [_]
                (let ((request (map 'method 'GET
                                    'uri "/about"
                                    'version #(1 1)
                                    'get (map)
                                    'headers (map)))
              
                      (routes (list (tuple (map 'method 'GET 'uri "/")
                                           (list 'wrong))
                                    (tuple (map 'method 'GET 'uri "/about")
                                           (list 'right))
                                    (tuple (map)
                                           (list 'fallback)))))
              
                  (_assertEqual (tuple request (list 'right))
                                (game:route routes request))))
              (lambda [_]
                (let ((request (map 'method 'GET
                                    'uri "/burp"
                                    'version #(1 1)
                                    'get (map)
                                    'headers (map)))
              
                      (routes (list (tuple (map 'uri "/poot")
                                           (list 'wrong))
                                    (tuple (map)
                                           (list 'fallback)))))
              
                  (_assertEqual (tuple request (list 'fallback))
                                (game:route routes request))))
              (lambda [_]
                (let ((request (map 'method 'GET
                                    'uri "/user/1234"
                                    'version #(1 1)
                                    'get (map)
                                    'headers (map)))
                      (routes (list (tuple (map 'uri "/about")
                                           (list 'wrong))
                                    (tuple (map 'uri-like "^/user/(\\d+)$")
                                           (list 'matched))
                                    (tuple (map) (list 'fallback)))))
              
                  (_assertEqual (tuple request (list 'matched))
                                (game:route routes request))))
              (lambda [_]
                (let ((request (map 'method 'GET
                                    'uri "/user/1234/update"
                                    'version #(1 1)
                                    'get (map)
                                    'headers (map)))
              
                      (routes (list (tuple (map 'uri "/wrong")
                                           (list 'wrong))
                                    (tuple (map 'uri-like #("^/user/(\\d+)/([^/]+)$" (user-id action)))
                                           (list 'right))
                                    (tuple (map)
                                           (list 'fallback)))))
              
                  (_assertEqual (tuple (maps:put 'uri-params
                                                 (map 'user-id "1234" 'action "update")
                                                 request)
                                       (list 'right))
                                (game:route routes request))))
              (lambda [_]
                (list (_assertEqual 'false (game:subset? (map 'a 'A) (map 'b 'B)))
                      (_assertEqual 'false (game:subset? (map 'a 'A 'b 'B) (map 'b 'B)))
                      (_assertEqual 'false (game:subset? (map 'a 'A ) (map 'a 'not-A)))
                      (_assertEqual 'true (game:subset? (map 'b 'B) (map 'a 'A 'b 'B)))
                      (_assertEqual 'true (game:subset? (map) (map 'a 'A 'b 'B)))))
              (lambda [_]
                "Tests bypass of the middleware if any previous middleware fails."
                (let* ((input (tuple 'fail 'anything 'anything 'anything))
                       (expected input)
                       (actual (game:basic-auth input)))
                  (_assertEqual expected actual)))
              
              (lambda [_]
                "Tests that the client receives a 401."
                (let* (((= mock-server (tuple _ accepted-socket _)) (myserver:make-mock-server))
                       (mock-request (map 'headers (map)))
                       (input (tuple 'pass 'acceptor-state accepted-socket mock-request))
                       (expected-return (tuple 'fail 'acceptor-state accepted-socket mock-request))
                       (expected-side-effect #B("HTTP/1.1 401 Unauthorized\r\nWWW-Authenticate: Basic realm=\"Mason's Game\"\r\n\r\n")))
                  (list (_assertEqual expected-return (game:basic-auth input))
                        (_assertEqual expected-side-effect (myserver:destroy-mock-server mock-server)))))
              
              (lambda [_]
                "Tests that the request object is updated with user credentials."
                (let* (((= mock-server (tuple _ accepted-socket _)) (myserver:make-mock-server))
                       (mock-request (map 'headers (map 'Authorization #B("Basic c2FsbHk6c2VjcmV0"))))
                       (input (tuple 'pass 'acceptor-state accepted-socket mock-request))
                       (expected-return (tuple 'pass
                                               'acceptor-state
                                               accepted-socket
                                               (maps:put 'basic-auth
                                                         (map 'username "sally"
                                                              'password "secret")
                                                         mock-request))))
                  (myserver:destroy-mock-server mock-server)
                  (_assertEqual expected-return (game:basic-auth input))))
              (lambda [_]
                (_assertEqual 16 (string:len (game:create-salt))))
              (lambda [_]
                (list
                  (_assertEqual
                    'true
                    (game:passwords-match? (game:hash-password "dumb-salt" "secret")
                                           "secret"))
                  (_assertEqual
                    'false
                    (game:passwords-match? (game:hash-password "dumb-salt" "secret")
                                           "not right"))))
              )))

(defun game_test_ []
  (tuple 'setup
         (fun setup 0)
         (fun cleanup 1)
         (fun instantiator 1)))