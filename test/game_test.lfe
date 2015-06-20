(defmodule game_test
  (export all))

(include-lib "eunit/include/eunit.hrl")

(defun basic-auth-bypass_test []
  "Tests bypass of the middleware if any previous middleware fails."
  (let* ((input (tuple 'fail 'anything 'anything 'anything))
         (expected input)
         (actual (game:basic-auth input)))
    (assertEqual expected actual)))

(defun basic-auth-request-credentials_test []
  "Tests that the client receives a 401."
  (let* (((= mock-server (tuple _ accepted-socket _)) (myserver:make-mock-server))
         (mock-request (map 'headers (map)))
         (input (tuple 'pass 'acceptor-state accepted-socket mock-request))
         (expected-return (tuple 'fail 'acceptor-state accepted-socket mock-request))
         (expected-side-effect #B("HTTP/1.1 401 Unauthorized\r\nWWW-Authenticate: Basic realm=\"Mason's Game\"\r\n\r\n")))
    (assertEqual expected-return (game:basic-auth input))
    (assertEqual expected-side-effect (myserver:destroy-mock-server mock-server))))

(defun basic-auth-credentials-provided_test []
  "Tests that the request object is updated with user credentials."
  (lfe_io:format "ENTER the test." '())
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
    (lfe_io:format "DESTROY the mock." '())
    (myserver:destroy-mock-server mock-server)
    (lfe_io:format "RUN it." '())
    (assertEqual expected-return (game:basic-auth input))))
(defun password-hashing_test []
  (let* ((plain-text "A user's password.")
         (stored-password (game:make-storable-password plain-text)))

    (assertEqual 'true
                 (game:passwords-match stored-password plain-text))
    (assertEqual 'false
                  (game:passwords-match stored-password "Some other thing."))))
(defun authentication-bypass_test []
  (let* ((input (tuple 'fail 'anything 'anything 'anything))
         (expected input)
         (actual (game:authentication input)))
    (assertEqual expected actual)))