(defmodule myserver-sup
  (export
    (start-link 2)
    (init 1)
    (spawn-children 2)
  ))

(defun print [message]
  (lfe_io:format message '()))

(defun print [message data]
  (lfe_io:format message data))
(defun start-link [implementation-module server-name]
  "Starts a server using `implementation-module` as the myserver callback module."
  (let (((tuple 'ok pid) (supervisor:start_link (tuple 'local server-name)
                                                (MODULE)
                                                (list (self) implementation-module server-name))))
    (receive
      ((tuple 'port port)
        (tuple 'ok pid port)))))
(defun init
  ([(list caller-pid implementation-module server-name)]
    (let* (((tuple 'ok port-num num-acceptors)
            (call implementation-module 'server-init server-name))

           ((tuple 'ok listen-socket)
            (gen_tcp:listen port-num '(binary #(active false))))

           ((tuple 'ok actual-port) (inet:port listen-socket)))

      (! caller-pid (tuple 'port actual-port))

      (spawn_link (MODULE) 'spawn-children (list server-name num-acceptors))
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
    )))
(defun spawn-children
  ([_ 0] 'ok)
  ([server-name num]
    ; No arguments are given here, but they will get the listen socket from the
    ; child spec given in `init/2`.
    (supervisor:start_child server-name '())
    (spawn-children server-name (- num 1))))