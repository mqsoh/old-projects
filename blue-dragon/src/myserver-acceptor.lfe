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


(defun print [message]
  (lfe_io:format message '()))

(defun print [message data]
  (lfe_io:format message data))
(defun start-link [listen-socket implementation-module server-name num-acceptors port-number]
    (gen_server:start_link (MODULE)
                           (list listen-socket
                                 implementation-module
                                 server-name
                                 num-acceptors
                                 port-number)
                           '()))
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