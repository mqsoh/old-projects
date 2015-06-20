(defmodule myserver-implementation
  (export all))

(defun print [message]
  (lfe_io:format message '()))

(defun print [message data]
  (lfe_io:format message data))

(defun server-init
  (['my-server-name]
    (tuple 'ok 0 1)))
(defun acceptor-init
  (['my-server-name port-number 1]
    (tuple 'ok 'my-state)))
(defun handle
  (['my-server-name socket 'my-state]
    (inet:setopts socket '(#(active once)))
    (receive
      ((tuple 'tcp socket "Are you alive?")
        (gen_tcp:send socket "I am alive."))
      (after 1000))))

(defun server_test []
  (let* (((tuple 'ok pid port) (myserver:start-link 'myserver-implementation 'my-server-name))
         ((tuple 'ok connection) (gen_tcp:connect "localhost" port '(list))))

    (gen_tcp:send connection "Are you alive?")

    (receive
      ((tuple 'tcp connection "I am alive."))
      (after 1000))

    (gen_tcp:close connection)))