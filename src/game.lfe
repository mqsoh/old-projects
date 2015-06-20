(defmodule game
  (export all))

(defun dsn-params [dsn]
  (let* ((pattern (list
                    "(?# This could be used with any driver, but since my application only uses"
                    "PostgresQL, that's all I'll match.)"
                    "postgres://"
                    ""
                    "(?# The username and password are optional, but required as a unit.)"
                    "(?|(?<username>[^:]+):(?<password>[^@]+)@)?"
                    ""
                    "(?# The host name is required up until the port number, if provided.)"
                    "(?<host>[^:/]+)"
                    ""
                    "(?# The port number is optional.)"
                    "(?|:(?<port>[^/]+))?"
                    ""
                    "(?# A '/' will mark the start of the database name.)"
                    "/"
                    ""
                    "(?# The rest of the string is the database name and is required.)"
                    "(?<db>.+)"
                  ))
         ((tuple 'match (list _all username password host port dbname))
          (re:run dsn pattern '(extended #(capture all list)))))

    (++ (list (tuple 'host host)
              (tuple 'port (case port ("" 5432)
                                      (_ (erlang:list_to_integer port))))
              (tuple 'database dbname))

        (case (tuple username password)
          ((tuple "" "") '())
          (_ (list (tuple 'username username)
                   (tuple 'password password)))))))

(defun connect-to-database [dsn]
  (let (((tuple 'ok dbconn) (epgsql:connect (dsn-params dsn))))
    dbconn))

(defun configured-database-connection []
  (let* (((tuple 'ok dsn) (application:get_env (MODULE) 'DATABASE_URL)))
    (connect-to-database dsn)))
(defun create-schema [dbconn]
  (let* ((sql (list
                "CREATE SCHEMA game;"
                "CREATE TABLE game.user ("
                "  name VARCHAR(100) NOT NULL,"
                "  password VARCHAR(104) NOT NULL"
                ");"
              ))
         (single-statement (string:join sql ""))
         (responses (epgsql:squery dbconn single-statement)))
    (lfe_io:format "~p~n" (list responses))))

(defun destroy-schema [dbconn]
  (epgsql:squery dbconn "DROP SCHEMA IF EXISTS game CASCADE"))

(defun rebuild-schema [dbconn]
  (destroy-schema dbconn)
  (create-schema dbconn))
(defun start [_type _args]
  (myserver:start-link (MODULE) 'game))

(defun stop [_] 'ok)
(defun server-init
  (['game]
    (lfe_io:format "game:server-init~n" '())
    (let (((tuple 'ok port-number) (application:get_env (MODULE) 'PORT))
          (number-of-acceptors 4))
      (lfe_io:format "game:server-init returning ~p~n" (list (tuple 'ok port-number number-of-acceptors)))
      (tuple 'ok port-number number-of-acceptors))))
(defun acceptor-init
  (['game port-number 4]
    (lfe_io:format "Acceptor ~p running on ~p.~n" (list (self) port-number))
    (tuple 'ok (configured-database-connection))))
(defun handle
  (['game socket dbconn]
    (let ((request (http-request:run socket)))
      (let* ((routes (list
                        
      
                        ; 404 handler.
                        (tuple (map)
                               (list (match-lambda ([(tuple _pass _dbconn socket _request)]
                                                    (gen_tcp:send socket (list
                                                      #B("HTTP/1.1 404 Not Found\r\n")
                                                      #B("Content-Type: text/plain\r\n")
                                                      #B("\r\n")
                                                      #B("Page not found.")))))))))
      
              ((tuple routed-request middleware) (game:route routes request)))
      
        ; Feed the middleware.
        (lists:foldl (lambda [fun state]
                      (funcall fun state))
                     (tuple 'pass dbconn socket routed-request)
                     middleware))
      (tuple 'close dbconn))))
(defun subset? [a b]
  (subset? a b (maps:keys a)))

(defun subset?
  ([_ _ (list)]
    'true)

  ([a b (cons key rest)]
    (case (maps:find key b)
          ('error
            'false)
          ((tuple 'ok b-value)
            (case (== b-value (maps:get key a))
              ('false
                'false)
              ('true
                (subset? a b rest)))))))
(defun route
  ([(list) request]
    (tuple request (list)))

  ([(cons (tuple request-spec middleware-functions) rest) request]
    (case (subset? (maps:remove 'uri-like request-spec)
                   request)
          ('false
            (route rest request))

          ('true
            (case (maps:get 'uri-like request-spec 'nil)
                  ('nil
                    (tuple request middleware-functions))

                  ((tuple pattern capture-group-names)
                    (case (re:run (map-get request 'uri) pattern '(#(capture all list)))
                          ('nomatch
                            (route rest request))

                          ((tuple 'match (cons _full groups))
                            (tuple (map-set request
                                            'uri-params
                                            (maps:from_list (lists:zip capture-group-names groups)))
                                    middleware-functions))))

                  (pattern
                    (case (re:run (map-get request 'uri) pattern)
                          ('nomatch
                            (route rest request))

                          ((tuple 'match _)
                            (tuple request middleware-functions)))))))))
(defun basic-auth
  ; Fail track; bypass middleware.
  ([(= (tuple 'fail _ _ _) state)]
    state)

  ([(tuple 'pass acceptor-state socket request)]
    (case (maps:get 'Authorization (map-get request 'headers) 'nil)
      ; Fail track; request credentials.
      ('nil
        (gen_tcp:send socket (list
          #B("HTTP/1.1 401 Unauthorized\r\n")
          #B("WWW-Authenticate: Basic realm=\"Mason's Game\"\r\n")
          #B("\r\n")))
        (tuple 'fail acceptor-state socket request))

      ; Pass track; add credentials to the request map.
      ((binary #\B #\a #\s #\i #\c #\  (encoded binary))
        (let* ((decoded (base64:decode encoded))
               ((list username password) (re:split decoded ":" '(#(return list))))
               (credentials (map 'username username 'password password))
               (updated-request (maps:put 'basic-auth credentials request)))
          (tuple 'pass acceptor-state socket updated-request))))))
(defun create-salt []
  "Returns 16 random, base64-encoded characters."
  (base64:encode_to_string (crypto:strong_rand_bytes 12)))
(defun hash-password [salt plain-text]
  "Returns \"<salt><hash>\"."
  (let* ((hmac (crypto:hmac 'sha512 "TODO" (list salt plain-text)))
         (encoded (base64:encode_to_string hmac)))
    (string:join (list salt encoded) "")))
(defun passwords-match? [stored-password plain-text]
  (let ((salt (string:substr stored-password 1 16)))

    (string:equal stored-password
                  (hash-password salt plain-text))))