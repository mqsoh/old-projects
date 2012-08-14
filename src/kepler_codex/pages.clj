(ns kepler-codex.pages
    "A collection of page handlers."

    (:require
        [kepler-codex.openid :as openid]
        [kepler-codex.view :as view]
        [kepler-codex.response :as resp]))


(defn not-found []
    (resp/html-not-found
        (view/error "There's nothing here. (404)")))


(defn home
    [request]
    (resp/html-ok
        (view/skel-simple "Home" "home")))


(defn login
    [request]
    (resp/html-ok
        (view/skel-simple "Login" "login")))


(defn login-post
    "Handles an OpenID authentication request."

    [request]

    (if-let [openid-identity (get (:params request) "openid-identity")]
        ; Authenticate the user.
        (let [postback-url (str "http://" (get (:headers request) "host") "/login-postback")
              {auth-url :url, discovery :discovery} (openid/auth postback-url openid-identity)]

            (resp/redirect auth-url
                {:session
                    {:openid-discovery discovery}}))

        ; Bad request.
        (resp/html-bad-request (view/error "You must provide an 'openid-identity' parameter."))))


(defn login-postback
    "Handles an OpenID authentication response and starts an admin session if
    the verified identity is the 'admin-identity'.

    Args:
        admin-identity
            An ID that the app will consider the admin user."

    [admin-identity request]
    (let [session (:session request)
          discovery (:openid-discovery session)
          request-url (str "http://" (get (:headers request) "host") (:uri request) "?" (:query-string request))]

        (if (and discovery request-url)
            (let [verified (openid/verify request-url discovery)]
                (if (= verified admin-identity)

                    (resp/redirect
                        "/admin"
                        {:session
                            (assoc (:session request) :admin true)})

                    (resp/html-forbidden
                        (view/error "You're not authorized."))))

            (resp/html-bad-request (view/error "You've got a malformed request.")))))


(defn admin
    [request]
    (if-not (:admin (:session request))
        (resp/html-forbidden (view/error "You're not authorized."))

        (resp/html-ok (view/message "Admin" "Welcome to the admin, dood."))))
