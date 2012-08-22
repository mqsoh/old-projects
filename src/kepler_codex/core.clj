(ns kepler-codex.core

    (:use
        [clojure.pprint :as pp :only [pprint]]
        [ring.middleware.session :only [wrap-session]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.resource :only [wrap-resource]]
        [ring.middleware.file-info :only [wrap-file-info]]
        [ring.util.response :only [resource-response]]
        [ring.adapter.jetty :only [run-jetty]]

        [kepler-codex.pages :as pages]
        [kepler-codex.response :as resp])

    (:gen-class))


(def config
    "This will probably need to be a properties file, eventually."

    {:session-name "kepler-codex-session"
     :admin-identity "https://www.google.com/accounts/o8/id?id=AItOawlS7Q-SfuFC7pRtAv6G2cEJbKsyfGb0kJU"}
     :datomic-uri "datomic:dev://localhost/kepler-codex")


(defn router
    "`condp` with some regex makes a really nice router!"

    [request]
    (let [response (condp re-find (:uri request)
        #"^/public/"
        (resource-response (:uri request))

        #"^/$"
        (pages/home request)

        #"^/login$"
        (pages/login request)

        #"^/login-post$"
        (pages/login-post request)

        #"^/login-postback$"
        (pages/login-postback (:admin-identity config) request)

        #"^/admin$"
        (pages/admin request)

        #".*"
        (pages/not-found))]

    ; I don't want to force the handlers to maintain the session if they're
    ; uninterested in it. This will default to the request's session if the
    ; response has no session.
    (merge {:session (:session request)} response)))


(defn log
    "Middleware to log all requests and responses.

    TODO: Make this dev-only."

    [handler]
    (fn [request]
        (let [response (handler request)
              content-type (get-in [:headers "Content-Type"] response)]
            (println "\n\n----------")
            (println (str "Request URI: " (:uri request)))
            (println (str "Request time: " (System/currentTimeMillis)))
            (println "==========")
            response)))


(def app
    "This is Jetty's handler and the place we add all the middleware."

    (-> router
        (wrap-params)
        (wrap-session {:cookie-name (:session-name config)})
        (wrap-resource "resources")
        (wrap-file-info)
        (log)))


(defn -main
    [port]
    (run-jetty #'app {:port (Integer. port) :join? false}))
