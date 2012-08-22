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


(def ^:dynamic config (atom (read-string (slurp "config.clj"))))


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
        (pages/login-postback (:admin-identity @config) request)

        #"^/admin$"
        (pages/admin request)

        #".*"
        (pages/not-found))]

    ; I don't want to force the handlers to maintain the session if they're
    ; uninterested in it. If the response doesn't have a :session, we'll
    ; default to the request's :session.
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
        (wrap-session {:cookie-name (:session-name @config)})
        (wrap-resource "resources")
        (wrap-file-info)
        (log)))


(defn -main
    [& args]

    (if (not (empty? args))
        (let [config-file (first args)]
            (swap! config merge (read-string (slurp config-file)))))

    (run-jetty #'app (:jetty @config)))
