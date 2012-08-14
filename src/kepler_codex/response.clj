(ns kepler-codex.response
    "This module response maps for ring.")


(defn html
    ([body] (html body {}))
    ([body extra]
        (merge
            {:status 200
             :headers
                {"Content-Type" "text/html"}
             :body body}
            extra)))


(def html-ok html)


(defn html-forbidden [body] (html body {:status 403}))


(defn html-bad-request [body] (html body {:status 400}))


(defn html-not-found [body] (html body {:status 404}))


(defn redirect
    ([url] (redirect url {}))
    ([url extra]
        (merge
            {:status 302
             :headers {"Location" url}}
            extra)))
