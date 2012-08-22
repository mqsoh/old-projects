(ns kepler-codex.test.core
  (:use [kepler-codex.core])
  (:use [clojure.test]))


(defn body-has
    [response something]
    (is (.contains (:body response) something)))


(defn status-is
    [response something]
    (is (= something (:status response))))


(defn content-type-is
    [response content-type]
    (is (get-in [:header "Content-Type"] content-type)))


(deftest properly-configured
  (is (not (empty? (:session-name @config))))
  (is (not (empty? (:admin-identity @config)))))


(deftest pages
    ; Home
    (doto (app {:uri "/"})
        (status-is 200)
        (body-has "</html>"))

    ; Login.
    (doto (app {:uri "/login"})
        (status-is 200)
        (body-has "action=\"/login-post\""))

    ; Login post.
    (doto (app {:uri "/login-post"})
        (status-is 400)
        (body-has "Error"))

    ; Login postback (from the OpenID provider).
    (doto (app {:uri "/login-postback"})
        (status-is 400)
        (body-has "Error"))

    ; Admin.
    (doto (app {:uri "/admin"})
        (status-is 403)
        (body-has "not authorized"))

    ; 404.
    (doto (app {:uri "/please-never-put-anything-here"})
        (status-is 404))
    )
