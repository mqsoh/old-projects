(ns kepler-codex.view
    "This module renders all sorts of pages and fragments."

    (:require
        [clojure.string :as string])

    (:use
        [clojure.java.io :only [resource]])

    (:import
        (org.stringtemplate.v4 STGroupString)))


(declare render skel)


(defn home
    [user-is-admin]
    (skel "Home"
        (render {
            :template-name "home"
            :user_is_admin user-is-admin})))


(defn error
    [message]
    (skel "Error"
        (render {
            :template-name "error"
            :message message})))


(defn message
    [title message]
    (skel title (render {
        :template-name "message"
        :title title
        :message message})))


(defn simple
    "Render a template with no parameters."

    [template-name]
    (render {:template-name template-name}))


(defn skel
    "Render the "
    [title body]
    (let [body-class (string/replace
                        (string/lower-case title)
                        #"[^\w]+"
                        "-")]
        (render {
            :template-name "skel"
            :title title
            :body_class body-class
            :page body})))


(defn skel-simple
    [title template-name]
    (skel "title" (simple template-name)))


(defn render
    "Renders a template. 'Params' is a map that must have :template defined.
    The rest are template parameters that you'll negotiate between yourself and
    that template."
    [params]

    (let [template-group (:template-group params)
          template-name (:template-name params)
          template-file (str "templates/" (or template-group template-name) ".st")

          contents (slurp (resource template-file))
          group (STGroupString. "What is sourceName for?" contents \{ \})
          template (.getInstanceOf group template-name)]

        (doseq [[k v] (dissoc params :template-name :template-group)]
            (.add template (name k) v))

        (.render template)))
