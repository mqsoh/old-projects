(ns exocodex.view
    "This module renders all sorts of pages and fragments. It acts as a Clojure
    wrapper around template requirements."

    (:require
        [clojure.string :as string])

    (:use
        [clojure.java.io :only [resource]])

    (:import
        (org.stringtemplate.v4 STGroupString)))


(defn render
    "Render a template.

    Args:
        params -- A map with :template-name defined. You can additionally
                  define :template-group. Any additional keys are added to the
                  template, negotiated between the caller of this function and
                  the template.

    Returns:
        A string of template data."
    [params]

    (let [template-group (:template-group params)
          template-name (:template-name params)
          template-file (str "templates/" (or template-group template-name) ".st")

          contents (slurp (resource template-file))
          group (STGroupString. "What is sourceName for?" contents \{ \})
          group-function (last (string/split template-name #"/"))
          template (.getInstanceOf group group-function)]

        (doseq [[k v] (dissoc params :template-name :template-group)]
            (.add template (name k) v))

        (.render template)))


(defn skel
    "Render something in the sitewide skeleton.

    The template used to render the skeleton can take the following input.

        {:title \"Page title.\"
         :body_class \"page-title\"
         :body \"A string of content.\"}

    Args:
        title -- The page title. A tokenized version of this will be passed to
                 the skeleton as :body_class.
        body -- Some kind of string.

    Returns:
        A string of text."
    [title body]

    (let [body-class (string/replace
                        (string/lower-case title)
                        #"[^\w]+"
                        "-")]
        (render {:template-name "skel"
                 :title title
                 :body_class body-class
                 :body body})))


(defn simple
    "Render a template with no parameters."

    [template-name]
    (render {:template-name template-name}))


(defn skel-simple
    "Render a template with no parameters in the sitewide skeleton."

    [title template-name]
    (skel "title" (simple template-name)))


(defn home
    [user-is-admin]
    (skel "Home"
        (render {:template-name "pages/home"
                 :user_is_admin user-is-admin})))


(defn error
    [message]
    (skel "Error"
        (render {:template-name "pages/error"
                 :message message})))


(defn message
    [title message]
    (skel title (render {:template-name "pages/message"
                         :title title
                         :message message})))
