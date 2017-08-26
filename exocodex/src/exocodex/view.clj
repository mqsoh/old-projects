(ns exocodex.view
    "This module renders all sorts of pages and fragments."

    (:require
        [clojure.string :as string]
        [hiccup.core :as hiccup]
        [hiccup.page]
    )

    (:use
        [clojure.java.io :only [resource]]
    ))


(defn skel
    "Render something in the sitewide skeleton. This function takes the
    following key value pairs.

    Args:
        :title -- The page title. A tokenized version of this will be passed to
                  the skeleton as :body_class.
        :body -- Some kind of string.
        :body-class -- Optional; defaults to the title, lower cased and spaces
                       replaced with dashes.

    Returns:
        A string of text."
    [& {title :title body :body body-class :body-class}]

    (hiccup.page/html5
        [:head
            [:title (str title " | Exocodex")]

            (hiccup.page/include-css
                "//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css"
                "//netdna.bootstrapcdn.com/bootswatch/2.1.0/cyborg/bootstrap.min.css"
            )

            (hiccup.page/include-js
                "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"
                "/public/scripts.js"
            )
        ]
        [:body
            {:class (if body-class
                        body-class
                        (string/lower-case (string/replace title #" " "-")))}

            [:div.navbar
                [:div.navbar-inner
                    [:div.container
                        [:a {:class "brand" :href "/"} "The Exocodex"]]]]

            [:div.container.page
                body]

            [:div.container.footer
                [:a {:href "/"} "Home"]]

            [:script {:type "text/javascript"}
                "if (typeof(docready) !== 'undefined') {"
                    "docready(window);"
                "}"]
        ]))


(defn home
    "The home page."
    [exoplanets candidates]

    (skel
        :title "Home"
        :body (hiccup/html
                  [:div
                      [:h1 "Confirmed exoplanets"]
                      [:table
                          [:thead
                              [:tr
                                  [:th "Name"]
                                  [:th "Earth masses"]
                                  [:th "Earth radii"]
                                  [:th "Distance from Earth (parsecs)"]
                                  [:th "Galactic longitude"]
                                  [:th "Galactic latitude"]
                                  [:th "Oribital distance from host star"]]]
                          [:tbody
                              (for [planet exoplanets]
                                   [:tr
                                       [:td (:name planet)]
                                       [:td (:mass/earth planet)]
                                       [:td (:radius/earth planet)]
                                       [:td (:loc/distance planet)]
                                       [:td (:loc/longitude planet)]
                                       [:td (:loc/latitude planet)]
                                       [:td (:loc/orbit planet)]
                                    ])]]])))

(defn error
    "Render an error page."
    [message]

    (skel :title "Error"
          :body (hiccup/html
                    [:div.error
                        [:h1 "Error"]
                        [:p message]])))


(defn message
    "Render an arbitrary message."
    [title message]

    (skel :title title
          :body (hiccup/html
                    [:div.message
                        [:h1 title]
                        [:p message]])))


(defn login
    "Renders a login form."
    []

    (skel
        :title "Login"
        :body (hiccup/html
            [:div
                (hiccup.page/include-css
                    "/public/openid/openid.css")
                (hiccup.page/include-js
                    "/public/openid/openid-jquery.js"
                    "/public/openid/openid-en.js")

                [:form {:action "/login-post" :method "get" :id "openid_form"}
                    [:fieldset

                        [:legend "Sign-in or Create New Account"]

                        [:div#openid_choice
                            [:p "Please click your account provider:"]
                            [:div#openid_btns]]

                        [:div#openid_input_area
                            [:input#openid_identity
                                {:name "openid_identity" :type "text" :value "http://"}]
                            [:input#openid_submit {:type "submit" :value "Sign-In"}]]

                        [:noscript
                            [:p
                                "OpenID is service that allows you to log-on to "
                                "many different websites using a single indentity. "
                                "Find out "
                                [:a
                                    {:href "http://openid.net/what/"}
                                    "more about OpenID"]
                                " and "
                                [:a
                                    {:href "http://openid.net/get/"}
                                    "how to get an OpenID enabled account"]
                                "."]]]]

                [:script {:type "text/javascript"}
                    "openid.img_path = \"/public/openid/\";"
                    "openid.init('openid-identity');"]])))
