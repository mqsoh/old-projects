(defproject exocodex "alpha"
    :description
        "The Exocodex is a web site to visualize confirmed exoplanets or
        exoplanet candidates."

    :main exocodex.core

    :plugins [[lein-ring "0.7.0"]]

    :ring {:handler exocodex.core/app}

    :dev-dependencies [[lein-marginalia "0.7.1"]]

    :dependencies [
        [org.clojure/clojure "1.4.0"]
        [ring "1.1.0"]
        [org.openid4java/openid4java "0.9.5"]
        [org.antlr/stringtemplate "4.0.2"]
        [com.datomic/datomic-free "0.8.3397"]
        [org.clojure/core.cache "0.6.2"]
        [clj-http "0.5.3"]])
