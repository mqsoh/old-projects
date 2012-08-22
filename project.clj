(defproject kepler-codex "alpha"
    :description
        "The Kepler Codex is a web site to visualize potential exoplanets found
        by NASA's Kepler mission."

    :main kepler-codex.core

    :plugins [[lein-ring "0.7.0"]]

    :ring {:handler kepler-codex.core/app}

    :dev-dependencies [[lein-marginalia "0.7.1"]]

    :dependencies [
        [org.clojure/clojure "1.4.0"]
        [ring "1.1.0"]
        [org.openid4java/openid4java "0.9.5"]
        [org.antlr/stringtemplate "4.0.2"]
        [com.datomic/datomic-free "0.8.3397"]])
