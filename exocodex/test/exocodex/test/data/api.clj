(ns exocodex.test.data.api
    (:use
        [clojure.test]
        [clojure.java.io :only [resource]]
        [clojure.pprint]
    )

    (:require
        [clojure.string :as string]
        [exocodex.data.api :as api]
    ))


(deftest simple-template
    (is (= "foobarbaz"
           (api/simple-template
               "|a|b|!c!"
               [[#"\|a" "foo"]
                [#"\|b\|" "bar"]
                [#"!c!" "baz"]]))))


(deftest parse-table
    (let [results (api/parse-table "first,middle,last\nJohn,Mason,Staugler\nJohn,,Doe")
          me (first results)
          nobody (second results)]

        (is (= (:first me) "John"))
        (is (= (:middle me) "Mason"))
        (is (= (:last me) "Staugler"))))


(deftest normalize
    (let [instance {
            :a "John"
            :b "Mason"
            :c "Staugler"
            :d "33"
            :extra-instance "WRONG"
          }

          normalization {
            :full-name [(partial string/join " ") [:a :b :c]]
            :first [str :a]
            :middle [str :b]
            :last [str :c]
            :age [read-string :d]
            :extra-normalization [str :WRONG]
          }

          expected {
            :full-name "John Mason Staugler"
            :first "John"
            :middle "Mason"
            :last "Staugler"
            :age 33
          }]

        (is (= expected (api/normalize normalization instance)))))


(deftest fetch-exoplanets
    (let [planet (first (api/fetch-exoplanets (resource "database/exoplanets-sample.csv")))]
        (is (contains? planet :name))
        (is (contains? planet :name/stellar))
        (is (contains? planet :name/letter))))


(deftest fetch-candidates
    (let [planet (first (api/fetch-candidates (resource "database/keplercandidates-sample.csv")))]
        (is (contains? planet :status))
        (is (not (contains? planet :mass/earth)))
        (is (contains? planet :name))
        (is (.startsWith (:name planet) "KOI"))))
