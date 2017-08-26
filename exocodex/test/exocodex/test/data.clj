(ns exocodex.test.data
    (:use
        [clojure.test]
        [clojure.java.io :only [resource]]
        [clojure.pprint]

        [datomic.api :only [q db] :as d])

    (:require
        [clojure.string :as string]
        [exocodex.data :as data]
        [clojure.core.cache :as cache]))


(def uri "datomic:mem://exocodex.test.data")


(deftest earth-exists
    (let [earth (ffirst (data/query uri
                    '[:find ?name
                      :where
                          [?planet :name ?name]
                          [?planet :name/stellar "Sun"]
                          [?planet :name/letter "a"]]))]

        (is (= "Earth" earth))))


(deftest sun-exists
    (let [sun (ffirst (data/query uri
                        '[:find ?name
                          :where
                              [?star :name ?name]
                              [?star :loc/distance _]
                              [?star :name/stellar "Sun"]]))]

        (is (= "Sun" sun))))


(deftest lazy-retrieval
    (let [entities (data/query-entities uri
                        '[:find ?thing
                          :where
                              [?thing :name "Earth"]
                              [?thing :name/stellar]
                              [?thing :name/letter]])
          earth (first entities)]

        (is (= (type entities) clojure.lang.LazySeq))
        (is (= (:name/stellar earth) "Sun"))
        (is (= (:name/letter earth) "a"))))


(deftest full-data-set
    (data/data-refresher uri
        (resource "databases/exoplanets-sample.csv")
        (resource "databases/keplercandidates-sample.csv"))

    (let [candidate (data/query-entity uri '[:find ?p :where [?p :name "KOI 469.01"]])]
        (is (= (:name candidate) "KOI 469.01"))
        (is (= (:status candidate) :status/candidate)))

    (let [exoplanet (data/query-entity uri '[:find ?p :where [?p :name "Gliese 876c"]])]
        (is (= (:name exoplanet) "Gliese 876c"))
        (is (= (:mass/earth exoplanet) 260.0))))


(deftest confirmed-exoplanets
    (is (= 790 (count (data/get-confirmed-exoplanets uri)))))


(deftest candidates
    (is (= 2924 (count (data/get-candidates uri)))))
