(ns exocodex.test.data
    (:use
        [clojure.test]
        [clojure.pprint]

        [datomic.api :only [q db] :as d])

    (:require
        [exocodex.data :as data]))


(def uri "datomic:mem://exocodex.test.data")


(deftest earth-exists
    (let [earth (ffirst (data/query uri
                    '[:find ?name
                      :where
                          [?planet :name ?name]
                          [?planet :name/stellar "Sol"]
                          [?planet :name/letter "a"]]))]

        (is (= "Earth" earth))))


(deftest sun-exists
    (let [sun (ffirst (data/query uri
                        '[:find ?name
                          :where
                              [?star :name ?name]
                              [?star :loc/distance _]
                              [?star :name/stellar "Sol"]]))]

        (is (= "Sol" sun))))


(deftest lazy-retrieval
    (let [entities (data/query-entities uri
                        '[:find ?thing :where [?thing :name "Earth"]])
          earth (first entities)]

        (is (= (type entities) clojure.lang.LazySeq))
        (is (= (:name/stellar earth) "Sol"))
        (is (= (:name/letter earth) "a"))))
