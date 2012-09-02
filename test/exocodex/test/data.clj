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
                        '[:find ?thing
                          :where
                              [?thing :name "Earth"]
                              [?thing :name/stellar]
                              [?thing :name/letter]])
          earth (first entities)]

        (is (= (type entities) clojure.lang.LazySeq))
        (is (= (:name/stellar earth) "Sol"))
        (is (= (:name/letter earth) "a"))))


(deftest caching
    "Does caching work like I think it does?"

    (let [mycache (cache/ttl-cache-factory {:thing :initialized} :ttl 500)
          updater (fn [] :updated)]

        (is (= (data/cache-get mycache :thing updater)
               {:thing :initialized}))

        (Thread/sleep 1000)

        (is (= (data/cache-get mycache :thing updater)
               {:thing :updated}))))
