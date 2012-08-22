(ns kepler-codex.test.data
    (:use
        [clojure.test]
        [clojure.pprint]

        [datomic.api :only [q db] :as d])

    (:require
        [kepler-codex.data :as data]))


(def uri "datomic:mem://kepler-codex.test.data")


(deftest earth-exists
    (let [earth (ffirst (data/query uri
                    '[:find ?name
                      :where
                          [?planet :exocodex/name ?name]
                          [?planet :exocodex/type :exocodex.type/planet]
                          [?planet :exocodex.stellar/name "Sol"]
                          [?planet :exocodex.planet/letter :exocodex.planet.letter/a]]))]

        (is (= "Earth" earth))))


(deftest sun-exists
    (let [sun (ffirst (data/query uri
                        '[:find ?name
                          :where
                              [?star :exocodex/name ?name]
                              [?star :exocodex/type :exocodex.type/star]
                              [?star :exocodex.stellar/name "Sol"]]))]

        (is (= "Sun" sun))))


(deftest lazy-retrieval
    (let [entities (data/query-entities uri
                        '[:find ?thing :where [?thing :exocodex/name "Earth"]])
          earth (first entities)]

        (is (= (type entities) clojure.lang.LazySeq))
        (is (= (:exocodex.stellar/name earth) "Sol"))
        (is (= (:exocodex.planet/letter earth) :exocodex.planet.letter/a))))
