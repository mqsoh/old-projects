(ns exocodex.data
    (:use
        [clojure.pprint]
        [clojure.java.io :only [resource]]

        [datomic.api :only [q db] :as d])

    (:require
        [clojure.string :as string]
        [clojure.core.cache :as cache]))


(def update-frequency (* 1 60 60 1000))

(def update-log
    "I'm using this TTL cache only as a marker to indicate when data should be
    refreshed."
    (cache/ttl-cache-factory {} :ttl update-frequency))


(declare
    data-recentness-check)


(defn init-database
    "Import the schema and fixtures for a new database. You will have checked
    for a return of false from datomic.api.create-database."
    [connection]

    (let [schema (read-string (slurp (resource "database/schema.dtm")))
          fixtures (read-string (slurp (resource "database/fixtures.dtm")))]

        (doto connection
            (d/transact schema)
            (d/transact fixtures))))


(defn open-connection
    "Opens a connection to the database. If the database doesn't exist, the
    schema and fixtures will be loaded."
    [uri]

    ; create-database returns true if it already exists.
    (if (not (d/create-database uri))
        (d/connect uri)

        (let [connection (d/connect uri)]
            (init-database connection)
            connection)))


(def get-connection
    "Return a Datomic connection based on a URI."
    (memoize open-connection))


(defn query
    "Query based on a URI."
    [uri query]

    (data-recentness-check uri)
    (q query (db (get-connection uri))))


(defn get-entity
    "Get a record based on its ID."
    [uri id]

    (data-recentness-check uri)
    (-> (get-connection uri)
        (db)
        (d/entity id)))


(defn query-entities
    "Return a (lazy) seq of entities. The query must have an ID as the first
    variable."
    [uri user-query]

    (->>
        (query uri user-query)
        (map #(get-entity uri (first %1)))))


(defn query-entity
    "Return a single entity based on a query. The query must have the ID as the
    first variable."
    [uri user-query]

    (first (query-entities uri user-query)))


(defn cache-get
    "If cache-key is in the cache, return it. If not, call update-function and
    store it."
    [store cache-key update-function]

    (if (cache/has? store cache-key)
        (cache/hit store cache-key)
        (cache/miss store cache-key (update-function))))


(defn data-recentness-check
    "Checks if it's time to refresh the data from the exoplanet archive. If it
    is, it runs the update."
    [datomic-uri]
    )
