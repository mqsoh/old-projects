(ns exocodex.data
    (:use
        [clojure.pprint]
        [clojure.java.io :only [resource]]

        [datomic.api :only [q db] :as d]
    )

    (:require
        [clojure.string :as string]
        [clojure.core.cache :as cache]
        [exocodex.data.api :as api]
    ))


(def update-frequency (* 1 60 60 1000))

(def update-log
    "I'm using this TTL cache only as a marker to indicate when data should be
    refreshed."
    (atom (cache/ttl-cache-factory {} :ttl update-frequency)))


(declare
    data-refresher)


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

    (data-refresher uri)
    (q query (db (get-connection uri))))


(defn get-entity
    "Get a record based on its ID."
    [uri id]

    (data-refresher uri)
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
    [cache-key update-function]

    (if (cache/has? @update-log cache-key)
        (cache/hit @update-log cache-key)
        (swap! update-log #(cache/miss % cache-key (update-function)))))


(defn data-refresher
    "Provides an update-function to cache-get. Cache-get, combined with the
    defined ttl in the cache, is what decides when (and how often) data is
    updated from the exoplanet archive."
    [uri & [exoplanets-file candidates-file]]

    (letfn [(add-db-id [api-entity]
                       (assoc api-entity :db/id #db/id[:db.part/user]))

            (merger [fetch-fn]
                    ; The doall is necessary here. I knew that 'map' was
                    ; generating lazy sequences; I thought the conditions for
                    ; evaluating them all were already met. I can't explain
                    ; why -- ignorance! It keeps you on your toes.
                    (doall
                        (map
                            #(d/transact (get-connection uri) [%])
                            (map add-db-id (fetch-fn)))))]

        (cache-get
            :data-recent
            (fn []
                ; This doall is also necessary.
                (doall (map merger [(if exoplanets-file
                                 (partial api/fetch-exoplanets exoplanets-file)
                                 api/fetch-exoplanets)

                             (if candidates-file
                                 (partial api/fetch-candidates candidates-file)
                                 api/fetch-candidates)]))
                true))))


(defn get-confirmed-exoplanets
    "Return a vector of all the planets that don't start with KOI, which is the
    prefix of all the Kepler Objects of Interest in the candidates table."
    [uri]

    ; I think that there must be a way to do this with the Datomic query,
    ; however, I'll review all this later. I want to get to some of the front end
    ; visualizations.
    (filter
        #(not (.startsWith (:name %) "KOI"))
        (query-entities uri '[:find ?p :where [?p :name]])))


(defn get-candidates
    "Return a list of all the Kepler objects of interest."
    [uri]

    (filter
        #(.startsWith (:name %) "KOI")
        (query-entities uri '[:find ?p :where [?p :name ?name]])))
