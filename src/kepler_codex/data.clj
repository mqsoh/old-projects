(ns kepler-codex.data
    (:use
        [clojure.pprint]

        [datomic.api :only [q db] :as d]))


(defn init-database
    "Import the schema and fixtures for a new database. You will have checked
    for a return of false from datomic.api.create-database."
    [connection]

    (let [schema (read-string (slurp "resources/database/schema.dtm"))
          fixtures (read-string (slurp "resources/database/fixtures.dtm"))]

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
    (q query (db (get-connection uri))))


(defn get-entity
    "Get a record based on its ID."

    [uri id]
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
