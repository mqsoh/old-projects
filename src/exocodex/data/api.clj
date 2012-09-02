(ns exocodex.data.api
    "This module is an interface to the NASA Exoplanets Archive API. It's got
    some configuration, lists for the data columns we need, and a mapping to
    normalize the data to our internal format."

    (:use
        [clojure.pprint :only [pprint]]
    )

    (:require
        [clojure.string :as string]
        [clj-http.client :as http-client]
    ))


(defn simple-template
    "Renders a simple template.

    Args:
        template -- A string that contains things that match the search regex
                    in 'replacements'.
        replacements -- A vector of vectors of regex and replacement strings, for example

                            [[#\"%foo%\" \"A foo.\"]
                             [#\"%bar%\" \"Drink responsibly.\"]]
    Returns:
        A string."
    [template replacements]

    (reduce
        (fn [acc [s r]]
            (string/replace acc s r))
        template
        replacements))


(defn parse-table
    "A naive CSV parser. This assumes that the first line is of column names
    (they're converted to keywords). It also assumes that a split on ',' is
    sufficient. Returns a vector of maps with column names in the first row as
    keywords."
    [result-string]

    ; If we don't set a high limit on the split any trailing, empty columns are
    ; excluded.
    (let [split-csv-line (fn [line]
                             (map #(if (not-empty %) % nil)
                                  (string/split line #"," Integer/MAX_VALUE)))

          lines (string/split-lines result-string)
          columns (map keyword (split-csv-line (first lines)))
          data (map split-csv-line (rest lines))]

        (map (fn [line] (zipmap columns line)) data)))


(defn normalize
    "A normalization is a map of our internal name to a transformation
    definition of the source data. The value in the map is a vector. The first
    item is a conversion function for type casting. The second item is the
    keyword of a column in the source data.

    If it's a vector instead of a keyword, the type conversion function will be
    passed the instance's values as a vector.

    To concatenate some strings:

        [(partial reduce str) [:a :b :c]]

    To join them with a space:

        [(partial clojure.string/join \" \") [:a :b :c]]

    Example:

        ; instance
        {:a \"John\"
         :b \"Mason\"
         :c \"Staugler\"
         :d \"33\"}

        ; normalization
        {:full-name [(partial string/join \" \") [:a :b :c]]
         :first [str :a]
         :middle [str :b]
         :last [str :c]
         :age [read-string :d]}

        ; normalized instance
        {:full-name \"John Mason Staugler\"
         :first \"John\"
         :middle \"Mason\"
         :last \"Staugler\"
         :age 33}"
    [normalization instance]

    (letfn [
        (builder [acc [normal-key [type-fn keys]]]
                 (if-let [new-val (if (vector? keys)
                                      (map instance keys)
                                      (keys instance))]

                     (assoc acc normal-key (type-fn new-val))

                     acc))]

        ; The seq is necessary for the destructuring in the builder.
        (reduce builder {} (seq normalization))))


(def exoplanet-columns [
    ; Host star name.
    :pl_hostname
    ; Planet letter.
    :pl_letter
    ; Stellar distance.
    :st_dist
    :st_disterr
    ; Galactic longitude.
    :st_glon
    ; Galactic latitude.
    :st_glat
    ; Orbital distance.
    :pl_orbsmax
    :pl_orbsmaxerr1
    :pl_orbsmaxerr2
    ; Earth mass multiple.
    :pl_masse
    :pl_masseerr1
    :pl_masseerr2
    ; Earth radius mulitple.
    :pl_rade
    :pl_radeerr1
    :pl_radeerr2
    ; Jupiter mass multiple.
    :pl_massj
    :pl_massjerr1
    :pl_massjerr2
    ; Jupiter radius multiple.
    :pl_radj
    :pl_radjerr1
    :pl_radjerr2
])


(def exoplanet-normalization {
    :name
    [(partial reduce str) [:pl_hostname :pl_letter]]

    :name/stellar
    [str :pl_hostname]

    :name/letter
    [str :pl_letter]

    :loc/distance
    [read-string :st_dist]

    :loc/distance-error
    [read-string :st_disterr]

    :loc/latitude
    [read-string :st_glat]

    :loc/longitude
    [read-string :st_glon]

    :loc/orbit
    [read-string :pl_orbsmax]

    :loc/orbit-error-high
    [read-string :pl_orbsmaxerr1]

    :loc/orbit-error-low
    [read-string :pl_orbsmaxerr2]

    :mass/earth
    [read-string :pl_masse]

    :mass/earth-error-high
    [read-string :pl_masseerr1]

    :mass/earth-error-low
    [read-string :pl_masseerr2]

    :radius/earth
    [read-string :pl_rade]

    :radius/earth-error-high
    [read-string :pl_radeerr1]

    :radius/earth-error-low
    [read-string :pl_radeerr2]

    :mass/jupiter
    [read-string :pl_massj]

    :mass/jupiter-error-high
    [read-string :pl_massjerr1]

    :mass/jupiter-error-low
    [read-string :pl_massjnerr2]

    :radius/jupiter
    [read-string :pl_radj]

    :radius/jupiter-error-high
    [read-string :pl_radjerr1]

    :radius/jupiter-error-low
    [read-string :pl_radjnerr2]
})


(def candidate-columns [
    ; Kepler Object of Interest (i.e. KOI 718.01) all entries guaranteed to
    ; have this.
    :kepoi_name
    ; Galactic longitude.
    :glon
    ; Galactic longitude.
    :glat
    ; Orbital distance of the planet (AU).
    :sma
    ; Orbital distance uncertainty.
    :smaunc
    ; Earth radius multiple.
    :prad
    ; Earth radius multiple uncertainty.
    :pradunc
    ; Status: CANDIDATE, CANDIDATE-FOP, or FALSE POSITIVE.
    :kepoi_type
])


(def candidate-normalization {
    :name
    [str :kepoi_name]

    :loc/longitude
    [read-string :glon]

    :loc/latitude
    [read-string :glat]

    :loc/orbit
    [read-string :sma]

    :loc/orbit-error-high
    [read-string :smaunc]

    ; In the confirmed exoplanets, the lower uncertainty is a negative number.
    ; In the candidates table, there's only one value for uncertainty and...I
    ; think...that it's an range.
    :loc/orbit-error-low
    [(fn [v] (* -1 (read-string v))) :smaunc]

    :radius/earth
    [read-string :prad]

    :radius/earth-error-high
    [read-string :pradunc]

    :radius/earth-error-low
    [(fn [v] (* -1 (read-string v))) :pradunc]

    :status
    [(fn [v]
         (cond
            (= v "CANDIDATE")
            :status/candidate

            (= v "CANDIDATE-FOP")
            :status/studied

            (= v "CONFIRMED")
            :status/confirmed

            (= v "FALSE POSITIVE")
            :status/rejected))

     :kepoi_type]
})


(def url-template
    "I want to define the columns required by the two tables we'll query as a
    vector so we'll use this as a template to generate the URLs."
    (str "http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI"
         "?table=!table!&select=!columns!"))


(def exoplanet-url
    "URL for the data we need from the exoplanets table."

    (simple-template url-template [
        [#"!table!" "exoplanets"]
        [#"!columns!" (string/join "," (map name exoplanet-columns))]
    ]))


(def candidate-url
    "URL for the data we need from the Kepler candidates table."

    (simple-template url-template [
        [#"!table!" "keplercandidates"]
        [#"!columns!" (string/join "," (map name candidate-columns))]
    ]))


(def normalize-exoplanet
    "Normalize a confirmed exoplanet."
    (partial normalize exoplanet-normalization))


(def normalize-candidate
    "Normalizae an exoplanet candidate."
    (partial normalize candidate-normalization))


(defn fetch-exoplanets
    "Makes the HTTP request to get the exoplanets.

    Args:
        file -- Optional; if provided, load data from a file without making an
                http request."
    [& [file]]

    (let [response (if file
                       (slurp file)
                       (:body (http-client/get exoplanet-url)))]

        (map normalize-exoplanet (parse-table response))))


(defn fetch-candidates
    "Makes the HTTP request to get the exoplanets.

    Args:
    [& [file]]
        file -- Optional; if provided, load data from a file without making an
                http request."
    [& [file]]

    (let [response (if file
                       (slurp file)
                       (:body (http-client/get candidate-url)))]

    (map normalize-candidate (parse-table response))))
