(ns exocodex.openid
    "A wrapper around the *openid4java* library."

    (:require
        [clojure.string :as string]
        [ring.util.codec :as codec])

    (:import
        (java.io StringWriter)
        (org.openid4java.consumer ConsumerManager)
        (org.openid4java.message ParameterList)
        (org.openid4java.message.ax FetchRequest AxMessage)))


(defonce consumer-manager (ConsumerManager.))


(defn auth
    "Returns a map with a URL to which you should redirect the user for
    authentication and an org.openid4java.discovery.DiscoveryInformation which
    you'll need to pass back to the verify function. The example stores this
    thing in the user's session. I suspect that I can store this in a global
    map or something instead of forcing the user of this function to handle
    it.

    Returns:
        {:url \"A URL.\"
         :discovery org.openid4java.discovery.DiscoveryInformation}"
    [postback-url openid-identity]

    (let [discoveries (.discover consumer-manager openid-identity)
          discovered  (.associate consumer-manager discoveries)
          auth-req    (.authenticate consumer-manager discovered postback-url)
          fetch-req   (doto (FetchRequest/createFetchRequest)
                            (.addAttribute "email" "http://schema.openid.net/contact/email" true))
          auth-url    (.getDestinationUrl (doto auth-req (.addExtension fetch-req)) true)]

        {:url auth-url
         :discovery discovered}))


(defn verify
    "Verifies user authentication.

    Args:
        request-url
            The full URL of the endpoint. It should be that same request handler as
            that specified in the postback-url for the 'auth' function, but it
            needs all the query params from the OpenID provider to properly
            authenticate.

        discovery
            The DiscoveryInformation provided in the response from the 'auth'
            function.

    Returns:
        Verified identity or nil on failure."
    [request-url discovery]

    (let [[_ query]     (string/split request-url #"\?")
          query-params  (into {}
                             (for [[_ k v] (re-seq #"([^=&]+)=([^&]+)" query)]
                                 [k (codec/form-decode v)]))
          response      (ParameterList. query-params)
          openid-ns-ax  AxMessage/OPENID_NS_AX
          verification  (.verify consumer-manager request-url response discovery)]

        ; The ID is a UrlIdentifier. We'll just return the string.
        (str (.getVerifiedId verification))))
