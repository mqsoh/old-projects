{
    ; These options are passed to run-jetty.
    ; https://github.com/mmcgrana/ring/blob/master/ring-jetty-adapter/src/ring/adapter/jetty.clj#L61
    :jetty {:host "localhost"
            :port 3000
            :join? false}

    :session-name "exocodex-session"

    :admin-identity
        "https://www.google.com/accounts/o8/id?id=AItOawlS7Q-SfuFC7pRtAv6G2cEJbKsyfGb0kJU"

    :datomic-uri
        "datomic:free://localhost:4334/exocodex"
}
