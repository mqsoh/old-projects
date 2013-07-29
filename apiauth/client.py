import core


def sign_request(key_id, key, method, host, port, request_uri):
    """Generates an 'Authentication' header with some sane defaults.

    >>> sign_request('keyid', 'mykey', 'POST', 'api.example.com', 443, '/foo/bar?baz=buzz') # doctest: +SKIP
    'MAC id="keyid", ts="1374996296", nonce="e4493430d227af804ef7fbac9c40d4564a133c03", mac="ntu+vZtLt98Vx2T1FpKXoPiCnhD3oJCTC6gioUlGNa0="'

    Args:
        method: A valid HTTP method, like 'GET' or 'POST'.
        host: The server name.
        request_uri: The URI of the request, including parameters.
        key_id: The key ID you negotiated with the server.
        key: The secret key you negotiated with the server.
        port: The remote port; defaults to 443.

    Returns:
        A value for the Authorization header. This will generate the timestamp,
        nonce, and mac for you.
    """

    ts = core.now()
    mynonce = core.nonce()
    mac = core.request_mac(key, ts, mynonce, method, host, port, request_uri)

    return core.authorization_header(key_id, ts, mynonce, mac)
