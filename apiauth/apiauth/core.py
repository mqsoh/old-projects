import base64
import datetime
import doctest
import hashlib
import hmac
import os
import random
import time


def authorization_header(key_id, timestamp, nonce, mac, ext=''):
    """Returns a string for the 'Authorization' header.

    >>> authorization_header('keyid', '1234567890', 'asdfghjkl', 'qwertyuiop')
    'MAC id="keyid", ts="1234567890", nonce="asdfghjkl", mac="qwertyuiop"'
    >>> authorization_header('keyid', '1234567890', 'asdfghjkl', 'qwertyuiop', 'zxcvbnm')
    'MAC id="keyid", ts="1234567890", nonce="asdfghjkl", mac="qwertyuiop", ext="zxcvbnm"'
    """
    maybe_ext = ''
    if ext:
        maybe_ext = ', ext="{}"'.format(ext)

    return 'MAC id="{key_id}", ts="{ts}", nonce="{nonce}", mac="{mac}"{maybe_ext}'.format(
        key_id=key_id,
        ts=timestamp,
        nonce=nonce,
        mac=mac,
        maybe_ext=maybe_ext)


def mac(key, text, hmac_algorithm=hashlib.sha256):
    """Returns a signature.

    >>> mac('a', 'a')
    'Ps9TiOIg2p4PkZSF3rZ22L7jrsBGp3k1O0Y0GFEe5iI='
    >>> mac('a', 'a', hashlib.sha1)
    'OQLthH/yiTC18UGr+otHFoElNnM='

    Args:
        key: The key negotiated between the server and client.
        test: A 'signable_string'.
        hmac_algorithm: Defaults to hashlib.sha256. The spec also defines SHA1
            and with extensions supports others, however, I'm not sure why
            you'd want to use SHA1. My understanding is that is has flaws.

    Returns:
        A signature string.
    """

    # http://stackoverflow.com/a/1306575
    return base64.b64encode(
        hmac.new(key, msg=text, digestmod=hmac_algorithm).digest())


def nonce():
    """Generates a nonce.

    >>> len(nonce())
    40
    >>> isinstance(nonce(), basestring)
    True
    """
    # https://github.com/litl/rauth/blob/0cc0ad59d7a21ff7bee722f91c2e49db50fe80fb/rauth/session.py#L227
    return hashlib.sha1(str(random.random())).hexdigest()


def now():
    """Returns the Unix time right now.

    >>> isinstance(now(), int)
    True
    """
    return int(time.time())


def request_mac(key, timestamp, nonce, method, host, port, request_uri):
    """Returns a mac given all the required parameters."""
    return mac(key, signable_string(timestamp, nonce, method, request_uri, host, port))


def signable_string(timestamp, nonce, request_method, request_uri, host, port, ext=''):
    """Creates a signable string.

    >>> signable_string(1234567890, 'asdfghjkl', 'POST', '/foo/bar', 'api.example.com', 443)
    '1234567890\\nasdfghjkl\\nPOST\\n/foo/bar\\napi.example.com\\n443\\n\\n'
    >>> signable_string(1234567890, 'asdfghjkl', 'POST', '/foo/bar', 'api.example.com', 443, 'a,b,c')
    '1234567890\\nasdfghjkl\\nPOST\\n/foo/bar\\napi.example.com\\n443\\na,b,c\\n'

    Args:
        timestamp: An integer representing the Unix time at the time of the
            request.
        nonce: A string value.
        request_method: 'GET', 'POST', or and valid HTTP method.
        request_uri: The path portion of the URL; this contains the query string.
        host: The server name.
        port: The remote port.
        ext: Optional; Extensions to the spec. I'm not sure what would be used
            here.

    Returns:
        These values concatenated with line breaks.
    """
    # This function just defines what's required by the spec.
    # https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01#page-7
    return ''.join('{}\n'.format(s) for s in [timestamp, nonce, request_method, request_uri, host, port, ext])
