import base64
import hashlib
import os

import core


def key_pair():
    """Returns a tuple of a key ID and a secret key.

    Both use os.urandom. The key is a md5sum, all upper case. The key is simply
    base64 encoded.

    Flask has a very simple suggestion for creating secret keys:
    http://flask.pocoo.org/docs/quickstart/#sessions

    >>> [len(x) for x in key_pair()]
    [32, 40]
    """
    return (hashlib.md5(str(os.urandom(30))).hexdigest().upper(),
            base64.b64encode(os.urandom(30)))


def unpack_header(header):
    """Returns a dictionary with the values in the header.

    >>> sorted(unpack_header('MAC id="keyid", ts="1234567890", nonce="asdfghjkl", mac="qwertyuiop", ext="zxcvbnm"').keys())
    ['ext', 'id', 'mac', 'nonce', 'ts']
    >>> sorted(unpack_header('MAC id="keyid", ts="1234567890", nonce="asdfghjkl", mac="qwertyuiop"').keys())
    ['id', 'mac', 'nonce', 'ts']

    Returns:
        A dictionary with the following keys: id, ts, nonce, mac, and
        optionally ext.
    """
    header = header.replace('MAC ', '').replace(' ', '').replace('"', '')
    return dict([pair.split('=', 1) for pair in header.split(',')])


def valid_request(given_timestamp, given_nonce, given_mac,
                  key, method, host, port, request_uri):
    """Whether or not the given timestamp, nonce, and mac can be recalculated.

    >>> valid_request(1234567890, 'nonce', 'aDBHxns5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 443, '/foo/bar')
    True
    >>> valid_request(1987654321, 'nonce', 'aDBHxns5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 443, '/foo/bar')
    False
    >>> valid_request(1234567890, 'badnonce', 'aDBHxns5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 443, '/foo/bar')
    False
    >>> valid_request(1234567890, 'nonce', 'BADMACs5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 443, '/foo/bar')
    False
    >>> valid_request(1234567890, 'nonce', 'aDBHxns5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 80, '/foo/bar')
    False
    >>> valid_request(1234567890, 'nonce', 'aDBHxns5jtbW2kQPD3wlyvIdyOJPlkAaY2l4oBA9Vk8=', 'mykey', 'GET', 'api.example.com', 443, '/foo/bar/')
    False
    """

    recalculated_mac = core.request_mac(key, given_timestamp, given_nonce,
                                        method, host, port, request_uri)

    return given_mac == recalculated_mac


def valid_request_header(header, key, method, host, port, request_uri):
    """Whether or not the given header can be recalculated.

    >>> valid_request_header('MAC id="keyid", ts="1374996296", nonce="e4493430d227af804ef7fbac9c40d4564a133c03", mac="ntu+vZtLt98Vx2T1FpKXoPiCnhD3oJCTC6gioUlGNa0="', 'mykey', 'POST', 'api.example.com', 443, '/foo/bar?baz=buzz')
    True
    """

    given = unpack_header(header)
    return valid_request(given.get('ts'), given.get('nonce'), given.get('mac'),
                         key, method, host, port, request_uri)
