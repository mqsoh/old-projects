apiauth
=======

A library to create valid API requests by a client and to verify API requests
on the server. This library just handles the nonce and MAC generation, so

- The client is responsible for acquiring the shared secret (the *key*) and
  the key ID.

- The server will need to `prevent replay attacks`_.

Rationale
---------
I was looking for a library to handle the signing and validating of API
requests and found `macauthlib`_. It's good; you should probably use it. It
supports a 'variety of common object types' and mutates those objects by adding
the header for you. I'm not using it because

- I've got a bug up my butt about mutable objects even though the `macauthlib`_
  functions are idempotent.
- I haven't contributed much code to the world and `macauthlib`_ links to `a
  spec that was simple enough that I thought I could write code against it`_!
  Later I noticed that it was the last version of the OAuth 2.0 spec that `Eran
  Hammer`_ authored. He withdrew from that development and `his reasons are
  really interesting`_.



Client Usage
============

A client will have received a key ID and a key by some method outside the scope
of this library. So, the client only needs to do one thing: sign the requests
it makes.

The output of :func:`apiauth.client.sign_request` should be placed in the
**Authorization** header.

API
---
.. automodule:: apiauth.client
    :members:



Server Usage
============

The server will need to `prevent replay attacks`_ (which is outside the scope
of this library).

It will also be responsible for dispensing key IDs and secrect keys. We have
provided :func:`apiauth.server.key_pair` to help with that.

The server should expect an **Authorization** header. If it's not provided,
`the response should be a 401 and contain the WWW-Authenticate header`_.

The :func:`apiauth.server.unpack_header` function can be used to extract the
values in the header and the either of the following functions used to validate
it: :func:`apiauth.server.valid_request` or
:func:`apiauth.server.valid_request_header`.

API
---
.. automodule:: apiauth.server
    :members:



.. _macauthlib: https://github.com/mozilla-services/macauthlib
.. _Eran Hammer: http://hueniverse.com/author/eran/
.. _his reasons are really interesting: http://hueniverse.com/2012/07/oauth-2-0-and-the-road-to-hell/
.. _a spec that was simple enough that I thought I could write code against it: https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01
.. _prevent replay attacks: https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01#section-4.1
.. _the response should be a 401 and contain the WWW-Authenticate header: https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01#section-4.2
