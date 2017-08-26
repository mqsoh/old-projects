apiauth
=======

A library to create valid API requests by a client and to verify API requests
on the server. This library just handles the nonce and MAC generation, so

- The client is responsible for acquiring the shared secret (the *key*) and
  the key ID.

- The server will need to `prevent replay attacks`_.


Documentation
=============

`Documentation is available at Read the Docs`_


Rationale
=========
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



.. _Documentation is available at Read the Docs: https://apiauth.readthedocs.org/en/latest/
.. _macauthlib: https://github.com/mozilla-services/macauthlib
.. _Eran Hammer: http://hueniverse.com/author/eran/
.. _his reasons are really interesting: http://hueniverse.com/2012/07/oauth-2-0-and-the-road-to-hell/
.. _a spec that was simple enough that I thought I could write code against it: https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01
.. _prevent replay attacks: https://tools.ietf.org/html/draft-ietf-oauth-v2-http-mac-01#section-4.1
