============================
django-s3-etag-collectstatic
============================

Overrides the core collectstatic command to compare the S3 etag and the md5 of
the local file.


Installation
============

This contains very little code, so I wont even bother submitting it to the PyPi. ::

    pip install git+git://github.com/mqsoh/django-s3-etag-collectstatic.git


In your Django settings::

    INSTALLED_APPS = (
        ...
        'django_s3_etag_collectstatic',
        ...
    )

You also need to follow the django-storages_ installation instructions. These
are the pertinent settings for this library.::

    DEFAULT_FILE_STORAGE = 'storages.backends.s3boto.S3BotoStorage'
    STATICFILES_STORAGE = DEFAULT_FILE_STORAGE


Rationale
=========

Heroku does something with the modified times of files during deployments. The
collectstatic management command uses the modified time to determine if a file
needs to be deleted (before being replaced). When you use the s3boto backend
with django-storages_ it often fails to see changes (the output of collecstatic
on Heroku will say 0 files copied).

Other people have had the opposite problem, where the Heroku deployment always
copies everything:

- `A stackoverflow question with someone having the opposite problem from me.`_
- `A Django snippet that prevents pointless copying.`_
- `A library wrapping the aforesaid Django snippet.`_


Solution
========

This app will override the core collecstatic command and prefer the etag and
md5 comparison to anything else.


License (ISAC)
==============
Copyright (c) 2013, Mason Staugler

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.


.. _django-storages: http://django-storages.readthedocs.org/en/latest/
.. _A stackoverflow question with someone having the opposite problem from me.: http://stackoverflow.com/questions/14417322/django-collectstatic-from-heroku-pushes-to-s3-everytime
.. _A Django snippet that prevents pointless copying._: http://djangosnippets.org/snippets/2889/
.. _A library wrapping the aforesaid Django snippet._: https://github.com/AGoodId/django-s3-collectstatic
