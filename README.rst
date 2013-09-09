truss
=====

Truss is a temporary, browser-based bridge to a remote machine. It will serve
and receive files over an SSL connection with basic HTTP authorization. It will
shut itself down (after five minutes, by default).

It is meant to augment my usage of `python -m SimpleHTTPSever`.


Installation
============

The package name is trussws (because 'truss' is taken by something that isn't
used in the PyPI), but the executable is 'truss'.

Globally ::

    pip install trussws

As a user ::

    pip install --user trussws

From a clone ::

    python setup.py install


Usage
=====

::

    truss -h
    truss myuser mypassword 8000
    truss myuser mypassword 8000 --hostname example.com --docroot /path/to/http-temp --lifetime 3h

Be sure to access the server with **https://**. You'll get a warning; this is
caused by the self-signed certificate.


Rationale
=========

I'm using ChromeOS as my primary development machine (at home and at work). It
comes with a shell and ssh client, but to rsync files you need to turn on
developer mode. I don't want to do that because

#. I'm a developer, but I'm not developing for ChromeOS.
#. It disables the verified boot.
#. It takes longer to boot up due to a warning about the lack of verification.
#. The warning has a big computer thing frowning at me that is annoying.

Truss is meant to facilitate development in the cloud. To get stuff off my
remote development server, I would frequently run `python -m SimpleHTTPServer`.
To get stuff on to the remote machine, I would copy and paste contents or, if
it was too big, upload the file into S3 from a browser and pull it on to the
remote machine. This came with some issues:

#. It's inconvenient to take multiple steps to get something on to a remote
   machine.
#. I would often leave the SimpleHTTPServer running acccidentally for long
   periods of time.
#. It would be nice to have some authentication to get to what I'm temporarily
   serving.
#. It would be nice to do it over SSL.


License
=======

Copyright 2013 Mason Staugler

See LICENSE; it's the MIT license.
