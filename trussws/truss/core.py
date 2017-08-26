import base64
import cgi
import functools
import math
import mimetypes
import os
import re
import shutil
import ssl
import tempfile
import wsgiref.simple_server
import wsgiref.util

import markup


def application(env, start_response, root=None, username=None, password=None):
    """A WSGI handler for this site.

    Args:
        env: A WSGI environment dictionary.
        start_response: A callable to send the initial response.
        root: The root directory from we we'll serve this site. I'm adding this
            because the os.getcwd() seems to switch to / when the request comes
            in.
        username: The username required to access this site.
        password: The password required to access this site.

    Yields:
        Strings of HTML.
    """

    uri = env['PATH_INFO']
    file_or_directory = os.path.join(root.rstrip('/'), uri.lstrip('/'))

    # The default handler will be to serve directories.
    handler = functools.partial(serve_directory, root=root, directory=file_or_directory)

    if not authorized(env, username, password):
        # Request authorization.
        handler = serve_credential_request

    if env['REQUEST_METHOD'] == 'POST':
        # Download files from the user.
        handler = functools.partial(receive_files, root=root)

    if uri == '/favicon.ico':
        handler = functools.partial(serve_file, file=os.path.join(
            os.path.dirname(__file__),
            'files',
            'favicon.ico'))

    if os.path.isfile(file_or_directory):
        # Serve a file to the user.
        handler = functools.partial(serve_file, file=file_or_directory)

    for line in handler(env, start_response):
        yield line


def make_server(hostname, port, docroot, username, password):
    server = wsgiref.simple_server.make_server(
        hostname,
        port,
        functools.partial(application,
                          root=os.getcwd() if docroot == '.' else docroot,
                          username=username,
                          password=password))

    server.socket = ssl.wrap_socket(server.socket,
                                    server_side=True,
                                    certfile=os.path.join(os.path.dirname(__file__), 'files', 'cert.pem'))
    return server


def authorized(env, username, password):
    """Whether or not the HTTP_AUTHORIZATION header contains the proper
    credentials."""

    header = env.get('HTTP_AUTHORIZATION')

    if not username or not password:
        return False

    if not header:
        return False

    if not header.startswith('Basic '):
        return False

    given_username, given_password = base64.b64decode(header[len('Basic '):]).split(':')
    if not username == given_username or not password == given_password:
        return False

    return True


def serve_credential_request(_env, start_response):
    """Responds with a request for credentials."""

    start_response('401 Not Authorized', [('WWW-Authenticate', 'Basic realm="truss"')])
    yield 'You must provide valid credentials.'


def serve_directory(env, start_response, root, directory):
    """Renders directories, files, and an upload form to the user."""

    start_response('200 OK', [('Content-Type', 'text/html')])

    uri = env['PATH_INFO']
    yield markup.head(uri)
    yield markup.breadcrumbs(uri_to_breadcrumbs(uri))

    for current, directories, files in os.walk(directory):
        current = current[len(root):]

        for directory in directories:
            path = os.path.join(current, directory)
            yield markup.file(directory, path, 'directory')

        for file in files:
            path = os.path.join(current, file)
            yield markup.file(file, path, 'file')

        break

    yield markup.upload_form()


def serve_file(_env, start_response, file):
    """Serves a file."""

    content_type, _ = mimetypes.guess_type(file)

    # We'll assume that the file is a text file if the size is small.
    if not content_type and os.path.getsize(file) < 500000:
        content_type = 'text/plain'

    if not content_type:
        content_type = 'application/octet-stream'

    start_response('200 OK', [('Content-Type', content_type)])
    for chunk in wsgiref.util.FileWrapper(open(file)):
        yield chunk


def receive_files(env, start_response, root):
    """Receives posted files.

    Args:
        start_response: So that we can issue a redirect.
        root: The docroot of the server.
    """

    current_uri = env['PATH_INFO']
    target_directory = os.path.join(root.rstrip('/'), current_uri.lstrip('/'))
    form = cgi.FieldStorage(fp=env['wsgi.input'], environ=env)
    uploaded_files = form['uploaded-files']

    # FieldStorage always returns other FieldStorage objects. If only only file
    # was uploaded, we'll have a FieldStorage instance. If multiple files were
    # uploaded, we'll have a list of FieldStorages.
    if not isinstance(uploaded_files, list):
        uploaded_files = [uploaded_files]

    for field in uploaded_files:
        if field.file:
            with open(os.path.join(target_directory, field.filename), 'w') as target_file:
                shutil.copyfileobj(field.file, target_file)

    start_response('302 Found', [('Location', current_uri)])
    yield 'Redirecting you to {}.'.format(current_uri)


def uri_to_breadcrumbs(uri):
    """Converts a uri to a list of tuple of names and URIs.

    >>> uri_to_breadcrumbs('foo/bar')
    [('foo', 'foo'), ('bar', 'foo/bar')]

    >>> uri_to_breadcrumbs('/foo/bar')
    [('.', '/'), ('foo', '/foo'), ('bar', '/foo/bar')]
    """

    crumbs = []
    components = uri.rstrip(os.path.sep).split(os.path.sep)

    for ii, component in enumerate(components):
        subpath = os.path.sep.join(components[0:ii] + [component])
        crumbs.append((component or '.', subpath or '/'))

    return crumbs
