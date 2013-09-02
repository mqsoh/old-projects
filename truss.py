import base64
import cgi
import functools
import math
import mimetypes
import os
import re
import ssl
import tempfile
import wsgiref.simple_server
import wsgiref.util

import markup


_username = 'foo'
_password = 'bar'


def application(env, start_response, root=None):
    """A WSGI handler for this site.

    Args:
        env: A WSGI environment dictionary.
        start_response: A callable to send the initial response.
        root: The root directory from we we'll serve this site. I'm adding this
            because the os.getcwd() seems to switch to / when the request comes
            in.

    Yields:
        Strings of HTML.
    """

    # Short circuit; request credentials if the global _username or _password
    # are set.
    if not authorized(env.get('HTTP_AUTHORIZATION'), _username, _password):
        for line in serve_credential_request(start_response):
            yield line
        return

    uri = env['PATH_INFO']
    directory = os.path.join(root.rstrip('/'), uri.lstrip('/'))

    # Short circuit; handle file uploads.
    if env['REQUEST_METHOD'] == 'POST':
        for line in receive_files(start_response, uri, directory, env['wsgi.input']):
            yield line
        return

    # Short circuit; serve files.
    if os.path.isfile(directory):
        for line in serve_file(start_response, directory):
            yield line
        return

    start_response('200 OK', [('Content-Type', 'text/html')])

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


def make_server(port=8000):
    server = wsgiref.simple_server.make_server('', port, functools.partial(application, root=os.getcwd()))
    server.socket = ssl.wrap_socket(server.socket,
                                    server_side=True,
                                    certfile=os.path.join(os.path.dirname(__file__), 'cert.pem'))
    return server


def authorized(header, username, password):
    """Whether or not the provided header contains the proper credentials.

    Any argument can be None. If username or password are falsy then we're open
    to the world and this will return true.
    """

    if not username or not password:
        return True

    if not header:
        return False

    if not header.startswith('Basic '):
        return False

    given_username, given_password = base64.b64decode(header[len('Basic '):]).split(':')
    if not username == given_username or not password == given_password:
        return False

    return True


def serve_credential_request(start_response):
    start_response('401 Not Authorized', [('WWW-Authenticate', 'Basic realm="truss"')])
    yield 'You must provide valid credentials.'


def serve_file(start_response, file):
    content_type, _ = mimetypes.guess_type(file)

    # We'll assume that the file is a text file if the size is small.
    if not content_type and os.path.getsize(file) < 500000:
        content_type = 'text/plain'

    if not content_type:
        content_type = 'application/octet-stream'

    start_response('200 OK', [('Content-Type', content_type)])
    for chunk in wsgiref.util.FileWrapper(open(file)):
        yield chunk


def receive_files(start_response, current_uri, current_directory, input_file):
    """Receives POSTed files.

    Args:
        start_response: So that we can issue a redirect.
        current_uri: The URI of the current page. This is where the user will
            be redirected once the files are uploaded.
        current_directory: The target location for the file uploads.
        input_file: The wsgi.input file handler.
    """

    def download_file(input_file, separator):
        """Returns file meta data and whether or not there's more content.

        This function has a side effect of writing the contents to a
        temporary file on disk.

        Args:
            input_file: The wsgi.input.
            separator: Since the separator will have been read to determine
                the end of a file, this must be provided.

        Returns:
            A tuple of file information and a boolean indicating whether or
            not there is more content. File information is a dictionary
            with the following keys: 'file name', 'content type',
            'temporary file name'.

                ({'file name': 'foo.txt',
                  'content type': 'text/plain',
                  'temporary file name': '/tmp/asdfASDF'},
                 True)
        """

        filename = re.match(r'.*filename="(?P<fn>[^"]+)".*', input_file.readline().strip()).group('fn')
        content_type = input_file.readline().strip().replace('Content-Type: ', '')
        _ = input_file.readline().strip()
        temp_filename = tempfile.mktemp(prefix='truss-upload-')

        meta = {
            'file name': filename,
            'content type': content_type,
            'temporary file name': temp_filename,
        }

        with open(temp_filename, 'w') as f:
            while True:
                line = input_file.readline()
                if '{}--\r\n'.format(separator.strip()) == line:
                    return (meta, False)
                if separator == line:
                    return (meta, True)

                f.write(line)


    def download_files(input_file):
        """Returns a list of file information for content we downloaded.
        See 'download_file'."""

        separator = input_file.readline()
        file_information = []

        while True:
            fi, more = download_file(input_file, separator)
            file_information.append(fi)
            if not more:
                break

        return file_information

    for info in download_files(input_file):
        os.rename(info['temporary file name'],
                  os.path.join(current_directory, info['file name']))

    start_response('302 Found', [('Location', current_uri)])
    yield 'Redirecting you to {}.'.format(current_uri)


def uri_to_breadcrumbs(uri):
    """Converts a uri to a list of tuple of names and URIs.

    >>> uri_to_breadcrumbs('foo/bar')
    [('foo', 'foo'), ('bar', 'foo/bar')]

    >>> uri_to_breadcrumbs('/foo/bar')
    [('top', '/'), ('foo', '/foo'), ('bar', '/foo/bar')]
    """

    crumbs = []
    components = uri.rstrip(os.path.sep).split(os.path.sep)

    for ii, component in enumerate(components):
        subpath = os.path.sep.join(components[0:ii] + [component])
        crumbs.append((component or '.', subpath or '/'))

    return crumbs


if __name__ == '__main__':
    server = make_server()
    print('Serving...')
    server.serve_forever()
