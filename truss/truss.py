"""Truss is a bit more than a SimpleHTTPServer.

It uses SSL, basic HTTP auth, and supports file uploading. Its purpose is to
facilitate development in the cloud.

Usage:
    truss myuser mypassword 8000
    truss -d /path/to/docroot -h myhost myuser mypassword 8000
"""

import argparse
import datetime
import multiprocessing
import re
import sys

import core


def main():
    config = parse_args(sys.argv[1:])
    lifetime = config.pop('lifetime')

    server = multiprocessing.Process(target=core.make_server(**config).serve_forever)
    server.start()

    print((
        'Serving directory {docroot} at {hostname}:{port} for {lifetime} '
        'seconds with credentials {username}:{password}.'
    ).format(lifetime=lifetime, **config))

    start_time = datetime.datetime.now()
    delta = datetime.timedelta(seconds=lifetime)
    while True:
        now = datetime.datetime.now()
        if now - start_time > delta:
            server.terminate()
            sys.exit('Time expired; shutting down.')


def parse_args(args):
    """Converts arguments to program configuration.

    Args:
        args: Probably sys.argv[1:].

    Returns:
        A dictionary with the following keys: username, password, host, port,
        and docroot.
    """

    def lifetime_type(input):
        """Converts a human readable time input to seconds. The input is an
        integer with an optional trailing multiple: 'h', 'm', or 'd'."""

        multiple = 1
        last_char = input[-1:]
        if last_char == 'h':
            multiple = 60 * 60
        elif last_char == 'm':
            multiple = 60
        elif last_char == 'd':
            multiple = 24 * 60 * 60

        integer = int(re.sub('[^0-9]+', '', input))

        return integer * multiple

    parser = argparse.ArgumentParser(description=sys.modules[__name__].__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('-n', '--hostname', default='', help=(
        'When creating the server, you can specify a host. I never use this.'))
    parser.add_argument('-d', '--docroot', default='.', help=(
        'The path to serve; defaults to the current directory.'))
    parser.add_argument('-l', '--lifetime', default='5m', type=lifetime_type, help=(
        'The length of time to leave the server running; defaults to "5m". '
        'Examples of valid values are: 300 (300 seconds), 5m (five minutes), 3h '
        '(three hours), and 1d (one day).'
    ))
    parser.add_argument('username', help='The accepted user name for basic HTTP auth.')
    parser.add_argument('password', help='The accepted password for basic HTTP auth.')
    parser.add_argument('port', type=int, help='The port to which we are binding.')

    return vars(parser.parse_args(args))
