#!/usr/bin/python3.2
"""Tenper is a tmux wrapper with support for Python virtualenv.

It uses virtualenvwrappers conventions, so they'll work side-by-side.
"""

import argparse
import os
import subprocess
import sys


editor = os.getenv('EDITOR')
configs = os.path.join(os.path.expanduser('~'), '.tenper')
virtualenv = os.path.join(os.path.expanduser('~'), '.virtualenvs')
config_template = """session name: my session
virtualenv: my project
project root: ~/code/tenper
windows:
  - name: One
    command: ls -l
  - name: Two
    command: ls /
"""


def command_list(template, **kwargs):
    """Split a command into an array (for subprocess).

    >>> command_list('ls')
    ['ls']
    >>> command_list('ls /')
    ['ls', '/']
    >>> command_list('echo {message}', message='Hello, world.')
    ['echo', 'Hello, world.']

    Args:
        template: A string that will be split on ' ' and will have the
            remaining arguments to this function applied to the 'format' of
            each segment.

    Returns:
        A list of strings.
    """

    return [part.format(**kwargs) for part in template.split(' ')]


def run(command, **kwargs):
    """Run a command template (see command_list)."""
    return subprocess.call(command_list(command, **kwargs))


def list_envs():
    pass


def edit(env):
    pass


def rebuild(env):
    pass


def start(env):
    pass


def parse_args(args):
    """Return a function and its arguments based on 'args'.

    Args:
        args: Probably never anything but sys.argv[1:].

    Returns:
        (callable, [...arguments])
    """

    parser = argparse.ArgumentParser(description=(
        'A wrapper for tmux sessions and (optionally) virtualenv{,wrapper}. '
        'Usage:\n'
        '  tenper -l\n'
        '  tenper -e new-environment\n'
        '  tenper --rebuild-env some-env\n'
        '  tenper some-env\n'))

    parser.add_argument('--list', '-l', action='store_true', help=(
        'List the available environments.'))
    parser.add_argument('--edit', '-e', action='store_true', help=(
        'Edit (or create) a new environment.'))
    parser.add_argument('--rebuild-env', action='store_true', help=(
        'If the environment uses virtualenv, rebuild it.'))
    parser.add_argument('env', nargs='?', help=(
        'An environment name.'))

    parsed = parser.parse_args(args)

    if parsed.list:
        return (list_envs, [])

    if not parsed.env:
        raise Exception('You must provide an environment name')

    if parsed.edit:
        return (edit, [parsed.env])

    if parsed.rebuild_env:
        return (rebuild, [parsed.env])

    if parsed.env:
        return (start, [parsed.env])

    # This description of the problem is rude (stupid); maybe this interface
    # sucks.
    raise Exception((
        'You must provide an environment name and maybe a flag, too. Use -h '
        'for help'))


if __name__ == '__main__':
    f, a = parse_args(sys.argv[1:])
    f(*a)
