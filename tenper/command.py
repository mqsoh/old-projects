import os
import shutil
import sys

from . import config
from . import core


def _confirm_virtualenv(env):
    """Makes sure we have a virtualenv installed for env."""

    directory = core.configured('{virtualenv_path}')

    if not os.path.exists(directory):
        print(
            'We have a virtualenv confirgured for {}. Building...'.format(env))

        core.run((
            'virtualenv -p {virtualenv_python_binary} '
            '{virtualenv_use_site_packages} {virtualenv_path}'))


def _remove_virtualenv(env):
    """Deletes a possibly extant virtualenv and rebuild it."""

    # Short circuit; no virtualenv configured.
    if not core._run_context['virtualenv_configured']:
        print('No virtualenv configured for {}.'.format(env))
        return

    directory = core.configured('{virtualenv_path}')

    if not os.path.exists(directory):
        print('No virtualenv created for {}.'.format(env))
        return

    prompt = 'Are you sure you want to delete {}? '.format(directory)
    try:
        response = raw_input(prompt)
    except (ValueError, EOFError):
        response = input(prompt)

    if resp.strip() in ['yes', 'YES', 'y', 'Y']:
        shutil.rmtree(directory)
        print('Deleted {}.'.format(directory))
    else:
        print('Skipping.')


def delete(env):
    """Removes an environment configuration and extant virtualenv."""

    _remove_virtualenv(env)

    file_name = core.configured('{config_file_name}')
    directory = os.path.dirname(file_name)

    if os.path.exists(file_name):
        print('Removing environment config at {}'.format(file_name))
        os.remove(file_name)
    else:
        print('No tenper config for {}.'.format(env))

    # Clean up the .tenper directory is it's no longer used.
    try:
        os.rmdir(os.path.dirname(core.configured('{config_file_name}')))
        print('Cleaned up tenper config directory at {}.'.format(directory))
    except OSError:
        pass


def edit(env):
    """Edit (or create) an environment's configuration."""

    config_file_name = core.configured('{config_file_name}')

    if not os.path.exists(config_file_name):
        config.create(config_file_name, env)

    with core.run_context():
        core.run('{editor} {config_file_name}')


def list():
    """Prints the configuration files in {config_path} to stdout."""

    directory = core.configured('{config_path}')

    # Short circuit; no configuration.
    if not os.path.exists(directory):
        print(core.configured(
            'You have no environments; {config_path} is empty.'))
        return

    args = [f[0:-4] for f in os.listdir(directory) if f.endswith('.yml')]

    if not args:
        print('None.')
    else:
        for yml in args:
            print(yml)


def rebuild(env):
    """Deletes a possibly extant virtualenv and rebuilds it."""

    _remove_virtualenv(env)
    _confirm_virtualenv(env)
