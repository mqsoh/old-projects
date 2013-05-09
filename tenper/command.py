import os
import shutil
import sys

from . import config


def _attach_or_switch():
    from . import core

    if os.getenv('TMUX'):
        core.run('{tmux_command} switch-client -t {session_name}')
    else:
        core.run('{tmux_command} attach-session -t {session_name}')


def _confirm_virtualenv(env):
    """Makes sure we have a virtualenv installed for env."""
    from . import core

    directory = core.configured('virtualenv_path')

    if (core.configured('virtualenv_configured') and
        directory and
        not os.path.exists(directory)):

        print(
            'We have a virtualenv confirgured for {}. Building...'.format(env))

        core.run((
            'virtualenv -p {virtualenv_python_binary} '
            '{virtualenv_use_site_packages} {virtualenv_path}'))


def _query_base_indeces():
    """Checks the tmux options for the base index for windows and panes.

    Returns:
        (base_window_index, base_pane_index)
    """
    from . import core

    base_index = 0
    _, output = core.run('{tmux_command} show-options -g -t {session_name}')
    for line in output.split('\n'):
        if 'base-index' in line:
            base_index = int(line.replace('base-index ', ''))
            break

    pane_base_index = 0
    _, output = core.run('{tmux_command} show-window-options -g -t {session_name}')
    for line in output.split('\n'):
        if 'pane-base-index' in line:
            pane_base_index = int(line.replace('pane-base-index ', ''))

    return (base_index, pane_base_index)


def _remove_virtualenv(env):
    """Deletes a possibly extant virtualenv and rebuild it."""
    from . import core

    # Short circuit; no virtualenv configured.
    if not core.configured('virtualenv_configured'):
        print('No virtualenv configured for {}.'.format(env))
        return

    directory = core.configured('virtualenv_path')

    if not os.path.exists(directory):
        print('No virtualenv created for {}.'.format(env))
        return

    response = core.user_input(
        'Are you sure you want to delete {}? '.format(directory))

    if response.strip() in ['yes', 'YES', 'y', 'Y']:
        shutil.rmtree(directory)
        print('Deleted {}.'.format(directory))
    else:
        print('Skipping.')


def delete(env):
    """Removes an environment configuration and extant virtualenv."""
    from . import core

    _remove_virtualenv(env)

    file_name = core.configured('config_file_name')
    directory = os.path.dirname(file_name)

    if os.path.exists(file_name):
        print('Removing environment config at {}'.format(file_name))
        os.remove(file_name)
    else:
        print('No tenper config for {}.'.format(env))

    # Clean up the .tenper directory is it's no longer used.
    try:
        os.rmdir(os.path.dirname(core.configured('config_file_name')))
        print('Cleaned up tenper config directory at {}.'.format(directory))
    except OSError:
        pass


def edit(env):
    """Edit (or create) an environment's configuration."""
    from . import core

    config_file_name = core.configured('config_file_name')

    if not os.path.exists(config_file_name):
        config.create(config_file_name, env)

    with core.run_context():
        core.run('{editor} {config_file_name}')


def list():
    """Prints the configuration files in {config_path} to stdout."""
    from . import core

    directory = core.configured('config_path')

    # Short circuit; no configuration.
    if not os.path.exists(directory):
        print(core.configured_string(
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


def start(env):
    from . import core
    print('Starting {}'.format(env))
    _confirm_virtualenv(env)

    # Short circuit; prexisting session.
    ok, _ = core.run('{tmux_command} has-session -t {session_name}')
    if ok:
        core.user_input('This session already exists. Press any key to reattach.')
        _attach_or_switch()
        return

    core.run('{tmux_command} new-session -d -s {session_name}')
    core.run('{tmux_command} set-option -t {session_name} default-path {project_root}')
    core.run('{tmux_command} set-option -t {session_name} status-left-length ' +
                str(len(core.configured('session_name'))))

    if core.configured('virtualenv_configured'):
        core.run(('{tmux_command} set-environment -t {session_name} '
                  'TENPER_VIRTUALENV {virtualenv_path}/bin/activate'))

    if core.configured('environment'):
        for k, v in core.configured('environment').items():
            with core.run_context(key=k, value=os.path.expandvars(v)):
                core.run(('{tmux_command} set-environment -t {session_name} '
                          '{key} {value}'))

    base_window_index, base_pane_index = _query_base_indeces()

    for window_index, window in enumerate(core.configured('windows', [])):
        with core.run_context(window_index=base_window_index+window_index):
            core.run(('{tmux_command} new-window -d -k -t '
                      '{session_name}:{window_index} -n {window_name}'),
                     window_name=window.get('name', 'No Name'))

            for pane_index, pane in enumerate(window.get('panes', [])):
                with core.run_context(pane_index=base_pane_index+pane_index,
                                      previous_pane_index=base_pane_index+pane_index-1):
                    if pane_index != 0:
                        core.run(('{tmux_command} split-window -t '
                                  '{session_name}:{window_index}.{previous_pane_index}'))

                    if core.configured('virtualenv_configured'):
                        core.run(('{tmux_command} send-keys -t '
                                  '{session_name}:{window_index}.{pane_index} '
                                  'source {virtualenv_path}/bin/activate '
                                  'ENTER'))

                    # It might be an empty command.
                    if pane:
                        core.run(('{tmux_command} send-keys -t '
                                  '{session_name}:{window_index}.{pane_index} '
                                  '{pane_command} ENTER'),
                                 pane_command=pane)

            if window.get('layout'):
                core.run(('{tmux_command} select-layout -t '
                          '{session_name}:{window_index} {layout}'),
                         layout=window['layout'])

            core.run(('{tmux_command} select-pane -t '
                      '{session_name}:{window_index}.{base_pane_index}'),
                     base_pane_index=base_pane_index)

    _attach_or_switch()
