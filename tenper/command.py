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


def _tmux_option(name, cast=str, default=None):
    """Returns the value of a tmux option or the value of default."""

    from . import core
    _, output = core.run('{tmux_command} show-options -g -t {session_name}')
    return _tmux_option_parser(name, output, cast, default)


def _tmux_option_parser(name, output, cast, default):
    """Returns the value from tmux's option output or the value of default."""

    for line in output.splitlines():
        parts = line.split()
        if parts[0] == name:
            return cast(' '.join(parts[1:]))

    return default


def _tmux_window_option(name, cast=str, default=None):
    """Returns the value of a tmux window option or the value of default."""

    from . import core
    _, output = core.run('{tmux_command} show-window-options -g -t {session_name}')
    return _tmux_option_parser(name, output, cast, default)


def completions():
    """Returns a space-separated list of available commands an arguments. This
    is appropriate for the zsh completions."""

    from . import core

    args = ['list', 'edit', 'rebuild', 'delete']

    if os.path.exists(core.configured('config_path')):
        for f in os.listdir(core.configured('config_path')):
            if f.endswith('.yml'):
                args.append(f[0:-4])

    print(' '.join(args))


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
        core.run('{editor} {config_file_name}', interactive=True)


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

    # Short circuit; confirm that the environment configuration exists.
    if not core.configured('session_name'):
        print('The \'{}\' environment doesn\'t exist.'.format(env))
        return

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

    status_left_length = _tmux_option('status-left-length', cast=int, default=0)
    if len(core.configured('session_name')) > status_left_length:
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

    base_window_index = _tmux_option('base-index', cast=int, default=0)
    base_pane_index = _tmux_window_option('pane-base-index',
                                          cast=int,
                                          default=0)

    for window_index, window in enumerate(core.configured('windows', [])):
        with core.run_context(window_index=base_window_index+window_index):
            core.run(('{tmux_command} new-window -d -k -t '
                      '{session_name}:{window_index} -n {window_name}'),
                     window_name=window.get('name', 'No Name'))

            # TODO(mason): This is so ugly. My abstractions have completely
            # fallen apart. I may want to rewrite all this again with: 1. a
            # mind to *testing* it, for crying out loud, and 2. making it less
            # complicated.
            #
            # The run context with augmented local configuration is good. It
            # lets us have clear commands without a lot of .format calls.
            # However, you can see it's not solved ideally because we still
            # need to add a bunch of context in most places.
            core.run(('{tmux_command} send-keys -t '
                      '{session_name}:{window_index} {cd_command} ENTER'),
                     cd_command='cd {}'.format(core.configured('project_root')))

            for pane_index, pane in enumerate(window.get('panes', [])):
                with core.run_context(pane_index=base_pane_index+pane_index,
                                      previous_pane_index=base_pane_index+pane_index-1):
                    if pane_index != 0:
                        core.run(('{tmux_command} split-window -t '
                                  '{session_name}:{window_index}.{previous_pane_index}'))

                    if core.configured('virtualenv_configured'):
                        core.run(('{tmux_command} send-keys -t '
                                  '{session_name}:{window_index}.{pane_index} '
                                  '{source_virtualenv} ENTER'),
                                 source_virtualenv='source $TENPER_VIRTUALENV')

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

    core.run(('{tmux_command} select-window -t '
              '{session_name}:{base_window_index}'),
             base_window_index=base_window_index)

    core.run(('{tmux_command} select-pane -t '
              '{session_name}:{base_window_index}.{base_pane_index}'),
             base_window_index=base_window_index,
             base_pane_index=base_pane_index)

    _attach_or_switch()
