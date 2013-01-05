#!/usr/bin/python3.2

import argparse
import collections
import os
import subprocess


editor = os.getenv('EDITOR')
configs = os.path.join(os.path.expanduser('~'), '.tenper')


def create(env_name, config_file):
    """Open an environment config in EDITOR."""

    os.makedirs(configs, exist_ok=True)
    subprocess.call([editor, config_file])


def edit(env_name, config_file):
    """Open an environment config in EDITOR."""

    if not os.path.exists(config_file):
        return create(env_name, config_file)

    subprocess.call([editor, config_file])


def show():
    """Print a list of environments to stdout."""

    print('Available tenper environments:')
    print()
    for f in os.listdir(configs):
        print('    ', f[0:-3])


def start(env_name, config_file):
    env = {}

    with open(config_file) as f:
        ast_object = compile(f.read(), config_file, mode='exec')
        exec(ast_object, env)

    session = env['session_name']
    session_arg = '-s{}'.format(session)

    # Short circuit for a previously existing session.
    has_session = subprocess.call(['tmux', 'has-session', '-t', session])
    if has_session == 0:
        print('Session already exists; attaching...')
        input()
        subprocess.call(['tmux', '-2', 'attach-session', '-t', session])
        return

    subprocess.call(['tmux', 'new-session', '-d', session_arg])

    for index, window in enumerate(env.get('windows', [])):
        window_name, command = window
        window_target = ':'.join([session, str(index)])

        if index == 0:
            subprocess.call(['tmux', 'rename-window', '-t', str(index), window_name])
        else:
            subprocess.call(['tmux', 'new-window', '-n', window_name, '-t', window_target])

        if env.get('project_root'):
            send(window_target, 'cd {}'.format(env['project_root']))

        if env.get('before_all'):
            send(window_target, env['before_all'])

        send(window_target, command)

    subprocess.call(['tmux', '-2', 'attach-session', '-t', session])


def send(window_target, command):
    run = ['tmux', 'send-keys', '-t', window_target]

    if isinstance(command, str):
        run.append(command)
    else:
        run.append(' '.join(command))

    run.append('ENTER')

    subprocess.call(run)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=(
        'A tmux and virtualenvwrapper wrapper.'
    ))
    subparsers = parser.add_subparsers()

    create_sp = subparsers.add_parser('create', help='Create a new tenper env.')
    create_sp.add_argument('env_name', help='The name of an environment.')
    create_sp.set_defaults(handler=create)

    edit_sp = subparsers.add_parser('edit', help='Edit a tenper env.')
    edit_sp.add_argument('env_name', help='The name of an environment.')
    edit_sp.set_defaults(handler=edit)

    start_sp = subparsers.add_parser('start', help='Start a tenper session.')
    start_sp.add_argument('env_name', help='The name of an environment.')
    start_sp.set_defaults(handler=start)

    show_sp = subparsers.add_parser('show', help='List available tenper envs.')
    show_sp.set_defaults(handler=show)

    args = parser.parse_args()
    try:
        args.handler(
            args.env_name,
            os.path.join(configs, '{}.py'.format(args.env_name)))
    except:
        args.handler()
