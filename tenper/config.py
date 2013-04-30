import os

import yaml


def _get(config, key, default=None):
    """Gets and expands a configuration value.

    Returns:
        The value or None.
    """

    value = config.get(key, default)

    try:
        return os.path.expandvars(value)
    except:
        return value


def _get_virtualenv(config, key, default=None):
    """Gets and expands a value inside the 'virtualenv' in 'config'.

    The 'virtualenv' might not be available, so we want to abstract that test.

    Returns:
        The value or None.
    """

    value = config.get('virtualenv', {}).get(key, default)

    try:
        return os.path.expandvars(value)
    except:
        return value


def create(file_name, env):
    """Creates a new config file from the template.

    Args:
        file_name: The target config file name.
        env: The initial session name.

    Returns:
        A boolean indicating success.
    """

    directory = os.path.dirname(file_name)
    template_file_name = os.path.join(os.path.dirname(__file__),
                                      'config_template.yml')

    if not os.path.exists(directory):
        os.mkdir(directory)

    if not os.path.exists(file_name):
        # We could do a simple file copy if it weren't for prepopulating the
        # 'session name' in the config.
        with open(template_file_name, 'r') as template_file:
            with open(file_name, 'w') as config_file:
                config_file.write(template_file.read().format(env=env))

    return True


def load(file_name):
    """Returns a dictionary with a parsed config file.

    This will flatten the virtualenv definition for the core._run_context and
    apply os.path.expandvars where appropriate.

    Args:
        file_name: The path to a configuration file.

    Returns:
        If no file exists:
            {'config_file_name': file_name}

        If the file exists:
            {'config_file_name': file_name,
             'session_name': ...,
             'project_root': ...,
             'environment': ...,
             'windows': ...}

        If a virtualenv is configured the dict will also have:
            {'virtualenv_configured': True,
             'virtualenv_base_path': ...,
             'virtualenv_python_binary': ...,
             'virtualenv_use_site_packages': True | False}
    """

    # Short circuit; new environment.
    if not os.path.exists(file_name):
        return {'config_file_name': file_name}

    with open(file_name, 'r') as f:
        config = yaml.load(f)

    d = {'config_file_name': file_name,
         'session_name': _get(config, 'session name'),
         'project_root': _get(config, 'project root'),
         'environment': config.get('environment', None),
         'windows': config.get('windows', None)}

    if config.get('virtualenv'):
        site_packages = '--no-site-packages'
        if _get_virtualenv(config, 'site packages?'):
            site_packages = '--system-site-packages'

        d.update({'virtualenv_configured': True,
                  'virtualenv_path': os.path.join(core._run_context['virtualenvs_path'], env),
                  'virtualenv_python_binary': _get_virtualenv(config, 'python binary'),
                  'virtualenv_use_site_packages': site_packages})

    return d
