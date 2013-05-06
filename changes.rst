Changes in 1.2.0
================

- Rewrote everything.

- Repackaged with setuptools -- or is it distutils? What a mess.

- Now provides environment variables for configuration. The following are
  available.

  - TENPER_VERBOSE
        Set to 'true' if you want to see the commands we execute.

  - TENPER_CONFIGS
        The path to where you want the configuration files stored. Defaults to
        ~/.tenper.

  - TENPER_VIRTUALENVS
        The path to where you keep your virtualenvs. Defaults to
        virtualenvwrapper's default of ~/.virtualenvs.

  - TENPER_TMUX_COMMAND
        Defaults to 'tmux'. Try 'tmux -2' if you want 256 colors without TERM
        wrangling.

- Fixed issue #3; Added pyyaml as a dependency.

- Fixed issue #4; If you're already in a tmux session, tenper with switch to
  that session instead of trying to nest.
