==============
Not maintained
==============
I didn't like the hassle of maintaining the yaml files. I opted for shell scripting instead. Here are my fish scripts.

https://github.com/mqsoh/dotfiles/blob/0f9818d0c196e1e1c1653af9b405ffa2bc815ce1/.config/fish/functions/t.fish
https://github.com/mqsoh/dotfiles/blob/0f9818d0c196e1e1c1653af9b405ffa2bc815ce1/.config/fish/functions/v.fish

======
tenper
======

Tenper is a tmux wrapper. It provides project-based tmux window/pane layouts.
It has optional support for Python's virtualenv and the conventions it uses
permits concurrent usage of virtualenvwrapper.

(The name is a corruption of 'tmuxvirtualenvwrapper'.)

There is a similar project that you may want to consider: `pytmux`_.


Installation
============
I registered it in the PyPi, so you can ::

    pip install tenper
    # ...or...
    easy_install tenper

Or clone the repo and install it with ::

    python setup.py install
    # ...or (without admin privileges)...
    python setup.py install --user



Usage
=====

Create (or edit) a project.
---------------------------
This will open a YAML file in your environment var $EDITOR.::

    tenper edit my-project

The template looks like this.::

    # Shows up in 'tmux list-sessions' and on the left side of the status bar.
    session name: my-project

    # Uncomment the following to manage a virtualenv for this session. If the
    # virtualenv doesn't exist at <base path>/<session name> a new one will be
    # created.
    #virtualenv:
    #  python binary: /usr/bin/python
    #  site packages?: false

    # When starting a tenper session, all windows and panes will be changed to this
    # directory.
    project root: $HOME

    # Environment variables (only available inside the tmux session).
    environment:
      MYHOME: $HOME

    windows:
      # Valid values for 'layout': even-horizontal, even-vertical, main-horizontal,
      # main-vertical, or tiled. You can also specify the layout string in the
      # list-windows command (see the layout section section in tmux's man page).

      - name: One
        layout: main-vertical
        panes:
          - ls -l
          - top

      - name: Two
        layout: main-vertical
        panes:
          - vim


Start a session.
----------------
If a session with this name already exists, you'll be reattached. If you're
already in a tmux session, you'll be switched (instead of nesting).::

    tenper my-project


Delete a project.
-----------------
You'll be prompted about deleting an associated virtualenv since you might be
using it with virtualenvwrapper and want to keep it.::

    tenper delete my-project


List projects.
--------------
::

    tenper list


Rebuild a virtualenv.
---------------------
If you want to change the Python binary in a virtualenv, you can edit the
project and then rebuild it with::

    tenper rebuild my-project



Configuration
=============
You can set the following environment variables.

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



Environment.
============

Virtualenv for new windows.
---------------------------
If you want to automatically source the configured virtualenv activation script
for new windows in your tmux sessions, tenper provides the TENPER_VIRTUALENV
environment variable to all sessions.

In your **.bashrc**::

    if [[ $TENPER_VIRTUALENV ]] then
        source $TENPER_VIRTUALENV
    fi

...or in your **.zshrc**::

    if [[ -n "$TENPER_VIRTUALENV" ]] then
        source $TENPER_VIRTUALENV
    fi


Tab completion.
---------------

Tenper will install ``tenper-completion.sh`` to a bin directory, so you can
enable **bash** completion by sourcing it. ::

    source $(which tenper-completion.sh)

If you use **zsh**, you can add the following line after you've loaded compinit in
your .zshrc. ::

    compdef "_arguments '*: :($(tenper completions))'" tenper



License
=======
Copyright (c) 2013 Mason Staugler

See LICENSE; it's the MIT license.


.. _pytmux: https://github.com/wraithan/pytmux
