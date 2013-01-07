from distutils.core import setup

setup(
    name='tenper',
    version='1.0',
    description='A tmux session manager with optional virtualenv support.',
    long_description=(
        'Tenper is a tmux wrapper. It provides project-based tmux window/pane '
        'layouts.  It has optional support for Python\'s virtualenv and the '
        'conventions it uses permits concurrent usage of virtualenvwrapper.'),
    keywords='tmux, virtualenv, virtualenvwrapper',
    author='Mason Staugler',
    author_email='mason@staugler.net',
    url='https://github.com/mqsoh/tenper',
    py_modules=['tenper'],
    scripts=['tenper'],
    license='MIT license',
)
