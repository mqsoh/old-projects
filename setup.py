from setuptools import setup, find_packages

setup(
    name='tenper',
    version='1.2.2',
    description='A tmux session manager with optional virtualenv support.',
    long_description=(
        'Tenper is a tmux wrapper. It provides project-based tmux window/pane '
        'layouts.  It has optional support for Python\'s virtualenv and the '
        'conventions it uses permits concurrent usage of virtualenvwrapper.'),
    keywords='tmux, virtualenv, virtualenvwrapper',
    author='Mason Staugler',
    author_email='mason@staugler.net',
    url='https://github.com/mqsoh/tenper',
    license='MIT license',
    classifiers=[
        'Programming Language :: Python :: 3',
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
    ],

    install_requires=['pyyaml'],
    packages=find_packages(exclude=['*_tests.py']),
    entry_points={'console_scripts': ['tenper = tenper.core:main']},
    scripts=['scripts/tenper-completion.sh'],
    package_data={'tenper': ['config_template.yml']})
