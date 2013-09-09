#!env python
from setuptools import setup, find_packages

setup(
    name='trussws',
    version='0.1',
    description=(
        'A web server that acts as a temporary, browser-based bridge to a '
        'remote machine.'),
    long_description=(
        'Truss is a temporary, browser-based bridge to a remote machine. It '
        'will serve and receive files over an SSL connection with basic HTTP '
        'authorization. It will shut itself down (after five minutes, by '
        'default). It is meant to augment my usage of python -m '
        'SimpleHTTPSever.'),
    keywords='file, manager, http, server, ssl, basic, http, auth',
    author='Mason Staugler',
    author_email='mason@staugler.net',
    url='https://github.com/mqsoh/truss',
    license='MIT license',
    classifiers=[
        'Programming Language :: Python :: 2.7',
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
    ],

    packages=find_packages(exclude=['*_test.py']),
    entry_points={'console_scripts': ['truss = truss.truss:main']},
    package_data={'truss': ['files/*']})
