#!env python
from setuptools import setup, find_packages

setup(
    name='truss',
    version='0.1',
    description='A simple+ HTTP server.',
    long_description=(
        'An HTTP server that also supports uploads. This is "python -m '
        'SimpleHTTPServer" augmented for someone doing all their development in '
        'the cloud.'),
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

    packages=find_packages(exclude=['*_tests.py']),
    entry_points={'console_scripts': ['truss = truss.core:main']},
    package_data={'truss': ['files/*']})
