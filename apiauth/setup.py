from setuptools import setup, find_packages

setup(
    name='apiauth',
    version='0.1',
    description=(
        'A library to create valid API requests by a client and verify '
        'requests on a server.'),
    long_description=(
        "A implementation of an IETF draft for MAC Access Authentication. It's "
        "equivalent to the request signing used by services like AWS, but uses "
        "HTTP headers instead of additional parameters. This libray provides "
        "convenience functions for signing requests by the client and also "
        "verifying requests on the server."),
    keywords='mac, access, authentication, api, server, client',
    author='Mason Staugler',
    author_email='mason@staugler.net',
    url='https://github.com/mqsoh/apiauth',
    license='MIT license',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
    ],
    package_dir={'apiauth': 'apiauth'})
