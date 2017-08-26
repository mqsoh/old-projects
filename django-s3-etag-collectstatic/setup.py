from distutils.core import setup

setup(
    name='django-s3-etag-collectstatic',
    version='1.0',
    description=(
        'Overrides the core collectstatic command and compares the S3 etag '
        'and the md5 of the local file.'),
    keywords='django, s3, etag, md5, static, collectstatic',
    author='Mason Staugler',
    author_email='mason@staugler.net',
    url='https://github.com/mqsoh/django-gfklookupwidget',
    packages=[
        'django_s3_etag_collectstatic',
        'django_s3_etag_collectstatic.management',
        'django_s3_etag_collectstatic.management.commands'],
    license='ISC license',
)
