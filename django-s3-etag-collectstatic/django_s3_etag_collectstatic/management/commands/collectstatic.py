import hashlib

import django.conf
from django.contrib.staticfiles.management.commands import collectstatic


def md5_for_file(f, block_size=2**20):
    """Generates an md5sum from a file without reading the entire thing into
    memory. From: http://stackoverflow.com/a/1131255"""

    md5 = hashlib.md5()

    while True:
        data = f.read(block_size)
        if not data:
            break
        md5.update(data)

    return md5.hexdigest()


class Command(collectstatic.Command):
    def delete_file(self, path, prefixed_path, source_storage):
        if self.storage.exists(prefixed_path):
            try:
                # Support django-s3-folder-storage by prefixing the remote key
                # with the configured directory.
                remote_path = prefixed_path
                if getattr(django.conf.settings, 'STATIC_S3_PATH', None):
                    remote_path = '/'.join([django.conf.settings.STATIC_S3_PATH,
                                            prefixed_path])

                remote_checksum = self.storage._wrapped.entries.get(remote_path).etag.strip('"')
                local_checksum = md5_for_file(source_storage.open(path))

            except Exception as e:
                print((
                    'django-s3-etag-collecstatic error: Skipping etag/md5 '
                    'comparison due to an exception ({}).'
                ).format(e))

            else:
                if remote_checksum == local_checksum:
                    return False
                else:
                    return True

        return super(Command, self).delete_file(path, prefixed_path, source_storage)
