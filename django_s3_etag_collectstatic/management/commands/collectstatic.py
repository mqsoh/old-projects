import hashlib

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
                remote_checksum = self.storage._wrapped.entries.get(prefixed_path).etag.strip('"')
                local_checksum = md5_for_file(source_storage.open(path))

            except Exception e:
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
