dbdo
====

Dbdo is a schema migration tool for MySQL. It has a library and command line
script.

Requirements
============

*PHP 5.3+* -- dbdo has a namespace definition. It's feasible to remove it (and
possibly rename functions to avoid collisions with other libraries) to make it
compatible with PHP 5.2.

Command line script usage
=========================

::

    dbdo-cli.php [-h] database_url directory command [command_arguments]

database_url:
    The MySQL database URL. I forgot what it's called, but it's of the form ::
        mysql://user:password@host/database

directory:
    The directory containing the migrations. The directory names are the unique
    names for the migrations. Inside you can have (it's optional) an 'info'
    file with a 'title' and 'description'. This is loaded with parse_ini_file
    and should look like::

        title = "Some Title"
        description = "This is a description of what this migration does."

    You should also have an up.sql and down.sql. For example::

        01-schema/
            down.sql
            info
            up.sql

        02-adding-something/
            down.sql
            info
            up.sql

command:
    One of
        - list
        - run
        - unrun
        - run-all
        - unrun-all
        - unrun-after-time
        - unrun-after-migration

[command_arguments]:
    Some of the commands take arguments. Run, unrun, and unrun-after-migration
    take a migration name. Unrun-after-time takes something that can be parsed
    by PHP's strtotime.
