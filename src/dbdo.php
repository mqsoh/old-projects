<?php
/*
 * This file is part of dbdo.
 *
 * Dbdo is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Dbdo is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * dbdo.  If not, see <http://www.gnu.org/licenses/>.
 */

namespace dbdo;

$database_connections = array();

/**
 * A list of all the migrations.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     A URL in the form of: mysqli://user:password@host/database.
 *
 * @param string $dir
 *     The directory containing the migrations. The directory names are the
 *     unique names for the migrations. Inside you can have (it's optional) an
 *     'info' file with a 'title' and 'description'. This is loaded with
 *     parse_ini_file and should look like:
 *
 *         title = "Some Title"
 *         description = "This is a description of what this migration does."
 *
 *     You should also have an up.sql and down.sql. For example:
 *
 *         01-schema/
 *             down.sql
 *             info
 *             up.sql
 *
 *         02-adding-something/
 *             down.sql
 *             info
 *             up.sql
 *
 * @return
 *     A list of lists representing the migrations, like:
 *         array(
 *             array($time_run_or_null, $name, $title, $description),
 *             array($time_run_or_null, $name, $title, $description),
 *             ...
 *             array($time_run_or_null, $name, $title, $description))
 */
function list_migrations($app, $database_url, $dir) {
    $all_migrations = array();

    $migrations_run = all_run_migrations($app, $database_url);

    foreach (array_diff(scandir($dir), array('.', '..')) as $migration) {
        $migration_dir = "$dir/$migration";

        if (is_dir($migration_dir)) {
            // Date run.
            $time_run = null;
            if (!empty($migrations_run[$migration])) {
                $time_run = $migrations_run[$migration];
            }

            // Title and description.
            $title = $migration;
            $description = '';
            $info_file = "$dir/$migration/info";
            if (file_exists($info_file)) {
                $data = parse_ini_file($info_file);

                if (!empty($data)) {
                    $title = empty($data['title']) ? $title : $data['title'];
                    $description = empty($data['description']) ? $description: $data['description'];
                }
            }

            $all_migrations[] = array($time_run, $migration, $title, $description);
        }
    }

    return $all_migrations;
}

/**
 * Runs a migration against the database, first checking that it hasn't already
 * been run.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @param string $name
 *     Name of the migration, which is the file name without the .sql
 *     extension.
 *
 * @return
 *     An array of the status (false on failure, true on success) and a
 *     response. If it failed, it's another list of the SQL response code and
 *     the error message. One might use it like this.
 *
 *         list($ok, $resp) = dbdo\run('url', 'dir', 'some-migration');
 *         if (!$ok) {
 *             list($code, $message) = array_values($resp);
 *             echo "Error. The code was $code, with message '$message'.";
 *         }
 */
function run($app, $database_url, $dir, $name) {
    $fn = "$dir/$name/up.sql";

    // Short circuit for the migration's file missing.
    if (!file_exists($fn)) {
        return array(false, array(0, "Upgrade file not found ($fn)."));
    }

    // Short circuit for migrations that have already been run.
    $all_run = all_run_migrations($app, $database_url);
    if (in_array($name, array_keys($all_run))) {
        return array(false, array(0, "Migration already run."));
    }

    list($ok, $resp) = query($database_url, file_get_contents($fn));

    if ($ok) {
        $app = scrub_app_name($app);
        $q = sprintf("
            insert into dbdo_migration_$app (name, ran)
            values ('%s', '%s');",
            mysqli_escape_string(database_connection($database_url), $name),
            date('Y-m-d H:i:s'));
        list($ok, $resp) = query($database_url, $q);

        if (!$ok) {
            list($code, $error) = $resp;
            return array(false, array(0, "The migration $name was run but we failed to log the migration to the dbdo_migration_$app table ($error)."));
        }
    }

   return array($ok, $resp);
}

/**
 * Undoes a migration, first checking that it has already been run.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @param string $name
 *     Name of the migration which is the directory that contains the down.sql.
 *
 * @return
 *     An array of the status (false on failure, true on success) and a
 *     response. If it failed, it's another list of the SQL response code and
 *     the error message. One might use it like this.
 *
 *         list($ok, $resp) = dbdo\unrun('url', 'dir', 'some-migration');
 *         if (!$ok) {
 *             list($errno, $code, $message) = array_values($resp);
 *             echo "Error. The code was $code, with message '$message'.";
 *         }
 */
function unrun($app, $database_url, $dir, $name) {
    $fn = "$dir/$name/down.sql";

    // Short circuit for the migration's file missing.
    if (!file_exists($fn)) {
        return array(false, array(0, "Downgrade file not found ($fn)."));
    }

    // Short circuit for a migration that has not been run.
    $all_run = all_run_migrations($app, $database_url);
    if (!in_array($name, array_keys($all_run))) {
        return array(false, array(0, "Migration hasn't been run."));
    }

    list($ok, $resp) = query($database_url, file_get_contents($fn));

    if ($ok) {
        $app = scrub_app_name($app);
        $q = sprintf("
            delete from dbdo_migration_$app
            where name = '%s';",
            mysqli_escape_string(database_connection($database_url), $name));

        list($ok, $resp) = query($database_url, $q);

        if (!$ok) {
            list($code, $error) = $resp;
            return array(false, array(0, "The migration $name was unrun but we failed to remove the migration to the dbdo_migration_$app table ($error)."));
        }
    }

   return array($ok, $resp);
}

/**
 * Runs all unrun migrations in $dir.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @return
 *     A boolean representing success, a list of the migrations that succeeded,
 *     and a list of the migration that failed and its error message (or null
 *     when $ok is true).
 *
 *         array(
 *             $ok,
 *             array('successful-1',
 *                   'successful-2',
 *                   ...,
 *                   'successful-N'),
 *             array('failed-name',
 *                   'Error message.'))
 */
function run_all($app, $database_url, $dir) {
    $all_run = array_keys(all_run_migrations($app, $database_url));

    $successes = array();

    foreach (array_diff(scandir($dir), array('.', '..')) as $migration) {
        if (!in_array($migration, $all_run)) {
            list($ok, $resp) = run($app, $database_url, $dir, $migration);

            // Short circuit when a migration fails.
            if (!$ok) {
                list($code, $message) = $resp;
                return array(false, $successes, array($migration, $message));
            }

            $successes[] = $migration;
        }
    }

    return array(true, $successes, null);
}

/**
 * Unruns all run migrations.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @return
 *     A boolean representing success, a list of the migrations that succeeded,
 *     and a list of the migration that failed and its error message (or null
 *     when $ok is true).
 *
 *         array(
 *             $ok,
 *             array('successful-1',
 *                   'successful-2',
 *                   ...,
 *                   'successful-N'),
 *             array('failed-name',
 *                   'Error message.'))
 */
function unrun_all($app, $database_url, $dir) {
    $all_run = array_reverse(array_keys(all_run_migrations($app, $database_url)));

    $successes = array();

    foreach ($all_run as $migration_name) {
        list($ok, $resp) = unrun($app, $database_url, $dir, $migration_name);
        if (!$ok) {
            list($code, $reason) = $resp;
            return array(false, $successes, array($migration_name, $reason));
        }

        $successes[] = $migration_name;
    }

    return array(true, $successes, null);
}

/**
 * Unruns all migrations that were run after a certain time.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @param int $time
 *     Unix time. All migrations run after this time will be unrun.
 *
 * @return
 *     A boolean representing success, a list of the migrations that succeeded,
 *     and a list of the migration that failed and its error message (or null
 *     when $ok is true).
 *
 *         array(
 *             $ok,
 *             array('successful-1',
 *                   'successful-2',
 *                   ...,
 *                   'successful-N'),
 *             array('failed-name',
 *                   'Error message.'))
 */
function unrun_after_time($app, $database_url, $dir, $time) {
    $all_run = array_reverse(all_run_migrations($app, $database_url));

    $successes = array();

    foreach ($all_run as $name => $time_ran) {
        if (strtotime($time_ran) > $time) {
            list($ok, $resp) = unrun($app, $database_url, $dir, $name);

            if (!$ok) {
                list($code, $reason) = $resp;
                return array(false, $successes, array($name, $reason));
            }

            $successes[] = $name;
        }
    }

    return array(true, $successes, null);
}

/**
 * Unruns all migrations that were run after a certain migration.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $database_url
 *     The URL to the database in the form of:
 *         mysqli://user:password@host/database.
 *
 * @param string $dir
 *     Path to the directory containing the migrations.
 *
 * @param string $name
 *     The name of the migration after which all migrations should be unrun.
 *
 * @return
 *     A boolean representing success, a list of the migrations that succeeded,
 *     and a list of the migration that failed and its error message (or null
 *     when $ok is true).
 *
 *         array(
 *             $ok,
 *             array('successful-1',
 *                   'successful-2',
 *                   ...,
 *                   'successful-N'),
 *             array('failed-name',
 *                   'Error message.'))
 */
function unrun_after_migration($app, $database_url, $dir, $name) {
    $all_run = array_keys(all_run_migrations($app, $database_url));

    // Find the index of the referenced migration.
    $referenced_ii = 0;
    foreach ($all_run as $ii => $run_name) {
        if ($run_name == $name) {
            $referenced_ii = $ii;
            break;
        }
    }

    $to_unrun = array_reverse(array_slice($all_run, $referenced_ii + 1));
    $successes = array();

    foreach ($to_unrun as $migration_name) {
        list($ok, $resp) = unrun($app, $database_url, $dir, $migration_name);

        if (!$ok) {
            list($code, $reason) = $resp;
            return array(false, $successes, array($migration_name, $reason));
        }

        $successes[] = $migration_name;
    }

    return array(true, $successes, null);
}

/**
 * Returns a database connnection from the provided URL. The connection is
 * stored in a global hash keyed on the URL for reuse.
 *
 * @param string $url
 *     The database URL in the form of: mysqli://user:password@host/database
 *
 * @return
 *     A database connection.
 */
function database_connection($url) {
    global $database_connections;

    $conn = null;

    if (!empty($database_connections[$url])) {
        $conn = $database_connections[$url];
    }
    else {
        if (preg_match('|^(?P<protocol>[^:]+)://(?P<user>[^:]+):(?P<password>[^@]+)@(?P<host>[^/]+)/(?P<database>[\w]+)$|', $url, $g)) {
            $conn = \mysqli_connect($g['host'], $g['user'], $g['password'], $g['database']);
            $database_connections[$url] = $conn;
        }
    }

    return $conn;
}

/**
 * Convenience method to avoid having to first call database_connection with
 * the URL.
 *
 * @param string $url
 *     The database URL.
 *
 * @param string $queries
 *     The SQL to run; can be more than one statement.
 *
 * @return
 *     A boolean status. If the status is true (ok), then the response is the
 *     database response object. If the status is false (not ok), then the
 *     response is an error list.
 *     example:
 *
 *         array(true, mysqli_connection), or
 *         array(false, array('errno' => 1193,
 *                            'sqlstate' => '42S02',
 *                            'error' => 'Table foo does not exist.'))
 *
 *     For more information about the SQL state, see:
 *         http://dev.mysql.com/doc/refman/5.6/en/error-messages-server.html
 */
function query($url, $query) {
    $conn = database_connection($url);

    $ok = true;

    $resp = mysqli_multi_query($conn, $query);

    while (mysqli_next_result($conn));

    if (!$resp) {
        return array(false, array(mysqli_sqlstate($conn), mysqli_error($conn)));
    }

    return array($ok, $conn);
}

/**
 * Returns a sorted list of all the run migrations stored in the database.
 *
 * @param string $app
 *     An application name, which is a suffix to the table name that tracks
 *     migrations. This allows you to run multiple applications in the same
 *     database.
 *
 * @param string $url
 *     The database URL.
 *
 * @return
 *     The migrations in ascending order of time.
 *
 *         array(
 *             $name => $time_run,
 *             $name => $time_run,
 *             ...
 *             $name => $time_run)
 */
function all_run_migrations($app, $url) {
    $app = scrub_app_name($app);

    $run = array();

    list($status, $resp) = query($url, "
        select *
        from dbdo_migration_$app
        order by ran asc");

    if (!$status) {
        // Error handling.
        list($code, $message) = $resp;
        if ($code == '42S02') {
            // Table doesn't exist: let's create it.
            $desc = 'Dbdo is a library and command line utility for managing a MySQL database.';
            query($url, "
                create table dbdo_migration_$app (
                    id integer auto_increment primary key,
                    name varchar(100) not null,
                    ran timestamp default current_timestamp
                ) comment = '$desc';");
        }
    }
    else {
        while (true) {
            $result = mysqli_store_result($resp);

            while (true) {
                $row = mysqli_fetch_array($result);
                if (!$row) {
                    break;
                }

                $name = $row['name'];
                $time = $row['ran'];
                $run[$name] = $time;
            }

            if (!mysqli_next_result($resp)) {
                break;
            }
        }
    }

    return $run;
}

/**
 * It's just ugly, so we'll abstract it.
 *
 * @param string $app
 */
function scrub_app_name($app) {
    return preg_replace('/[^\\w]/', '', $app);
}
?>
