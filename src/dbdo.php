<?php
namespace dbdo;

$database_connections = array();

/**
 * A list of all the migrations.
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
function list_migrations($database_url, $dir) {
    $all_migrations = array();

    $migrations_run = all_run_migrations($database_url);

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
function run($database_url, $dir, $name) {
    $fn = "$dir/$name/up.sql";

    // Short circuit for the migration's file missing.
    if (!file_exists($fn)) {
        return array(false, array(0, "Upgrade file not found ($fn)."));
    }

    // Short circuit for migrations that have already been run.
    $all_run = all_run_migrations($database_url);
    if (in_array($name, array_keys($all_run))) {
        return array(false, array(0, "Migration already run."));
    }

    list($ok, $resp) = query($database_url, file_get_contents($fn));

    if ($ok) {
        $q = sprintf("
            insert into dbdo_migration (name, ran)
            values ('%s', '%s');",
            mysqli_escape_string(database_connection($database_url), $name),
            date('Y-m-d H:i:s'));
        list($ok, $resp) = query($database_url, $q);

        if (!$ok) {
            list($code, $error) = $resp;
            return array(false, array(0, "The migration $name was run but we failed to log the migration to the dbdo_migration table ($error)."));
        }
    }

   return array($ok, $resp);
}

/**
 * Undoes a migration, first checking that it has already been run.
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
function unrun($database_url, $dir, $name) {
    $fn = "$dir/$name/down.sql";

    // Short circuit for the migration's file missing.
    if (!file_exists($fn)) {
        return array(false, array(0, "Downgrade file not found ($fn)."));
    }

    // Short circuit for a migration that has not been run.
    $all_run = all_run_migrations($database_url);
    if (!in_array($name, array_keys($all_run))) {
        return array(false, array(0, "Migration hasn't been run."));
    }

    list($ok, $resp) = query($database_url, file_get_contents($fn));

    if ($ok) {
        $q = sprintf("
            delete from dbdo_migration
            where name = '%s';",
            mysqli_escape_string(database_connection($database_url), $name));

        list($ok, $resp) = query($database_url, $q);

        if (!$ok) {
            list($code, $error) = $resp;
            return array(false, array(0, "The migration $name was unrun but we failed to remove the migration to the dbdo_migration table ($error)."));
        }
    }

   return array($ok, $resp);
}

/**
 * Runs all unrun migrations in $dir.
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
function run_all($database_url, $dir) {
    $all_run = array_keys(all_run_migrations($database_url));

    $successes = array();

    foreach (array_diff(scandir($dir), array('.', '..')) as $migration) {
        if (!in_array($migration, $all_run)) {
            list($ok, $resp) = run($database_url, $dir, $migration);

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
function unrun_all($database_url, $dir) {
    $all_run = array_reverse(array_keys(all_run_migrations($database_url)));

    $successes = array();

    foreach ($all_run as $migration_name) {
        list($ok, $resp) = unrun($database_url, $dir, $migration_name);
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
function unrun_after_time($database_url, $dir, $time) {
    $all_run = array_reverse(all_run_migrations($database_url));

    $successes = array();

    foreach ($all_run as $name => $time_ran) {
        if (strtotime($time_ran) > $time) {
            list($ok, $resp) = unrun($database_url, $dir, $name);

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
function unrun_after_migration($database_url, $dir, $name) {
    $all_run = array_keys(all_run_migrations($database_url));

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
        list($ok, $resp) = unrun($database_url, $dir, $migration_name);

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
function all_run_migrations($url) {
    $run = array();

    list($status, $resp) = query($url, 'SELECT * FROM dbdo_migration ORDER BY ran ASC');

    if (!$status) {
        // Error handling.
        list($code, $message) = $resp;
        if ($code == '42S02') {
            // Table doesn't exist: let's create it.
            $desc = 'Dbdo is a library and command line utility for managing a MySQL database.';
            query($url, "CREATE TABLE dbdo_migration (
                id INTEGER AUTO_INCREMENT PRIMARY KEY,
                name VARCHAR(100) NOT NULL,
                ran TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                ) COMMENT = '$desc';");
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
?>
