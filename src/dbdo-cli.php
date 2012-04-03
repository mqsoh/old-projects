<?php
$debugging = false;

/**
 * Prints usage information to stdout.
 */
function print_usage() {
    echo <<<EOF
dbdo: Library and command line script for managing a MySQL database.

Usage: dbdo.php [-h] application database_url directory command [command_arguments]

    application
        This will be suffixed to the dbdo_migration table. It's a way of
        tracking multiple applications in the same database.

    database_url
        The MySQL database URL. I forgot what it's called, but it's of the form
            mysql://user:password@host/database

    directory
        The directory containing the SQL scripts for the migration. Using SQL
        comments, these files have a special syntax. Please see the README.

    command
        One of: list, update, run [migration_name], rollback [migration_name],
        revert [date]. Please see the README.

    [command_arguments]
        Some of the commands require additional arguments.
EOF;
}

function info($any_number_of_strings) {
    print("\n[info]\n");
    print(implode("\n", func_get_args())."\n");
}

function error($any_number_of_strings) {
    $err = fopen('php://stderr', 'w');
    fwrite($err, "\n[error]\n");
    fwrite($err, implode("\n", func_get_args())."\n");
    fclose($err);
}

/**
 * Prints all the migrations in a reST table. Is 80 columns enough? Of course!
 */
function print_list($migrations) {
    $hr = "+-----+------------+----------------------+------------------------------------------+\n";
    $hr2 = "+=====+============+======================+==========================================+\n";
    $col_format = "| %-3s | %-10.10s | %-20.20s | %-40.40s |\n";

    printf($hr);
    printf($col_format, "Run", "Name", "Title", "Description");
    printf($hr2);

    foreach ($migrations as $migration) {
        list($time_run, $name, $title, $desc) = $migration;
        printf(
            $col_format,
            ($time_run ? "[X]" : "[ ]"),
            $name,
            $title,
            $desc);
        printf($hr);
    }
}

if (php_sapi_name() == 'cli') {
    require_once('dbdo.php');

    // Short circuit for usage information.
    foreach ($argv as $arg) {
        if ($arg == '-h' || $arg == '--help' || $arg == '-help' || $arg == '-?') {
            print_usage();
            die();
        }
    }

    // Short circuit for invalid input.
    if ($argc < 5) {
        error("Too few arguments. Run with -h for help.");
        die();
    }

    $app = $argv[1]
    $database_url = $argv[2];
    $dir = $argv[3];
    $command = $argv[4];
    $command_arg = empty($argv[5]) ? null : $argv[5];

    if ($command == 'list') {
        // List all migrations.
        print_list(
            dbdo\list_migrations($app, $database_url, $dir));
    }
    else if ($command == 'run') {
        // Run a specific migration.
        if (!$command_arg) {
            error("You must provide a fourth argument: the migration name.");
            die();
        }
        list($ok, $resp) = dbdo\run($app, $database_url, $dir, $command_arg);

        if ($ok) {
            info("Migration $command_arg has been run.");
        }
        else {
            list($code, $error) = $resp;
            error("Migration $command_arg failed ($error).");
        }
    }
    else if ($command == 'unrun') {
        // Undo a specific migration.
        if (!$command_arg) {
            error("You must provide a fourth argument: the migration name.");
            die();
        }

        list($ok, $resp) = dbdo\unrun($app, $database_url, $dir, $command_arg);

        if ($ok) {
            info("Migration $command_arg has been unrun.");
        }
        else {
            list($code, $error) = $resp;
            error("Migration $command_arg failed to be unrun ($error).");
        }
    }
    else if ($command == 'run-all') {
        // Run all new migrations.
        list($ok, $successes, $failure) = dbdo\run_all($app, $database_url, $dir);

        if (!empty($successes)) {
            $str = '';
            foreach ($successes as $success) {
                $str .= "    $success\n";
            }

            info("The following migrations were run.", $str);
        }

        if (!$ok) {
            list($failed_name, $error) = $failure;
            error("Migration $failed_name failed to run ($error).");
        }
    }
    else if ($command == 'unrun-all') {
        // Unrun all migrations.
        list($ok, $successes, $failure) = dbdo\unrun_all($app, $database_url, $dir);

        if (!empty($successes)) {
            $str = '';
            foreach ($successes as $success) {
                $str .= "    $success\n";
            }

            info("The following migrations were unrun.", $str);
        }

        if (!$ok) {
            list($failed_name, $error) = $failure;
            error("Migration $failed_name failed to run ($error).");
        }
    }
    else if ($command == 'unrun-after-time') {
        // Unruns everything after a certain time. Anything that can be parsed
        // by strtotime is suitable for the argument.
        if (!$command_arg) {
            error("You must provide a fourth argument: the time after which all migrations should be unrun.");
            die();
        }

        $time = strtotime($command_arg);

        list($ok, $successes, $failure) = dbdo\unrun_after_time($app, $database_url, $dir, $time);

        if (!empty($successes)) {
            $str = '';
            foreach ($successes as $success) {
                $str .= "    $success\n";
            }

            info("The following migrations were unrun.", $str);
        }

        if (!$ok) {
            list($failed_name, $error) = $failure;
            error("Migration $failed_name failed to run ($error).");
        }
    }
    else if ($command == 'unrun-after-migration') {
        // Unruns everything after a certain migration.
        if (!$command_arg) {
            error("You must provide a fourth argument: the migration after which all migrations should be unrun.");
            die();
        }

        list($ok, $successes, $failure) = dbdo\unrun_after_migration($app, $database_url, $dir, $command_arg);

        if (!empty($successes)) {
            $str = '';
            foreach ($successes as $success) {
                $str .= "    $success\n";
            }

            info("The following migrations were unrun.", $str);
        }

        if (!$ok) {
            list($failed_name, $error) = $failure;
            error("Migration $failed_name failed to run ($error).");
        }
    }
}
?>
