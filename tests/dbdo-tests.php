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

require_once(dirname(__FILE__).'/../src/dbdo.php');

$test_db_url = 'mysql://root:root@localhost/dbdo_test';
$test_dir = dirname(__FILE__).'/sql';

date_default_timezone_set('America/New_York');

class dbdo_test extends PHPUnit_Framework_TestCase {
    /**
     * list_migrations
     */
    public function test_list_migrations() {
        global $test_db_url;
        global $test_dir;

        $list_of_migrations = dbdo\list_migrations($test_db_url, $test_dir);

        $this->assertEquals($list_of_migrations, array(
            array(
                null,
                '01-one',
                'Test one.',
                'This is the first test.'),
            array(
                null,
                '02-two',
                'Two, this is.',
                'This is a second migration.'),
            array(
                null,
                '03-three',
                'Third test migration.',
                'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec pharetra adipiscing iaculis. Suspendisse blandit semper est, eleifend elementum augue viverra ac. Vivamus fringilla lectus a massa porttitor fringilla. Ut nec lacus vitae lectus viverra tincidunt. Vivamus elementum velit quis eros rhoncus dictum. Donec dapibus adipiscing massa, volutpat facilisis felis placerat ut. Donec et enim ipsum.')));
    }

    /**
     * run
     */
    public function test_run() {
        global $test_db_url;
        global $test_dir;

        list($ok, $resp) = dbdo\run($test_db_url, $test_dir, '01-one');
        $this->assertEquals($ok, true);

        list($ok, $resp) = dbdo\run($test_db_url, $test_dir, '02-two');
        $this->assertEquals($ok, true);

        list($ok, $resp) = dbdo\run($test_db_url, $test_dir, '03-three');
        $this->assertEquals($ok, false);
        $this->assertEquals(count($resp), 2);
    }

    /**
     * unrun
     */
    public function test_unrun() {
        global $test_db_url;
        global $test_dir;

        list($ok, $resp) = dbdo\unrun($test_db_url, $test_dir, '02-two');
        $this->assertEquals($ok, true);
    }

    /**
     * run_all
     */
    public function test_run_all() {
        global $test_db_url;
        global $test_dir;

        list($ok, $successes, $failure) = dbdo\run_all($test_db_url, $test_dir);

        $this->assertEquals($ok, false);
        $this->assertEquals($successes, array('02-two'));
        $this->assertEquals(count($failure), 2);
        $this->assertEquals($failure[0], '03-three');
    }

    /**
     * unrun_all
     */
    public function test_unrun_all() {
        global $test_db_url;
        global $test_dir;

        list($ok, $successes, $failure) = dbdo\unrun_all($test_db_url, $test_dir);
        $this->assertEquals($ok, true);
        $this->assertEquals($successes, array('02-two', '01-one'));
        $this->assertEquals($failure, null);
    }

    /**
     * unrun_after_time
     */
    public function test_unrun_after_time() {
        global $test_db_url;
        global $test_dir;

        // Run all first, because the previous test will have unrun everything.
        list($ok, $successes, $failure) = dbdo\run_all($test_db_url, $test_dir);

        $this->assertEquals($ok, false);
        $this->assertEquals($successes, array('01-one', '02-two'));
        $this->assertEquals(count($failure), 2);
        $this->assertEquals($failure[0], '03-three');

        // Now unrun everything run in the past 5 minutes, which will be
        // everything.
        list($ok, $successes, $failure) = dbdo\unrun_after_time($test_db_url, $test_dir, '-5 minutes');
        $this->assertEquals($ok, true);
        $this->assertEquals($successes, array('02-two', '01-one'));
        $this->assertEquals($failure, null);
    }

    /**
     * unrun_after_migration
     */
    public function test_unrun_after_migration() {
        global $test_db_url;
        global $test_dir;

        // Run all first, because the previous test will have unrun everything.
        list($ok, $successes, $failure) = dbdo\run_all($test_db_url, $test_dir);

        $this->assertEquals($ok, false);
        $this->assertEquals($successes, array('01-one', '02-two'));
        $this->assertEquals(count($failure), 2);
        $this->assertEquals($failure[0], '03-three');

        list($ok, $successes, $failure) = dbdo\unrun_after_migration($test_db_url, $test_dir, '01-one');
        $this->assertEquals(true, $ok);
        $this->assertEquals(array('02-two'), $successes);
        $this->assertEquals(null, $failure);
    }
}
?>
