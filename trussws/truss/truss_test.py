import unittest

import truss


class ParseArgsTest(unittest.TestCase):
    def test_no_args(self):
        with self.assertRaises(SystemExit):
            truss.parse_args([])

    def test_wrong_number_of_args(self):
        with self.assertRaises(SystemExit):
            truss.parse_args(['user', 'pass'])

    def test_minimal_args(self):
        args = truss.parse_args(['foouser', 'barpassword', '8000'])

        self.assertEqual(args['docroot'], '.')
        self.assertEqual(args['hostname'], '')
        self.assertEqual(args['password'], 'barpassword')
        self.assertEqual(args['port'], 8000)
        self.assertEqual(args['username'], 'foouser')

    def test_maximal_args(self):
        args = truss.parse_args([
            'foouser',
            'barpassword',
            '8000',
            '-d',
            '/path/to/docroot',
            '-n',
            'example.com'])

        self.assertEqual(args['docroot'], '/path/to/docroot')
        self.assertEqual(args['hostname'], 'example.com')
        self.assertEqual(args['password'], 'barpassword')
        self.assertEqual(args['port'], 8000)
        self.assertEqual(args['username'], 'foouser')
