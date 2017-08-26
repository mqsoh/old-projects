import base64
import io
import unittest

import mock

import core


class AuthorizedTest(unittest.TestCase):
    def test_no_required_credentials(self):
        self.assertFalse(core.authorized({}, None, None))

    def test_no_authorization_header(self):
        self.assertFalse(core.authorized({}, 'foo', 'bar'))

    def test_malformed_authorization_header(self):
        self.assertFalse(core.authorized({'HTTP_AUTHORIZATION': 'This is bad.'}, 'foo', 'bar'))

    def test_bad_credentials(self):
        env = {'HTTP_AUTHORIZATION': 'Basic {}'.format(base64.b64encode('baz:buzz'))}
        self.assertFalse(core.authorized(env, 'foo', 'bar'))

    def test_valid_credentials(self):
        env = {'HTTP_AUTHORIZATION': 'Basic {}'.format(base64.b64encode('foo:bar'))}
        self.assertTrue(core.authorized(env, 'foo', 'bar'))
