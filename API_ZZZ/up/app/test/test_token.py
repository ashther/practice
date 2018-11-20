import requests
import unittest

URL = 'http://localhost:5000'


class Token(unittest.TestCase):

    endpoint = '/token'
    auth = ('admin', 'admin')

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 400')

    def test_token(self):
        response = requests.get(URL + self.endpoint, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['duration', 'token'], list(response.json().keys()), 'should have duration and token')
        self.assertIsInstance(response.json().get('duration'), int, 'duration should be int type')
        self.assertIsInstance(response.json().get('token'), str, 'token should be string type')


if __name__ == '__main__':
    unittest.main()
