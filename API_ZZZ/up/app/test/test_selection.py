import requests
import unittest
import copy

URL = 'http://localhost:5000'


class Selection(unittest.TestCase):

    endpoint = '/selection'
    auth = ('admin', 'admin')

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint + '/college', auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response_college(self):
        response = requests.get(URL + self.endpoint + '/college', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_major(self):
        response = requests.get(URL + self.endpoint + '/major', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_business(self):
        response = requests.get(URL + self.endpoint + '/business', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_sex(self):
        response = requests.get(URL + self.endpoint + '/sex', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_area(self):
        response = requests.get(URL + self.endpoint + '/area', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_yearIn(self):
        response = requests.get(URL + self.endpoint + '/yearIn', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)

    def test_response_businessType(self):
        response = requests.get(URL + self.endpoint + '/businessType', auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'label'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], str)
        self.assertIsInstance(response.json().get('label')[0], str)


class Search(unittest.TestCase):

    endpoint = '/search'
    auth = ('admin', 'admin')

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response_name(self):
        response = requests.get(URL + self.endpoint, params={'q': '马文'}, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'percode', 'name', 'sex', 'major', 'college'],
                             list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], int)
        self.assertIsInstance(response.json().get('percode')[0], str)

    def test_response_percode(self):
        response = requests.get(URL + self.endpoint, params={'q': '320160935121'}, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['id', 'percode', 'name', 'sex', 'major', 'college'],
                             list(response.json().keys()))
        self.assertIsInstance(response.json().get('id')[0], int)
        self.assertIsInstance(response.json().get('percode')[0], str)


if __name__ == '__main__':
    unittest.main()
