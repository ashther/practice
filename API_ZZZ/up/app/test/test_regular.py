import requests
import unittest
import copy

URL = 'http://localhost:5000'


class RegularGroup(unittest.TestCase):

    endpoint = '/regularGroup'
    auth = ('admin', 'admin')
    args = {
        'level': 'all',
        'id': None,
        'startDate': '2017-09-01',
        'endDate': '2017-09-07'
    }

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'regularMean'], list(response.json().keys()))
        self.assertListEqual(['group', 'regular'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['group'][0], str)
        self.assertIsInstance(response.json().get('data')['regular'][0], float)
        self.assertIsInstance(response.json().get('regularMean'), float)

    def test_response_sex(self):
        args = copy.deepcopy(self.args)
        args.update({'sex': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'regularMean'], list(response.json().keys()))
        self.assertListEqual(['group', 'regular'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['group'][0], str)
        self.assertIsInstance(response.json().get('data')['regular'][0], float)
        self.assertIsInstance(response.json().get('regularMean'), float)

    def test_response_area(self):
        args = copy.deepcopy(self.args)
        args.update({'area': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'regularMean'], list(response.json().keys()))
        self.assertListEqual(['group', 'regular'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['group'][0], str)
        self.assertIsInstance(response.json().get('data')['regular'][0], float)
        self.assertIsInstance(response.json().get('regularMean'), float)

    def test_response_yearIn(self):
        args = copy.deepcopy(self.args)
        args.update({'yearIn': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'regularMean'], list(response.json().keys()))
        self.assertListEqual(['group', 'regular'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['group'][0], str)
        self.assertIsInstance(response.json().get('data')['regular'][0], float)
        self.assertIsInstance(response.json().get('regularMean'), float)


class RegularPerson(unittest.TestCase):

    endpoint = '/regularPerson/regular'
    auth = ('admin', 'admin')
    args = {
        'id': 10,
        'startDate': '2017-09-01',
        'endDate': '2017-10-01'
    }

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response_regular(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'mean'], list(response.json().keys()))
        self.assertListEqual(['date', 'regular'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['date'][0], str)
        self.assertIsInstance(response.json().get('data')['regular'][0], float)
        self.assertIsInstance(response.json().get('mean'), float)

    def test_response_normal(self):
        response = requests.get(URL + '/regularPerson/normal', params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['data', 'mean'], list(response.json().keys()))
        self.assertListEqual(['date', 'normal'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json().get('data')['date'][0], str)
        self.assertIsInstance(response.json().get('data')['normal'][0], float)
        self.assertIsInstance(response.json().get('mean'), float)


if __name__ == '__main__':
    unittest.main()
