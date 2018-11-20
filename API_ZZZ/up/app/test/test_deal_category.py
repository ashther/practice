import requests
import unittest
import copy

URL = 'http://localhost:5000'


class CategoryTotal(unittest.TestCase):

    endpoint = '/deal/category/total'
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
        self.assertListEqual(['type', 'n'], list(response.json().keys()), 'should have type and n')
        self.assertIsInstance(response.json().get('type')[0], str, 'type should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')


class CategoryDetail(unittest.TestCase):

    endpoint = '/deal/category/detail'
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

    def test_response_sex(self):
        args = copy.copy(self.args)
        args.update({'sex': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        response = response.json()
        self.assertListEqual(['date', 'data'], list(response.keys()), 'should have date and data')
        self.assertIsInstance(response.get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.get('data')[0], dict, 'data should be dict type')
        self.assertListEqual(['label', 'value'], list(response.get('data')[0].keys()))
        self.assertIsInstance(response['data'][0]['value'][0], float)

    def test_response_area(self):
        args = copy.copy(self.args)
        args.update({'area': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        response = response.json()
        self.assertListEqual(['date', 'data'], list(response.keys()), 'should have date and data')
        self.assertIsInstance(response.get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.get('data')[0], dict, 'data should be dict type')
        self.assertListEqual(['label', 'value'], list(response.get('data')[0].keys()))
        self.assertIsInstance(response['data'][0]['value'][0], float)

    def test_response_yearIn(self):
        args = copy.copy(self.args)
        args.update({'yearIn': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        response = response.json()
        self.assertListEqual(['date', 'data'], list(response.keys()), 'should have date and data')
        self.assertIsInstance(response.get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.get('data')[0], dict, 'data should be dict type')
        self.assertListEqual(['label', 'value'], list(response.get('data')[0].keys()))
        # self.assertIsInstance(response['data'][0]['value'][0], float)
        self.assertTrue(isinstance(response['data'][0]['value'][0], float) or
                        response['data'][0]['value'][0] is None)


class CategoryDaily(unittest.TestCase):

    endpoint = '/deal/category/daily'
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
        response = response.json()
        self.assertListEqual(['date', 'data'], list(response.keys()), 'should have date and data')
        self.assertIsInstance(response.get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.get('data')[0], dict, 'data should be dict type')
        self.assertListEqual(['label', 'value'], list(response.get('data')[0].keys()))
        self.assertIsInstance(response['data'][0]['value'][0], float)

    def test_response_with_isAvg(self):
        args = copy.copy(self.args)
        args.update({'isAvg': 'true'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        response = response.json()
        self.assertListEqual(['date', 'data'], list(response.keys()), 'should have date and data')
        self.assertIsInstance(response.get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.get('data')[0], dict, 'data should be dict type')
        self.assertListEqual(['label', 'value'], list(response.get('data')[0].keys()))
        self.assertIsInstance(response['data'][0]['value'][0], float)


if __name__ == '__main__':
    unittest.main()
