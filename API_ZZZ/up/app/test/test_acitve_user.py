import requests
import unittest
import copy

URL = 'http://localhost:5000'


class ActiveTotal(unittest.TestCase):

    endpoint = '/activeUser/total'
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
        self.assertListEqual(['total', 'ratio'], list(response.json().keys()), 'should have total and ratio')
        self.assertIsInstance(response.json().get('total'), int, 'total should be int type')
        self.assertIsInstance(response.json().get('ratio'), float, 'ratio should be float type')


class ActiveDaily(unittest.TestCase):

    endpoint = '/activeUser/daily'
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

    def test_response_total(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()), 'should have date and n')
        self.assertIsInstance(response.json().get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')

    def test_response_business(self):
        args = copy.copy(self.args)
        args.update({'business': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()), 'should have date and n')
        self.assertIsInstance(response.json().get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')

    def test_response_sex(self):
        args = copy.copy(self.args)
        args.update({'sex': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()), 'should have date and n')
        self.assertIsInstance(response.json().get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')

    def test_response_area(self):
        args = copy.copy(self.args)
        args.update({'area': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()), 'should have date and n')
        self.assertIsInstance(response.json().get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')

    def test_response_yearIn(self):
        args = copy.copy(self.args)
        args.update({'yearIn': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()), 'should have date and n')
        self.assertIsInstance(response.json().get('date')[0], str, 'date should be string type')
        self.assertIsInstance(response.json().get('n')[0], int, 'n should be integer type')


class ActiveLost(unittest.TestCase):

    endpoint = '/activeUser/lost'
    auth = ('admin', 'admin')
    args = {
        'level': 'all',
        'id': None,
        'pageIndex': 1,
        'pageSize': 5
    }
    data_col_names = []

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['total', 'data'], list(response.json().keys()), 'should have total and data')
        self.assertIsInstance(response.json().get('total'), int, 'total should be integer type')
        self.assertIsInstance(response.json().get('data'), dict, 'data should be dict type')
        self.assertListEqual(self.data_col_names, list(response.json().get('data').keys()))


if __name__ == '__main__':
    unittest.main()
