import requests
import unittest
import copy

URL = 'http://localhost:5000'


class DealCountDaily(unittest.TestCase):

    endpoint = '/deal/count/daily'
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
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], int)

    def test_response_isAvg(self):
        args = copy.deepcopy(self.args)
        args.update({'isAvg': 'true'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)


class DealCountDetail(unittest.TestCase):

    endpoint = '/deal/count/detail'
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

    def test_response_business(self):
        args = copy.deepcopy(self.args)
        args.update({'business': '丹桂苑一楼'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], int)

    def test_response_sex(self):
        args = copy.deepcopy(self.args)
        args.update({'sex': '男'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], int)

    def test_response_area(self):
        args = copy.deepcopy(self.args)
        args.update({'area': '甘肃省'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], int)

    def test_response_yearIn(self):
        args = copy.deepcopy(self.args)
        args.update({'yearIn': '2017'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], int)


if __name__ == '__main__':
    unittest.main()