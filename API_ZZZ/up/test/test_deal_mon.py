import requests
import unittest
import copy

URL = 'http://localhost:5000'


class DealMonDaily(unittest.TestCase):

    endpoint = '/deal/mon/daily'
    auth = ('admin', 'admin')
    args = {
        'level': 'all',
        'id': None,
        'startDate': '2017-09-01',
        'endDate': '2017-09-07'
    }

    # def test_wrong_user(self):
    #     response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
    #     self.assertEqual(response.status_code, 401, 'should be 401')

    def test_response(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)

    def test_response_isAvg(self):
        args = copy.deepcopy(self.args)
        args.update({'isAvg': 'true'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)


class DealMonDetail(unittest.TestCase):

    endpoint = '/deal/mon/detail'
    auth = ('admin', 'admin')
    args = {
        'level': 'all',
        'id': None,
        'startDate': '2017-09-01',
        'endDate': '2017-09-07'
    }

    # def test_wrong_user(self):
    #     response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
    #     self.assertEqual(response.status_code, 401, 'should be 401')

    def test_response_business(self):
        args = copy.deepcopy(self.args)
        args.update({'business': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)

    def test_response_sex(self):
        args = copy.deepcopy(self.args)
        args.update({'sex': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)

    def test_response_area(self):
        args = copy.deepcopy(self.args)
        args.update({'area': ''})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)

    def test_response_yearIn(self):
        args = copy.deepcopy(self.args)
        args.update({'yearIn': '2017'})
        response = requests.get(URL + self.endpoint, params=args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['date', 'n'], list(response.json().keys()))
        self.assertIsInstance(response.json().get('date')[0], str)
        self.assertIsInstance(response.json().get('n')[0], float)


class DealMonTransaction(unittest.TestCase):

    endpoint = '/deal/mon/transaction'
    auth = ('admin', 'admin')
    args = {
        'id': 7985,
        'startDate': '2017-09-01',
        'endDate': '2017-09-07',
        'pageIndex': 1,
        'pageSize': 5
    }

    # def test_wrong_user(self):
    #     response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
    #     self.assertEqual(response.status_code, 401, 'should be 401')

    def test_response_business(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['total', 'data'], list(response.json().keys()))
        self.assertListEqual(['business', 'mon', 'date'], list(response.json()['data'].keys()))
        self.assertIsInstance(response.json()['total'], int)
        self.assertIsInstance(response.json()['data']['business'][0], str)
        self.assertIsInstance(response.json()['data']['mon'][0], float)
        self.assertIsInstance(response.json()['data']['date'][0], str)


if __name__ == '__main__':
    unittest.main()
