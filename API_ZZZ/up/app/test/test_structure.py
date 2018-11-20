import requests
import unittest
import copy

URL = 'http://localhost:5000'


class StructureActive(unittest.TestCase):

    endpoint = '/structure/active'
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
        self.assertListEqual(['sex', 'politics', 'area', 'nation'], list(response.json().keys()))
        self.assertIsInstance(response.json()['sex']['sex'][0], str)
        self.assertIsInstance(response.json()['sex']['n'][0], int)
        self.assertIsInstance(response.json()['politics']['politics'][0], str)
        self.assertIsInstance(response.json()['politics']['n'][0], int)
        self.assertIsInstance(response.json()['area']['area'][0], str)
        self.assertIsInstance(response.json()['area']['n'][0], int)
        self.assertIsInstance(response.json()['nation']['nation'][0], str)
        self.assertIsInstance(response.json()['nation']['n'][0], int)


class StructureAllTime(unittest.TestCase):

    endpoint = '/structure/allTime'
    auth = ('admin', 'admin')
    args = {
        'level': 'all'
    }

    def test_wrong_user(self):
        response = requests.get(URL + self.endpoint, auth=('test', 'admin'))
        self.assertEqual(response.status_code, 403, 'should be 403')

    def test_response(self):
        response = requests.get(URL + self.endpoint, params=self.args, auth=self.auth)
        self.assertEqual(response.status_code, 200, 'should be 200')
        self.assertListEqual(['sex', 'area', 'politics', 'nation', 'area_simple', 'nation_simple'],
                             list(response.json().keys()))
        self.assertIsInstance(response.json()['sex']['year'][0], str)
        self.assertIsInstance(response.json()['sex']['data'][0]['label'], str)
        self.assertTrue((response.json()['sex']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['sex']['data'][0]['value'][0], int)))

        self.assertIsInstance(response.json()['area']['year'][0], str)
        self.assertIsInstance(response.json()['area']['data'][0]['label'], str)
        self.assertTrue((response.json()['area']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['area']['data'][0]['value'][0], int)))

        self.assertIsInstance(response.json()['politics']['year'][0], str)
        self.assertIsInstance(response.json()['politics']['data'][0]['label'], str)
        self.assertTrue((response.json()['politics']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['politics']['data'][0]['value'][0], int)))

        self.assertIsInstance(response.json()['nation']['year'][0], str)
        self.assertIsInstance(response.json()['nation']['data'][0]['label'], str)
        self.assertTrue((response.json()['nation']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['nation']['data'][0]['value'][0], int)))

        self.assertIsInstance(response.json()['area_simple']['year'][0], str)
        self.assertIsInstance(response.json()['area_simple']['data'][0]['label'], str)
        self.assertTrue((response.json()['area_simple']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['area_simple']['data'][0]['value'][0], int)))

        self.assertIsInstance(response.json()['nation_simple']['year'][0], str)
        self.assertIsInstance(response.json()['nation_simple']['data'][0]['label'], str)
        self.assertTrue((response.json()['nation_simple']['data'][0]['value'][0] is None) or
                        (isinstance(response.json()['nation_simple']['data'][0]['value'][0], int)))


if __name__ == '__main__':
    unittest.main()
