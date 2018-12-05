import requests
import unittest
import copy

URL_TOKEN = 'http://localhost:5000/token'
URL = 'http://localhost:5000/group_auth_test'


class GroupAuthVerify(unittest.TestCase):

    def test_cuser(self):
        token = requests.get(URL_TOKEN, auth=('cuser', 'cuser')).json()['token']

        response_all_no = requests.get(URL, params={'level': 'all', 'id': None}, auth=(token, ''))
        response_college_in = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_college_notin = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_major_in = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_major_notin = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_ego_in = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))
        response_ego_notin = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))

        self.assertEqual(response_all_no.status_code, 420)
        self.assertEqual(response_college_in.status_code, 200)
        self.assertEqual(response_college_notin.status_code, 420)
        self.assertEqual(response_major_in.status_code, 200)
        self.assertEqual(response_major_notin.status_code, 420)
        self.assertEqual(response_ego_in.status_code, 200)
        self.assertEqual(response_ego_notin.status_code, 420)

    def test_muser(self):
        token = requests.get(URL_TOKEN, auth=('muser', 'muser')).json()['token']

        response_all_no = requests.get(URL, params={'level': 'all', 'id': None}, auth=(token, ''))
        response_college_in = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_college_notin = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_major_in = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_major_notin = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_ego_in = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))
        response_ego_notin = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))

        self.assertEqual(response_all_no.status_code, 420)
        self.assertEqual(response_college_in.status_code, 420)
        self.assertEqual(response_college_notin.status_code, 420)
        self.assertEqual(response_major_in.status_code, 200)
        self.assertEqual(response_major_notin.status_code, 420)
        self.assertEqual(response_ego_in.status_code, 200)
        self.assertEqual(response_ego_notin.status_code, 420)

    def test_euser(self):
        token = requests.get(URL_TOKEN, auth=('euser', 'euser')).json()['token']

        response_all_no = requests.get(URL, params={'level': 'all', 'id': None}, auth=(token, ''))
        response_college_in = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_college_notin = requests.get(URL, params={'level': 'college', 'id': ''}, auth=(token, ''))
        response_major_in = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_major_notin = requests.get(URL, params={'level': 'major', 'id': ''}, auth=(token, ''))
        response_ego_in = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))
        response_ego_notin = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))
        response_ego_self = requests.get(URL, params={'level': 'ego', 'id': ''}, auth=(token, ''))

        self.assertEqual(response_all_no.status_code, 420)
        self.assertEqual(response_college_in.status_code, 420)
        self.assertEqual(response_college_notin.status_code, 420)
        self.assertEqual(response_major_in.status_code, 420)
        self.assertEqual(response_major_notin.status_code, 420)
        self.assertEqual(response_ego_in.status_code, 420)
        self.assertEqual(response_ego_notin.status_code, 420)
        self.assertEqual(response_ego_self.status_code, 200)


if __name__ == '__main__':
    unittest.main()
