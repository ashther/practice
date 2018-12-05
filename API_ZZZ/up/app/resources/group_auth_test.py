from flask_restful import Resource, reqparse, abort
from flasgger import swag_from

from models.users import get_group
from authy import auth, group_auth_verify


class GroupAuthVerifyTest(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('Authorization', type=str, location='headers')
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')

    @auth.login_required
    @swag_from('api_docs/GroupAuthVerifyTest.yml')
    def get(self):
        args = self.parser.parse_args()
        level = args.get('level')
        id = args.get('id')

        authorization = args.get('Authorization')
        group, group_id = get_group(authorization)
        group_auth_verify(group, group_id, level, id)

        return 'ok'
