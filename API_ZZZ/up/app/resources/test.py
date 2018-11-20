from flask_restful import Resource
from flasgger import swag_from
from flask import current_app as app

from authy import auth


class Test(Resource):

    @auth.login_required
    @swag_from('api_docs/test.yml')
    def get(self):
        return {'msg': 'ok, used auth'}