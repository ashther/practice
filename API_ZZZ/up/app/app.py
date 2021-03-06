import os
from time import strftime

from flask import Flask, g, jsonify, request
from flask_restful import Api
from flask_cors import *
from flasgger import Swagger, swag_from

from config import *
from authy import auth
from models.users import db, get_group
from resources.regular import Regular, RegularPerson
from resources.active_user import ActiveTotal, ActiveDaily, ActiveLost
from resources.structure import StructureActive, StructureAllTime
from resources.deal_count import DealCountDaily, DealCountDetail
from resources.deal_mon import DealMonDaily, DealMonDetail, DealMonTransaction
from resources.deal_category import DealCategoryTotal, DealCategoryDaily, DealCategoryDetail
from resources.selection import Selection, Search


app = Flask(__name__)
CORS(app=app, supports_credentials=True)
app.config.from_object(DevelopmentConfig)


api = Api(app, errors=http_errors)
swagger = Swagger(app, template=swagger_template)
db.init_app(app)
app.logger.addHandler(handler)


@app.after_request
def after_request(response):
    """
    log on every response
    """
    ts = strftime('[%Y-%m-%d %H:%M:%S]')
    if response.status_code < 400:
        app.logger.info(
            '%s %s %s %s %s',
            ts,
            request.remote_addr,
            request.method,
            request.full_path,
            response.status
        )
    elif response.status_code >= 400:
        app.logger.error(
            '%s %s %s %s %s',
            ts,
            request.remote_addr,
            request.method,
            request.full_path,
            response.status
        )
    return response


@app.after_request
def redirect_no_auth(response):
    """
    prevent web browser from invoking 401 error, changing status code to 403
    """
    if response.status_code == 401:
        response.status_code = 403
        # d = response.get_data()
        # response.data = json.dumps({'message': str(d.decode())})
    return response


@app.teardown_appcontext
def close_connection(exception):
    """
    close database connection
    """
    db = getattr(g, '_database', None)
    if db is not None:
        db.close()

# @app.route('/api/users', methods=['POST'])
# def new_user():
#     username = request.json.get('username')
#     password = request.json.get('password')
#     if username is None or password is None:
#         abort(400)    # missing arguments
#     if User.query.filter_by(username=username).first() is not None:
#         abort(400)    # existing user
#     user = User(username=username)
#     user.hash_password(password)
#     db.session.add(user)
#     db.session.commit()
#     return (jsonify({'username': user.username}), 201,
#             {'Location': url_for('get_user', id=user.id, _external=True)})


@app.route('/token')
@auth.login_required
@swag_from('resources/api_docs/token.yml')
def get_auth_token():
    """
    get token when user log in, and the token will be used as auth in the other API call
    """
    exp_time = app.config['EXP_TIME']
    token = g.user.generate_auth_token(exp_time)

    group, group_id = get_group(request.headers['Authorization'])
    return jsonify({'token': token.decode('ascii'), 'duration': exp_time, 'group': group, 'group_id': group_id})


from resources.group_auth_test import GroupAuthVerifyTest
api.add_resource(GroupAuthVerifyTest, '/group_auth_test')

api.add_resource(Selection, '/selection/<string:item>')
api.add_resource(Search, '/search')

api.add_resource(DealCountDaily, '/deal/count/daily')
api.add_resource(DealCountDetail, '/deal/count/detail')
api.add_resource(DealMonDaily, '/deal/mon/daily')
api.add_resource(DealMonDetail, '/deal/mon/detail')
api.add_resource(DealMonTransaction, '/deal/mon/transaction')

api.add_resource(DealCategoryTotal, '/deal/category/total')
api.add_resource(DealCategoryDaily, '/deal/category/daily')
api.add_resource(DealCategoryDetail, '/deal/category/detail')

api.add_resource(ActiveTotal, '/activeUser/total')
api.add_resource(ActiveDaily, '/activeUser/daily')
api.add_resource(ActiveLost, '/activeUser/lost')

api.add_resource(StructureActive, '/structure/active')
api.add_resource(StructureAllTime, '/structure/allTime')

api.add_resource(Regular, '/regularGroup')
api.add_resource(RegularPerson, '/regularPerson/<string:item>')

if __name__ == '__main__':
    if not os.path.exists('db/user.sqlite'):
        db.create_all()
    app.run(host=app.config['HOST'], port=app.config['PORT'], debug=app.config['DEBUG'])
