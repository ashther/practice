from flask import Flask, g
from flask_restful import Api
from flask_cors import *

from resources.regular import Regular, RegularPerson

app = Flask(__name__)
CORS(app=app, supports_credentials=True)
app.config['DATABASE'] = 'db/db.sqlite'
api = Api(app)


@app.teardown_appcontext
def close_connection(exception):
    """
    关闭连接
    :param exception:
    :return:
    """
    db = getattr(g, '_database', None)
    if db is not None:
        db.close()


"""
@app.route('/testdb')
def testdb():
    sql = 'select * from regular limit ?'
    result = None
    try:
        result = query_db(sql, request.args.get('limit'))
    except Exception as e:
        abort(500)
    return jsonify({'result': result if result is not None else None})
"""

api.add_resource(Regular, '/regularGroup', endpoint='regular')
api.add_resource(RegularPerson, '/regularPerson/<string:item>', endpoint='regular_person')

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8005, debug=True)
