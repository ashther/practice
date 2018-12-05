import re

from flask_restful import Resource, abort, reqparse
from flasgger import swag_from
from pypika import Query, Table, functions as fn

from models.db import query_db_pd
from models.users import get_group
from authy import auth, group_auth_verify
from config import NoSuchSelectionError


class Selection(Resource):

    @auth.login_required
    @swag_from('api_docs/Selection.yml')
    def get(self, item):
        """
        all drop-down list option
        """

        dict_table = Table('dict')
        q = Query.from_(dict_table).select(dict_table.item_code.as_('id'), dict_table.item_name.as_('label'))

        if item == 'college':
            q = q.where(dict_table.type_code == 'college')
        elif item == 'major':
            q = q.where(dict_table.type_code == 'major')
        elif item == 'business':
            q = q.where(dict_table.type_code == 'business')
        elif item == 'sex':
            q = q.where(dict_table.type_code == 'sex')
        elif item == 'area':
            q = q.where((dict_table.type_code == 'area') &
                        (fn.Substring(dict_table.item_code, 3, 4) == '0000'))
        elif item == 'yearIn':
            q = q.where(dict_table.type_code == 'year_in')
        elif item == 'businessType':
            q = q.where(dict_table.type_code == 'business_type')
        elif item == 'degree':
            q = q.where(dict_table.type_code == 'degree')
        else:
            raise NoSuchSelectionError

        sql = re.sub('\'(:\w+)\'', '\\1', str(q))
        sql = re.sub('substring\(', 'substr(', sql, flags=re.I)

        df = query_db_pd(sql)

        return df.to_dict(orient='list')


class Search(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('Authorization', type=str, location='headers')
    parser.add_argument('q', type=str, location='args', required=True)

    @auth.login_required
    @swag_from('api_docs/Search.yml')
    def get(self):
        """
        search bar for looking up specific user
        """
        args = self.parser.parse_args()
        args['q'] = args.get('q').strip()

        authorization = args.get('Authorization')
        group, group_id = get_group(authorization)
        group_auth_verify(group, group_id, level='ego', id=None)

        sql = """
            SELECT accnum AS id,
                   percode,
                   name,
                   sex,
                   major,
                   college
              FROM account
             WHERE percode = :q
            UNION
            SELECT accnum AS id,
                   percode,
                   name,
                   sex,
                   major,
                   college
              FROM account
             WHERE name = :q;
        """
        df = query_db_pd(sql, args)

        return df.to_dict(orient='list')