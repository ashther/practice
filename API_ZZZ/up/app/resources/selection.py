import re

from flask_restful import Resource, abort, reqparse
from flasgger import swag_from
from pypika import Query, Table, functions as fn

from models.db import query_db_pd
from models.users import get_group
from authy import auth
from config import NoSuchSelectionError, UnauthDataQueryError


class Selection(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('Authorization', type=str, location='headers')

    @auth.login_required
    @swag_from('api_docs/Selection.yml')
    def get(self, item):
        """
        all drop-down list option
        """
        args = self.parser.parse_args()
        authorization = args.get('Authorization')
        group, group_id = get_group(authorization)

        dict_table = Table('dict')
        account = Table('account')
        q = Query.from_(dict_table).select(dict_table.item_code.as_('id'), dict_table.item_name.as_('label'))

        if item == 'college':
            q = q.where(dict_table.type_code == 'college')
            if group == 'college':
                q = q.where(dict_table.item_code == group_id)
            elif group == 'major':
                q = q.where(dict_table.item_code.isin(
                    Query.
                    from_(dict_table).
                    select(dict_table.parent_id).
                    where((dict_table.type_code == 'major') & (dict_table.item_code == group_id))
                ))
            elif group == 'ego':
                q = q.where(dict_table.item_name == (
                    Query.from_(account).select(account.college).where(account.accnum == group_id)
                ))
        elif item == 'major':
            q = q.where(dict_table.type_code == 'major')
            if group == 'college':
                q = q.where(dict_table.parent_id == group_id)
            elif group == 'major':
                q = q.where(dict_table.item_code == group_id)
            elif group == 'ego':
                q = q.where(dict_table.item_code == (
                    Query.from_(account).select(account.major_id).where(account.accnum == group_id)
                ))
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
        # group_auth_verify(group, group_id, level='ego', id=None)

        dict_table = Table('dict')
        account = Table('account')
        q = Query.\
            from_(account).\
            select(account.accnum.as_('id'), 'percode', 'name', 'sex', 'major', 'college').\
            where((account.percode == ':q') | (account.name == ':q'))
        q_college = Query.\
            from_(dict_table).\
            select('item_name').\
            where((dict_table.type_code == 'college') & (dict_table.item_code == group_id))

        if group == 'all':
            pass
        elif group == 'college':
            q = q.where(account.college == q_college)
        elif group == 'major':
            q = q.where(account.major_id == group_id)
        else:
            raise UnauthDataQueryError

        sql = re.sub('\'(:\w+)\'', '\\1', str(q))
        # sql = re.sub('substring\(', 'substr(', sql, flags=re.I)
        df = query_db_pd(sql, args)

        return df.to_dict(orient='list')
