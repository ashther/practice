import re

from flask_restful import Resource, reqparse, abort
from flasgger import swag_from
import pandas as pd
from pypika import Query, Table, functions as fn, JoinType, Case

from models.db import query_db_pd
from authy import auth


class StructureActive(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    @swag_from('api_docs/StructureActive.yml')
    @auth.login_required
    def get(self):
        """
        active users structure
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')

            dict_table = Table('dict')
            account = Table('account')
            acc_ndeal = Table('acc_ndeal')

            q_college_sub = Query.\
                from_(account).\
                select(account.accnum).\
                where(account.major_id.isin(
                    Query.
                        from_(dict_table).
                        select(dict_table.item_code).
                        where((dict_table.type_code == 'major') &
                        (dict_table.parent_id == ':id'))
                ))
            q_major_sub = Query.\
                from_(account).\
                select(account.accnum).\
                where(account.major_id == ':id')

            q = Query.\
                from_(acc_ndeal).\
                join(account, how=JoinType.left).\
                on(acc_ndeal.accnum == account.accnum).\
                select(acc_ndeal.accnum, account.percode, account.sex, account.area, account.politics, account.nation).\
                distinct().\
                where((acc_ndeal.date >= ':startDate') & (acc_ndeal.date <= ':endDate'))

            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(acc_ndeal.accnum.isin(q_college_sub))
            elif level == 'major':
                q = q.where(acc_ndeal.accnum.isin(q_major_sub))
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)

            df.loc[df.area.notnull() & (df.area != '甘肃省'), 'area'] = '省外'
            df.loc[df.area == '甘肃省', 'area'] = '省内'
            df.loc[df.nation.notnull() & (df.nation != '汉族'), 'nation'] = '少数民族'

            items = ['sex', 'politics', 'area', 'nation']
            result = {}
            for item in items:
                try:
                    result[item] = df.groupby(item).\
                        size().\
                        reset_index(name='n').\
                        sort_values('n', ascending=False).\
                        to_dict(orient='list')
                except Exception as e:
                    result[item] = None

            return result

        except Exception as e:
            abort(500, message=str(e))


class StructureAllTime(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')

    @swag_from('api_docs/StructureAllTime.yml')
    @auth.login_required
    def get(self):
        """
        all users structure, which are in account table
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')

            dict_table = Table('dict')
            account = Table('account')

            q = Query.\
                from_(account).\
                select(fn.Substring(account.percode, 2, 4).as_('year'),
                       # Case()
                       # .when(account.sex == '男', 'm')
                       # .when(account.sex == '女', 'f')
                       # .else_('o').as_('sex'),
                       account.sex,
                       account.area,
                       account.politics,
                       account.nation)

            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(account.major_id.isin(
                    Query.
                        from_(dict_table).
                        select(dict_table.item_code).
                        where((dict_table.type_code == 'major') &
                              (dict_table.parent_id == ':id'))
                ))
            elif level == 'major':
                q = q.where(account.major_id == ':id')
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            sql = re.sub('substring\(', 'substr(', sql, flags=re.I)

            df = query_db_pd(sql, args)

            df['area_simple'] = df['area']
            df.loc[df.area_simple.notnull() & (df.area_simple != '甘肃省'), 'area_simple'] = '省外'
            df.loc[df.area_simple == '甘肃省', 'area_simple'] = '省内'
            df['nation_simple'] = df['nation']
            df.loc[df.nation_simple.notnull() & (df.nation_simple != '汉族'), 'nation_simple'] = '少数民族'

            result = {}
            for item in df.columns:
                if item == 'year':
                    continue
                try:
                    temp = df.groupby(['year', item]).\
                        size().\
                        reset_index(name='n').\
                        pivot('year', item, 'n').\
                        reset_index()
                    full_date = pd.DataFrame({'year': pd.date_range(min(temp.year), max(temp.year), freq='YS')})
                    full_date['year'] = full_date['year'].dt.strftime('%Y')
                    temp = pd.merge(full_date, temp, how='left', on='year')
                    temp = temp.where((pd.notnull(temp)), None)
                    result[item] = {
                        'year': list(temp.year),
                        'data': [{'label': x, 'value': list(temp[x])} for x in temp.columns if x != 'year']
                    }

                except Exception as e:
                    result[item] = None

            return result

        except Exception as e:
            abort(500, message=str(e))