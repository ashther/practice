import re

from flask_restful import Resource, reqparse, abort
from flasgger import swag_from
import pandas as pd
import numpy as np
from pypika import Query, Table, functions as fn

from models.db import query_db_pd
from authy import auth


class ActiveTotal(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    @auth.login_required
    @swag_from('api_docs/ActiveTotal.yml')
    def get(self):
        """
        total active user number between start date and end date
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')

            # define table to query
            acc_ndeal = Table('acc_ndeal')
            account = Table('account')
            dict_table = Table('dict')

            q = Query.from_(acc_ndeal).select(
                fn.Count(acc_ndeal.accnum).distinct()
            ).where(
                (acc_ndeal.date >= ':startDate') &
                (acc_ndeal.date <= ':endDate')
            )
            q_sub = Query.from_(account).select(account.accnum)

            # re-define sub query based on level value
            if level == 'all':
                sql_total_users = 'select count(1) from account;'
            elif level == 'college':
                q_sub = q_sub.where(
                    account.major_id.isin(
                        Query.from_(dict_table).select(
                            dict_table.item_code
                        ).where(
                            (dict_table.type_code == 'major') &
                            (dict_table.parent_id == ':id')
                        )
                    )
                )
                sql_total_users = """SELECT count(1) 
                                      FROM account
                                     WHERE major_id IN (
                                               SELECT DISTINCT item_code
                                                 FROM dict
                                                WHERE type_code = 'major' AND 
                                                      parent_id = :id
                                           );"""
            elif level == 'major':
                q_sub = q_sub.where(account.major_id == ':id')
                sql_total_users = 'select count(1) from account where major_id = :id;'
            else:
                raise ValueError('not correct level parameter')

            q = q.where(acc_ndeal.accnum.isin(q_sub))
            sql = re.sub('\'(:\w+)\'', '\\1', str(q))

            df = query_db_pd(sql, args)
            total_users = query_db_pd(sql_total_users, args)
            ratio = None
            try:
                ratio = df.iloc[0, 0] / total_users.iloc[0, 0]
            except Exception as e:
                pass

            return {
                'total': int(df.iloc[0, 0]),
                'ratio': float(ratio) if not np.isnan(ratio) else None
            }

        except Exception as e:
            abort(500, message=str(e))


class ActiveDaily(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)
    parser.add_argument('business', type=str, location='args')
    parser.add_argument('sex', type=str, location='args')
    parser.add_argument('area', type=str, location='args')
    parser.add_argument('yearIn', type=int, location='args')

    @swag_from('api_docs/ActiveDaily.yml')
    @auth.login_required
    def get(self):
        """
        active users number in every day
        """
        try:
            # get parameters
            args = self.parser.parse_args()
            level = args.get('level')
            business = args.get('business')
            sex = args.get('sex')
            area = args.get('area')
            year_in = args.get('yearIn')

            # define dict table for quering and subqueries
            dict_table = Table('dict')

            q_college_sub = Query.\
                from_(dict_table).\
                select(dict_table.item_name).\
                where((dict_table.type_code == 'college') &
                      (dict_table.item_code == ':id'))
            q_major_sub = Query. \
                from_(dict_table). \
                select(dict_table.item_name). \
                where((dict_table.type_code == 'major') &
                      (dict_table.item_code == ':id'))

            # for daily active users detail
            if business is not None:
                query_table = {
                    'all': Table('au_business_college'),
                    'college': Table('au_business_college'),
                    'major': Table('au_business_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (query_table.business == ':business')
                )
            elif sex is not None:
                query_table = {
                    'all': Table('au_sex_college'),
                    'college': Table('au_sex_college'),
                    'major': Table('au_sex_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (
                                query_table.sex == ':sex')
                )
            elif area is not None:
                query_table = {
                    'all': Table('au_area_college'),
                    'college': Table('au_area_college'),
                    'major': Table('au_area_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (
                                query_table.area == ':area')
                )
            elif year_in is not None:
                query_table = {
                    'all': Table('au_year_college'),
                    'college': Table('au_year_college'),
                    'major': Table('au_year_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (
                                query_table.year == ':yearIn')
                )
            else:
                query_table = {
                    'all': Table('au_college'),
                    'college': Table('au_college'),
                    'major': Table('au_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate')
                )

            # no matter detail or summary, daily active users can be queried after adding filter condition
            if level == 'all':
                q = q.select(query_table.date, fn.Sum(query_table.n).as_('n')).groupby(query_table.date)
            elif level == 'college':
                q = q.select(query_table.date, query_table.n).where(query_table.college == q_college_sub)
            elif level == 'major':
                q = q.select(query_table.date, query_table.n).where(query_table.major == q_major_sub)
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)

            full_date = pd.DataFrame({'date': pd.date_range(args.get('startDate'), args.get('endDate'))})
            full_date['date'] = full_date['date'].dt.strftime('%Y-%m-%d')
            df = pd.merge(full_date, df, how='left', on='date').fillna(0)

            return df.to_dict(orient='list')

        except Exception as e:
            abort(500, message=str(e))


class ActiveLost(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('pageIndex', type=int, location='args', required=True)
    parser.add_argument('pageSize', type=int, location='args', required=True)

    @swag_from('api_docs/ActiveLost.yml')
    @auth.login_required
    def get(self):
        """
        users table, which may be in lost state
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')
            page_index = args.get('pageIndex')
            page_size = args.get('pageSize')

            lost = Table('lost')
            dict_table = Table('dict')

            q_college_sub = Query. \
                from_(dict_table). \
                select(dict_table.item_name). \
                where((dict_table.type_code == 'college') &
                      (dict_table.item_code == ':id'))
            q_major_sub = Query. \
                from_(dict_table). \
                select(dict_table.item_name). \
                where((dict_table.type_code == 'major') &
                      (dict_table.item_code == ':id'))
            q = Query.from_(lost).select(
                    'percode', 'name', 'last', 'idtype', 'idno', 'sex', 'college', 'major'
                )

            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(lost.college == q_college_sub)
            elif level == 'major':
                q = q.where(lost.major == q_major_sub)
            else:
                raise ValueError('not correct level parameter')

            q_limit = q[((page_index - 1) * page_size):page_size]
            q = Query.from_(q).select(fn.Count(1))
            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            sql_limit = re.sub('\'(:\w+)\'', '\\1', str(q_limit))

            total = query_db_pd(sql, args)
            df = query_db_pd(sql_limit, args)

            return {
                'total': int(total.iloc[0, 0]),
                'data': df.to_dict(orient='list')
            }

        except Exception as e:
            abort(500, message=str(e))
