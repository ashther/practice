import re

from flask_restful import Resource, reqparse, abort, inputs
from flasgger import swag_from
import pandas as pd
from pypika import Query, Table, functions as fn

from models.db import query_db_pd
from authy import auth


class DealCountDaily(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('isAvg', type=inputs.boolean, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    @auth.login_required
    @swag_from('api_docs/DealCountDaily.yml')
    def get(self):
        """
        daily deal times number
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')
            is_avg = args.get('isAvg')

            dict_table = Table('dict')
            ndeal_college = Table('ndeal_college')
            ndeal_major = Table('ndeal_major')
            au_college = Table('au_college')
            au_major = Table('au_major')
            acc_ndeal = Table('acc_ndeal')

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

            q_avg = None
            if level == 'all':
                q = Query.\
                    from_(ndeal_college).\
                    select(ndeal_college.date, fn.Sum(ndeal_college.n).as_('n')).\
                    where((ndeal_college.date >= ':startDate') & (ndeal_college.date <= ':endDate')).\
                    groupby(ndeal_college.date)
                if is_avg:
                    q_avg = Query.\
                        from_(au_college).\
                        select(au_college.date, fn.Sum(au_college.n).as_('n')). \
                        where((au_college.date >= ':startDate') & (au_college.date <= ':endDate')).\
                        groupby(au_college.date)
            elif level == 'college':
                q = Query.\
                    from_(ndeal_college).\
                    select(ndeal_college.date, ndeal_college.n).\
                    where((ndeal_college.date >= ':startDate') & (ndeal_college.date <= ':endDate')).\
                    where(ndeal_college.college == q_college_sub)
                if is_avg:
                    q_avg = Query.\
                        from_(au_college).\
                        select(au_college.date, au_college.n). \
                        where((au_college.date >= ':startDate') & (au_college.date <= ':endDate')).\
                        where(au_college.college == q_college_sub)
            elif level == 'major':
                q = Query.\
                    from_(ndeal_major).\
                    select(ndeal_major.date, ndeal_major.n). \
                    where((ndeal_major.date >= ':startDate') & (ndeal_major.date <= ':endDate')).\
                    where(ndeal_major.major == q_major_sub)
                if is_avg:
                    q_avg = Query. \
                        from_(au_major). \
                        select(au_major.date, au_major.n). \
                        where((au_major.date >= ':startDate') & (au_major.date <= ':endDate')). \
                        where(au_major.major == q_major_sub)
            elif level == 'ego':
                q = Query.\
                    from_(acc_ndeal).\
                    select(acc_ndeal.date, fn.Sum(acc_ndeal.n).as_('n')).\
                    where((acc_ndeal.date >= ':startDate')
                          & (acc_ndeal.date <= ':endDate')
                          & (acc_ndeal.accnum == ':id')).\
                    groupby(acc_ndeal.date)
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)

            if level != 'ego' and is_avg:
                sql_avg = re.sub('\'(:\w+)\'', '\\1', str(q_avg))
                df_avg = query_db_pd(sql_avg, args)
                df = pd.merge(df, df_avg, how='left', on='date').fillna(0)
                df['n'] = df['n_x'] / df['n_y']
                df = df[['date', 'n']]

            full_date = pd.DataFrame({'date': pd.date_range(args.get('startDate'), args.get('endDate'))})
            full_date['date'] = full_date['date'].dt.strftime('%Y-%m-%d')
            df = pd.merge(full_date, df, how='left', on='date').fillna(0)

            return df.to_dict(orient='list')

        except Exception as e:
            abort(500, message=str(e))


class DealCountDetail(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)
    parser.add_argument('business', type=str, location='args')
    parser.add_argument('sex', type=str, location='args')
    parser.add_argument('area', type=str, location='args')
    parser.add_argument('yearIn', type=str, location='args')

    @auth.login_required
    @swag_from('api_docs/DealCountDetail.yml')
    def get(self):
        """
        daily deal time number for different user group
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')
            business = args.get('business')
            sex = args.get('sex')
            area = args.get('area')
            year_in = args.get('yearIn')

            # define dict table for quering and subqueries
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

            # for daily active users detail
            if business is not None:
                query_table = {
                    'all': Table('ndeal_business_college'),
                    'college': Table('ndeal_business_college'),
                    'major': Table('ndeal_business_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (query_table.business == ':business')
                )
            elif sex is not None:
                query_table = {
                    'all': Table('ndeal_sex_college'),
                    'college': Table('ndeal_sex_college'),
                    'major': Table('ndeal_sex_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (query_table.sex == ':sex')
                )
            elif area is not None:
                query_table = {
                    'all': Table('ndeal_area_college'),
                    'college': Table('ndeal_area_college'),
                    'major': Table('ndeal_area_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (query_table.area == ':area')
                )
            elif year_in is not None:
                query_table = {
                    'all': Table('ndeal_year_college'),
                    'college': Table('ndeal_year_college'),
                    'major': Table('ndeal_year_major')
                }.get(level)
                q = Query.from_(query_table).where(
                    (query_table.date >= ':startDate') & (query_table.date <= ':endDate') & (query_table.year == ':yearIn')
                )
            else:
                raise ValueError('there must be one parameter applied, in business, sex, area or yearIn')

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