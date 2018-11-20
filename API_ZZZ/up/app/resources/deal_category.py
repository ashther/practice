import re

from flask_restful import Resource, reqparse, abort, inputs
from flasgger import swag_from
import pandas as pd
from pypika import Query, Table, functions as fn, JoinType

from models.db import query_db_pd
from authy import auth


type_names = {
                'canteen': '饮食',
                'network': '网络',
                'bath': '洗浴',
                'bus': '校车',
                'electricity': '电费',
                'park': '停车',
                'hotel': '客房',
                'sport': '健身',
                'store': '购物',
                'library': '图书馆',
                'medical': '医疗',
                None: '其他'
}


class DealCategoryTotal(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    # parser.add_argument('isAvg', type=inputs.boolean, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    @swag_from('api_docs/DealCategoryTotal.yml')
    @auth.login_required
    def get(self):
        """
        total times number for kinds of deal category
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')

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
            query_table = {
                'all': Table('ndeal_business_college_type'),
                'college': Table('ndeal_business_college_type'),
                'major': Table('ndeal_business_major_type'),
                'ego': Table('acc_ndeal')
            }.get(level)
            q = Query.\
                from_(query_table).\
                select(query_table.type, fn.Sum(query_table.n).as_('n')).\
                where((query_table.date >= ':startDate') & (query_table.date <= ':endDate')).\
                groupby(query_table.type)

            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(query_table.college == q_college_sub)
            elif level == 'major':
                q = q.where(query_table.major == q_major_sub)
            elif level == 'ego':
                q = q.where(query_table.accnum == ':id')
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)
            df.type.replace(type_names, inplace=True)

            return df.to_dict(orient='list')

        except Exception as e:
            abort(500, message=str(e))


class DealCategoryDaily(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('isAvg', type=inputs.boolean, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    @swag_from('api_docs/DealCategoryDaily.yml')
    @auth.login_required
    def get(self):
        """
        daily times number for kinds of deal category
        """
        try:
            args = self.parser.parse_args()
            level = args.get('level')
            is_avg = args.get('isAvg')

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
            query_table = {
                'all': Table('ndeal_business_college_type'),
                'college': Table('ndeal_business_college_type'),
                'major': Table('ndeal_business_major_type'),
                'ego': Table('acc_ndeal')
            }.get(level)
            q = Query. \
                from_(query_table). \
                select(query_table.date, query_table.type, fn.Sum(query_table.n).as_('n')). \
                where((query_table.date >= ':startDate') & (query_table.date <= ':endDate')). \
                groupby(query_table.date, query_table.type)

            if level != 'ego' and is_avg:
                query_table_avg = {
                    'all': Table('au_business_college'),
                    'college': Table('au_business_college'),
                    'major': Table('au_business_major')
                }.get(level)
                q_avg = Query. \
                    from_(query_table_avg). \
                    join(dict_table, how=JoinType.left). \
                    on((query_table_avg.business == dict_table.item_name) &
                       (dict_table.type_code == 'business')). \
                    select(query_table_avg.date,
                           dict_table.remark.as_('type'),
                           fn.Sum(query_table_avg.n).as_('n')). \
                    where((query_table_avg.date >= ':startDate') &
                          (query_table_avg.date <= ':endDate')). \
                    groupby(query_table_avg.date, dict_table.remark)

            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(query_table.college == q_college_sub)
                if is_avg:
                    q_avg = q_avg.where(query_table_avg.college == q_college_sub)
            elif level == 'major':
                q = q.where(query_table.major == q_major_sub)
                if is_avg:
                    q_avg = q_avg.where(query_table_avg.major == q_major_sub)
            elif level == 'ego':
                q = q.where(query_table.accnum == ':id')
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)

            if level != 'ego' and is_avg:
                sql_avg = re.sub('\'(:\w+)\'', '\\1', str(q_avg))
                df_avg = query_db_pd(sql_avg, args)
                df = pd.merge(df, df_avg, how='left', on=['date', 'type'])
                df['n'] = df['n_x'] / df['n_y']
                df = df[['date', 'type', 'n']]

            df = df.pivot('date', 'type', 'n')

            full_date = pd.DataFrame({'date': pd.date_range(args.get('startDate'), args.get('endDate'))})
            full_date['date'] = full_date['date'].dt.strftime('%Y-%m-%d')
            df = pd.merge(full_date, df, how='left', on='date')
            df = df.where((pd.notnull(df)), None)
            # df.reset_index(inplace=True)

            return {
                'date': list(df.date),
                'data': [{'label': type_names.get(x, '其他'), 'value': list(df[x])} for x in df.columns if x != 'date']
            }

        except Exception as e:
            abort(500, message=str(e))


class DealCategoryDetail(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)
    parser.add_argument('sex', type=str, location='args')
    parser.add_argument('area', type=str, location='args')
    parser.add_argument('yearIn', type=str, location='args')

    @swag_from('api_docs/DealCategoryDetail.yml')
    @auth.login_required
    def get(self):
        """
        daily times number for different user group in kinds of deal category
        """
        try:

            args = self.parser.parse_args()
            level = args.get('level')
            sex = args.get('sex')
            area = args.get('area')
            year_in = args.get('yearIn')

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

            if sex is not None:
                query_table = {
                    'all': Table('ndeal_sex_college_type'),
                    'college': Table('ndeal_sex_college_type'),
                    'major': Table('ndeal_sex_major_type')
                }.get(level)
                q_sub = query_table.sex == ':sex'
            elif area is not None:
                query_table = {
                    'all': Table('ndeal_area_college_type'),
                    'college': Table('ndeal_area_college_type'),
                    'major': Table('ndeal_area_major_type')
                }.get(level)
                q_sub = query_table.area == ':area'
            elif year_in is not None:
                query_table = {
                    'all': Table('ndeal_year_college_type'),
                    'college': Table('ndeal_year_college_type'),
                    'major': Table('ndeal_year_major_type')
                }.get(level)
                q_sub = query_table.year == ':yearIn'
            else:
                raise ValueError('there must be one parameter applied in sex, area and yearIn')

            q = Query.\
                from_(query_table).\
                select(query_table.date, query_table.type, fn.Sum(query_table.n).as_('n')).\
                where((query_table.date >= ':startDate') & (query_table.date <= ':endDate') & q_sub).\
                groupby(query_table.date, query_table.type)
            if level == 'all':
                pass
            elif level == 'college':
                q = q.where(query_table.college == q_college_sub)
            elif level == 'major':
                q = q.where(query_table.major == q_major_sub)
            else:
                raise ValueError('not correct level parameter')

            sql = re.sub('\'(:\w+)\'', '\\1', str(q))
            df = query_db_pd(sql, args)
            df = df.pivot('date', 'type', 'n')

            full_date = pd.DataFrame({'date': pd.date_range(args.get('startDate'), args.get('endDate'))})
            full_date['date'] = full_date['date'].dt.strftime('%Y-%m-%d')
            df = pd.merge(full_date, df, how='left', on='date')
            df = df.where((pd.notnull(df)), None)
            # df.reset_index(inplace=True)

            return {
                'date': list(df.date),
                'data': [{'label': type_names.get(x, '其他'), 'value': list(df[x])} for x in df.columns if x != 'date']
            }

        except Exception as e:
            abort(500, message=str(e))
