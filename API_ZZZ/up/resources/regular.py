from flask_restful import Resource, reqparse, abort
from models.db import query_db, query_db_pd
import pandas as pd
import numpy as np
import re
from pypika import Query, Table


class Regular(Resource):
    parser = reqparse.RequestParser()
    parser.add_argument('level', type=str, location='args', required=True)
    parser.add_argument('id', type=str, location='args')
    parser.add_argument('degree', type=str, location='args')
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)
    parser.add_argument('sex', type=str, location='args')
    parser.add_argument('area', type=str, location='args')
    parser.add_argument('yearIn', type=int, location='args')

    def get(self):
        try:
            # get parameters
            args = self.parser.parse_args()
            level = args.get('level')
            degree = args.get('degree')
            sex = args.get('sex')
            area = args.get('area')
            year_in = args.get('yearIn')

            # define table and query object for query builder
            regular = Table('regular')
            account = Table('account')
            dict_table = Table('dict')

            q = Query.from_(regular).select(
                regular.regular
            ).where(
                (regular.date >= ':startDate') &
                (regular.date <= ':endDate')
            )
            q_sub = Query.from_(account).select(account.accnum)

            # re-define sub query based on level value
            if level == 'all':
                pass
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
            elif level == 'major':
                q_sub = q_sub.where(account.major_id == ':id')
            else:
                abort(400, message='not correct level parameter')

            # check parameters
            if (degree is not None) and (degree != '全部'):
                q_sub = q_sub.where(account.degree == ':degree')
            if sex is not None:
                q_sub = q_sub.where(account.sex == ':sex')
            if area is not None:
                q_sub = q_sub.where(account.area == ':area')
            if year_in is not None:
                args['yearIn'] = '_' + str(year_in) + '%'
                q_sub = q_sub.where(account.percode.like(':yearIn'))

            q = q.where(regular.accnum.isin(q_sub))
            sql = re.sub('\'(:\w+)\'', '\\1', str(q))

            df = query_db_pd(sql, args)

            # stat
            regular_mean = df['regular'].mean()

            bins = np.linspace(0, 1, 21)
            df['group'] = pd.cut(df['regular'], bins, include_lowest=True)
            df = df.groupby(df['group']).count()
            df['regular'] = df['regular'] / df['regular'].sum()
            df.reset_index(inplace=True)
            df['group'] = df['group'].astype(str)

            return {
                'data': {
                    'group': df['group'].tolist(),
                    'regular': df['regular'].tolist()
                },
                'regularMean': regular_mean if not np.isnan(regular_mean) else None
            }

        except Exception as e:
            abort(500, message=str(e))


class RegularPerson(Resource):
    parser = reqparse.RequestParser()
    # parser.add_argument('item', type=str, location='json', required=True)
    parser.add_argument('id', type=str, location='args', required=True)
    parser.add_argument('startDate', type=str, location='args', required=True)
    parser.add_argument('endDate', type=str, location='args', required=True)

    def get(self, item):
        try:
            args = self.parser.parse_args()
            sql = 'select {item}, date from {item} where date >= :startDate and date <= :endDate and accnum = :id'
            sql = sql.format(**dict(item=item))

            df = query_db_pd(sql, args)

            full_date = pd.DataFrame({'date': pd.date_range(args.get('startDate'), args.get('endDate'))})
            full_date['date'] = full_date['date'].dt.strftime('%Y-%m-%d')
            df = pd.merge(full_date, df, how='left', on='date').fillna(0)

            mean = df[item].mean()

            return {
                'data': {
                    'date': df['date'].tolist(),
                    'item': df[item].tolist()
                },
                'mean': mean if not np.isnan(mean) else None
            }

        except Exception as e:
            abort(500, message=str(e))
