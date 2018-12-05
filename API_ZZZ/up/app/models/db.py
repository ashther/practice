from flask import g, current_app as app
import sqlite3
import pandas as pd

from config import UnauthDataQueryError


def make_dict(cursor, row):
    return dict((cursor.description[idx][0], value) for idx, value in enumerate(row))


def get_db():
    """
    建立数据库连接
    :return: 返回数据库连接
    """
    db = getattr(g, '_database', None)
    if db is None:
        db = g._database = sqlite3.connect(app.config['DATABASE'])
        db.row_factory = make_dict
    return db


def query_db(query, args=(), one=False):
    """
    建立查询
    :return: 返回查询结果
    """
    cursor = get_db().execute(query, args)
    result = cursor.fetchall()
    cursor.close()
    return (result[0] if result else None) if one else result


def query_db_pd(query, args=()):
    df = pd.read_sql_query(query, sqlite3.connect(app.config['DATABASE']), params=args)
    return df
