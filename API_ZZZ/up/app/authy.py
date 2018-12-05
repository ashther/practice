from flask_httpauth import HTTPBasicAuth
from flask import g

from models.users import User
from models.db import query_db_pd
from config import UnauthDataQueryError

auth = HTTPBasicAuth()


@auth.verify_password
def verify_password(username_or_token, password):
    # first try to authenticate by token
    user = User.verify_auth_token(username_or_token)
    if not user:
        # try to authenticate with username/password
        user = User.query.filter_by(username=username_or_token).first()
        if not user or not user.verify_password(password):
            return False
    g.user = user
    return True


def find_major_from_college(college_id):
    """
    find all major id of specific college with college_id
    """
    major_id = query_db_pd("""
        SELECT item_code
          FROM dict
         WHERE type_code = 'major' AND 
               parent_id = :id;
    """, {'id': college_id})
    return major_id.item_code.unique()


def find_major_from_account(account_id):
    """
    find major id which the user is in
    """
    major_id = query_db_pd("""
        SELECT major_id
          FROM account
         WHERE accnum = :id;
    """, {'id': account_id})
    return major_id.major_id[0]


def group_auth_verify(group, group_id, level, id):
    """
    :param group: user's group
    :param group_id: user's group id
    :param level: level in query
    :param id: id in query
    :return: if user has auth to query result of level and id
    """
    if group == 'all':
        pass
    elif group == 'college':
        if (level == 'college') and (group_id == id):
            pass
        elif (level == 'major') and (id in find_major_from_college(group_id)):
            pass
        elif (level == 'ego') and (
            (find_major_from_account(id) in find_major_from_college(group_id)) or (id is None)
        ):
            pass
        else:
            raise UnauthDataQueryError
    elif group == 'major':
        if (level == 'major') and (group_id == id):
            pass
        elif (level == 'ego') and (
            (find_major_from_account(id) == group_id) or (id is None)
        ):
            pass
        else:
            raise UnauthDataQueryError
    elif group == 'ego':
        if (level == 'ego') and (group_id == id):
            pass
        else:
            raise UnauthDataQueryError
    else:
        raise UnauthDataQueryError
