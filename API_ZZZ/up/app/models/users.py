import base64

from flask import current_app as app
from flask_sqlalchemy import SQLAlchemy
from passlib.apps import custom_app_context as pwd_context
from itsdangerous import TimedJSONWebSignatureSerializer as Serializer, BadSignature, SignatureExpired

db = SQLAlchemy()


class User(db.Model):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(32), index=True)
    password_hash = db.Column(db.String(64))
    group = db.Column(db.String(32))
    group_id = db.Column(db.String(32))

    def hash_password(self, password):
        """ use this when insert user into user-db manually """
        self.password_hash = pwd_context.encrypt(password)

    def verify_password(self, password):
        return pwd_context.verify(password, self.password_hash)

    def generate_auth_token(self, expiration=600):
        s = Serializer(app.config['SECRET_KEY'], expires_in=expiration)
        return s.dumps({'id': self.id, 'group': self.group, 'group_id': self.group_id})

    @staticmethod
    def verify_auth_token(token):
        s = Serializer(app.config['SECRET_KEY'])
        try:
            data = s.loads(token)
        except SignatureExpired:
            return None    # valid token, but expired
        except BadSignature:
            return None    # invalid token
        user = User.query.get(data['id'])
        return user


def get_group(authorization):
    """
    get user group from token in the request
    """
    authorization = authorization.replace('Basic ', '')
    authorization = base64.b64decode(authorization).decode()
    token = authorization.split(':')[0]

    user = User.query.filter_by(username=token).first()
    if user is not None:
        return user.group, user.group_id
    else:
        s = Serializer(app.config['SECRET_KEY'])
        try:
            data = s.loads(token)
        except BadSignature:
            return None, None
        return data.get('group', None), data.get('group_id', None)
