import logging
from concurrent_log_handler import ConcurrentRotatingFileHandler
from flask_restful import HTTPException


class Config():
    DATABASE = 'db/db.sqlite'
    SECRET_KEY = 'test secret key'
    SQLALCHEMY_DATABASE_URI = 'sqlite:///db/user.sqlite'
    SQLALCHEMY_COMMIT_ON_TEARDOWN = True
    SQLALCHEMY_TRACK_MODIFICATIONS = True
    JSON_AS_ASCII = False
    DEBUG = False
    HOST = '0.0.0.0'
    PORT = 5000


class DevelopmentConfig(Config):
    DEBUG = True


class ProductionConfig(Config):
    SECRET_KEY = 'a very complicated secret key'
    PORT = 8005


swagger_template = {
    'info': {
        'title': '',
        'description': '',
        'version': '0.1.0'
    },
    'securityDefinitions': {
        'basicAuth': {
            'type': 'basic'
        }
    }
}

handler = ConcurrentRotatingFileHandler('log/flask.log', 'a', maxBytes=10000, backupCount=5, encoding='utf-8')
handler.setLevel(logging.ERROR)


class UnauthDataQueryError(HTTPException):
    code = 420


class LevelParamError(HTTPException):
    code = 400


class AtLeastOneParamError(HTTPException):
    code = 400


class NoSuchSelectionError(HTTPException):
    code = 400


http_errors = {
    'UnauthDataQueryError': {
        'message': 'Unauthorized Data Query',
        'status': 420
    },
    'LevelParamError': {
        'message': 'not correct level parameter',
        'status': 400
    },
    'AtLeastOneParamError': {
        'message': 'there must be one parameter applied in business, sex, area and yearIn',
        'status': 400
    },
    'NoSuchSelectionError': {
        'message': 'no such selection',
        'status': 400
    }
}
