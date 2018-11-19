import logging
from logging.handlers import RotatingFileHandler


class Config():
    DATABASE = 'D:/Documents/R Scripts/db_userProfile/db.sqlite'
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
    DATABASE = 'sqlite:///db/db.sqlite'
    SECRET_KEY = 'a very complicated secret key'
    PORT = 8005


swagger_template = {
    'info': {
        'title': 'some title',
        'description': 'some description',
        'version': '0.1.0'
    },
    'securityDefinitions': {
        'basicAuth': {
            'type': 'basic'
        }
    }
}

handler = RotatingFileHandler('log/flask.log', maxBytes=100000, backupCount=5, encoding='utf-8')
handler.setLevel(logging.ERROR)
