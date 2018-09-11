from flask import Flask, request
from flask_restful import Resource, Api, reqparse, abort
import json

app = Flask(__name__)
api = Api(app)

balances = {}

parser_post_user = reqparse.RequestParser()
parser_post_user.add_argument('user')
parser_post_user.add_argument('password')

parser_operation = reqparse.RequestParser()
parser_operation.add_argument('quantity', type=int)

parser_account = reqparse.RequestParser()
parser_account.add_argument('operation', type=str)
parser_account.add_argument('quantity', type=int)
parser_account.add_argument('to_user', type=str)
parser_account.add_argument('to_account', type=str)

parser_create_account = reqparse.RequestParser()
parser_create_account.add_argument('account_name', type=str)

operations = ["deposit", "withdraw", "transfer"]

def error(status, message=""):
    return {"status": status, "message": message}

class SingleAccount(Resource):
    def get(self, user, account):
        if user not in balances:
            return error(404, "user {} not found".format(user)), 404
        if account not in balances[user]:
            return error(404, "account {} of user {} not found".format(acccount, user)), 404
        return {"account_name": account,
                "balance": balances[user][account]}

    def post(self, user, account):
        args = parser_account.parse_args()
        operation = args['operation']
        quantity  = args['quantity']
        # Possible errors
        if operation is None:
            return error(422, "operation attribute not found"), 422
        if quantity is None:
            return error(422, "quantity attribute not found"), 422
        if user not in balances:
            return error(404, "user {} not found".format(user)), 404
        if account not in balances[user]:
            return error(404, "account {} of user {} not found".format(account, user)), 404
        if operation not in operations:
            return error(409, "operation {} not recognized".format(operation)),  409
        if operation == "deposit":
            balances[user][account] += quantity
            print(balances)
        if operation == "withdraw":
            if (balances[user][account] - quantity) < 0:
                return error(409, "Aborted. Negative balance after withdraw"),  409
            balances[user][account] -= quantity
        if operation == "transfer":
            to_user = args['to_user']
            if to_user is None:
                return error(422, "to_user: Destination user necessary for transfer operation"), 422
            to_account = args['to_account']
            if to_account is None:
                return error(422, "to_account: Destination user necessary for transfer operation"), 422
            if to_user not in balances:
                return error(404, "user {} not found".format(to_user)), 404
            if to_account not in balances[user]:
                return error(404, "account {} of user {} not found".format(to_account, to_user)), 404
            if (balances[user][account] - quantity) < 0:
                return error(409, "Aborted. Negative balance after withdraw"),  409
            balances[user][account] -= quantity
            balances[to_user][to_account] += quantity
        return {"account_name": account,
                "balance": balances[user][account]}, 201


class UserAccounts(Resource):
    location = "<string:user>/accounts/"
    def post(self, user):
        if user not in balances:
            return error(404, "user {} not found".format(user)), 404
        args = parser_create_account.parse_args()
        account = args['account_name']
        if account is None:
            return error(422, "account_name attribute missing"), 422
        if account in balances[user]:
            return error(409, "account {} of user {}  already exists".format(account, user)), 409
        balances[user][account] = 0
        return { "account_name" : account,
                 "balance": balances[user][account]
                 }, 201, {'Content-Location': "{}{}".format(request.base_url, account)}


class Balances(Resource):
    location = "/bank/users/"
    def get(self):
        return { "balances" : balances }

    def post(self):
        args = parser_post_user.parse_args()
        user = args['user']
        if user == "":
            return error(422, "Empty string not permitted"), 422
        password = args['password']
        if user is not None and password is not None:
            user = user.replace(" ", "-")
            if user not in balances:
                balances[user] = {}
                return {"user" : user}, 201, {'Content-Location': request.base_url + user}
            else:
                return error(409, "user already exists"), 409
        else:
            return error(422, "user and/or password missing"), 422

api.add_resource(Balances, Balances.location)
api.add_resource(UserAccounts, Balances.location + UserAccounts.location)
api.add_resource(SingleAccount, Balances.location + UserAccounts.location + "<string:account>")

if __name__ == '__main__':
    app.run(debug=True)
