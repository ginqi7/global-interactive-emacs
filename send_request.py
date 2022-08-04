#!/usr/bin/python

from pickle import NONE
import sys
from pycookiecheat import chrome_cookies
import json
import requests
from requests.auth import HTTPBasicAuth

import argparse
 
parser = argparse.ArgumentParser(description="Send a request",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("method", help="Request method")
parser.add_argument("url", help="Request URL")
parser.add_argument("-b", "--body", help="Request Body", required=False)
parser.add_argument("-u", "--user", help="Request User", required=False)
parser.add_argument("-p", "--password", help="Request Password", required=False)
args = parser.parse_args()
config = vars(args)


method = config.get("method")
url = config.get("url")
user = config.get("user")
passwod = config.get("password")
body = config.get("body")
cookies = chrome_cookies(url)



if method.lower() == 'get':
    response = requests.get(url=url, cookies=cookies, auth=HTTPBasicAuth(user, passwod))
elif method.lower() == 'post':
    response = requests.post(url=url, cookies=cookies, json=json.loads(body), auth=HTTPBasicAuth(user, passwod))
else:
    raise Exception("Sorry, not support the method") 
contentType = response.headers['content-type']
response.encoding = response.apparent_encoding
if  "text/html" in contentType :
    print(response.text)
elif  "application/json" in contentType :
    print(json.dumps(response.json()))
else :
    raise Exception("Sorry, not support the response contentType: " + contentType) 
