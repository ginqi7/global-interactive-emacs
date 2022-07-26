#!/usr/bin/python

from pickle import NONE
import sys
from pycookiecheat import chrome_cookies
import json
import requests


if (len(sys.argv) < 3):
    raise Exception("There need least 3 arguments") 

method = sys.argv[1]
url = sys.argv[2]

cookies = chrome_cookies(url)

if method.lower() == 'get':
    response = requests.get(url=url, cookies=cookies)
elif method.lower() == 'post':
    body = NONE
    if (len(sys.argv) >= 4):
        body = sys.argv[3]
    response = requests.post(url=url, cookies=cookies, json=json.loads(body))
else:
    raise Exception("Sorry, not support the method") 
print(json.dumps(response.json()))
