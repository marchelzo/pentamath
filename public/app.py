#!/usr/bin/env python

from flask import Flask, session, redirect, url_for, escape, request, render_template
from werkzeug.security import generate_password_hash, check_password_hash
import re
import psycopg2
import uuid
import redis

USERNAME_REGEX = re.compile('^[a-zA-Z0-9]+$')

def error(s):
    print('error ' + s)
    return '{"success": false, "error": "%s"}' % (s,), 200, {'Content-Type': 'application/json; charset=utf-8'}

def success():
    print('success')
    return '{"success": true}', 200, {'Content-Type': 'application/json; charset=utf-8'}

db_connection = psycopg2.connect('dbname=pentamath user=bradley')
cursor = db_connection.cursor()
redis_connection = redis.StrictRedis(host='localhost', port=6379, db=0)
app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/signup', methods=['GET', 'POST'])
def signup():
    if request.method == 'GET':
        return render_template('signup.html')

    # ensure validity of username and password
    user_len = len(request.form['username'])
    pass_len = len(request.form['password'])
    if user_len < 5 or user_len > 30:
        return error('Username must be between 5 and 30 characters')
    if pass_len < 5 or pass_len > 300: 
        return error('Password must be at least 5 characters')
    if USERNAME_REGEX.match(request.form['username']) is None:
        return error('Username must not contain any special characters')


    # ensure username uniqueness
    cursor.execute('SELECT 1 FROM users WHERE username = %s', (request.form['username'],))
    if cursor.rowcount > 0:
        return error('Username is already taken')

    # register user
    hash = generate_password_hash(request.form['password'])
    cursor.execute('INSERT INTO users (username, password) VALUES (%s, %s)', (request.form['username'], hash))

    # commit changes to db
    db_connection.commit()

    return success()

@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'GET':
        return render_template('login.html')
    try:
        cursor.execute('SELECT (username, password) FROM users WHERE LOWER(username) = LOWER(%s)', (request.form['username'],))
        if cursor.rowcount != 1:
            return error('Invalid username or password')
        row = cursor.fetchone()
        stored_hash = row[1].split()[0]
        if not check_password_hash(stored_hash, request.form['password']):
            return error('Invalid username or password')

        uid = str(uuid.uuid4())
        session['username'] = row[0]
        redis_connection.setex(uid, 1200, row[0])
        response = app.make_response(success())
        response.set_cookie('pentamath-uid', uid)
        return response
    
    except Exception:
        return error('Invalid username or password')
        

@app.route('/logout')
def logout():
    # remove the username from the session if it's there
    session.pop('username', None)
    return redirect(url_for('index'))

@app.route('/play')
def play():
    if 'username' in session: # logged in
        render_template('play.html')
    else:  # not logged in
        return redirect(url_for('login'), code=302)

@app.before_request
def update_session():
    if 'username' in session:
        redis.expire(session['uid'], 1200)


# set the secret key.  keep this really secret:
app.secret_key = '\x93\x9b}D%k\x11\x14!\xec\xa8"xvX\xb3\x1f\x82\xe4\x07\xf3@\xce\xc9'

if __name__ == '__main__':
    app.run('0.0.0.0', debug=True)
