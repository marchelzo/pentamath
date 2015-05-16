#!/usr/bin/env python

from flask import Flask, session, redirect, url_for, escape, request, render_template
from werkzeug.security import generate_password_hash, check_password_hash
import re
import psycopg2
import uuid
import redis
import traceback
from os import getenv

USERNAME_REGEX = re.compile('^[a-zA-Z0-9]+$')

def error(s):
    return '{"success": false, "error": "%s"}' % (s,), 200, {'Content-Type': 'application/json; charset=utf-8'}

def success():
    return '{"success": true}', 200, {'Content-Type': 'application/json; charset=utf-8'}

# retrieve necessary environment variables
DB_USER = getenv('PENTAMATH_DB_USER')
if DB_USER is None:
    raise Exception('DB_USER not set')
HOST = getenv('PENTAMATH_HOST')
if HOST is None:
    raise Exception('PENTAMATH_HOST not set')

db_connection = psycopg2.connect('dbname=pentamath user=' + DB_USER)
cursor = db_connection.cursor()
redis_connection = redis.StrictRedis(host='localhost', port=6379, db=0)
app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/signup', methods=['GET', 'POST'])
def signup():
    if request.method == 'GET':
        return render_template('signup.html', host=HOST)

    username = request.form['username']

    # ensure validity of username and password
    user_len = len(username)
    pass_len = len(request.form['password'])
    if user_len < 5 or user_len > 30:
        return error('Username must be between 5 and 30 characters')
    if pass_len < 5 or pass_len > 300: 
        return error('Password must be at least 5 characters')
    if USERNAME_REGEX.match(username) is None:
        return error('Username must not contain any special characters')


    # ensure username uniqueness
    cursor.execute('SELECT 1 FROM users WHERE username = %s', (username,))
    if cursor.rowcount > 0:
        return error('Username is already taken')

    # register user
    hash = generate_password_hash(request.form['password'])
    cursor.execute('INSERT INTO users (username, password) VALUES (%s, %s) RETURNING id', (username, hash))

    user_id = int(cursor.fetchone()[0])

    # initialize user's gameplay statistics
    cursor.execute('INSERT INTO stats VALUES (%s, 0, 0, 0, 0, 0)', (user_id,))

    # commit changes to db
    db_connection.commit()

    # log the user in for convenience
    session['username'] = username

    response = app.make_response(success())

    uid = str(uuid.uuid4())
    session['uid'] = uid
    session['username'] = username
    redis_connection.setex(uid, 12000, username)
    response.set_cookie('pentamath-uid', uid)

    return response

@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'GET':
        if 'username' in session:
            return redirect(url_for('play'), code=302)
        return render_template('login.html')
    try:
        cursor.execute('SELECT username, password FROM users WHERE LOWER(username) = LOWER(%s)', (request.form['username'],))
        if cursor.rowcount != 1:
            return error('Invalid username or password')
        row = cursor.fetchone()
        stored_hash = row[1].split()[0]
        if not check_password_hash(stored_hash, request.form['password']):
            return error('Invalid username or password')

        uid = str(uuid.uuid4())
        session['uid'] = uid
        session['username'] = row[0]
        redis_connection.setex(uid, 12000, row[0])
        response = app.make_response(redirect(url_for('play'), code=302))
        response.set_cookie('pentamath-uid', uid)
        return response
    
    except Exception:
        print(traceback.format_exc())
        return error('Invalid username or password')
        

@app.route('/logout')
def logout():
    if 'uid' in session:
        redis_connection.delete(session['uid'])
    session.clear()
    return redirect(url_for('index'))

@app.route('/play')
def play():
    if 'username' in session: # logged in
        return render_template('play.html', host=HOST)
    else:  # not logged in
        return redirect(url_for('login'), code=302)

@app.route('/practice')
def practice():
    if 'username' in session:
        return render_template('practice.html', host=HOST)
    else:
        return redirect(url_for('login'), code=302)

@app.route('/room/<user>')
def room(user):
    if 'username' in session:
        return render_template('lobby.html', host=HOST, owner=user, own='false', username=session['username'])
    else:
        return redirect(url_for('login'), code=302)

@app.route('/submitscore', methods=['POST'])
def submit_score():
    if 'username' not in session: return error('')
    username = session['username']
    cursor.execute('SELECT id FROM users WHERE username = %s', (username,))
    user_id = int(cursor.fetchone()[0])
    cursor.execute('UPDATE stats SET ' + request.form['gameMode'] + ' = ' + request.form['gameMode'] + ' + 1, correct = correct + %s, total = total + 5 WHERE user_id = %s',
            (int(request.form['correct']), user_id))
    db_connection.commit()

    return success()

@app.route('/createroom', methods=['POST'])
def create_room():
    if 'username' not in session: return error('')

    print(request.form['playing'])
    
    if request.form['playing'] == 'yes':
        return render_template('lobby.html', own='true', difficulty=request.form['difficulty'], owner=session['username'], host=HOST, username=session['username'])
    else:
        return render_template('spectate.html', difficulty=request.form['difficulty'], host=HOST, own='false')

@app.route('/lobby')
def lobby():
    return render_template('lobby.html')

@app.before_request
def update_session():
    pass
    if 'username' in session:
        redis_connection.expire(session['uid'], 120000)


# set the secret key.  keep this really secret:
app.secret_key = '\x93\x9b}D%k\x11\x14!\xec\xa8"xvX\xb3\x1f\x82\xe4\x07\xf3@\xce\xc9'

if __name__ == '__main__':
    app.run('0.0.0.0', debug=True)
