from flask import Flask, url_for, request, jsonify
from flask_sqlalchemy import SQLAlchemy
import json
import datetime

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql+mysqlconnector://user:password@127.0.0.1:3306'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)


class Node(db.Model):
    address = db.Column(db.String(80), primary_key=True, nullable=False)
    data = db.Column(db.Text(), nullable=False)
    added_time = db.Column(db.DateTime(), nullable=False)


@app.route('/')
def index():
    return 'index'


@app.route('/network', methods=['GET', 'POST'])
def network():
    if request.method == 'POST':
        # weird how its read all as a key...
        data = json.loads(list(request.form.keys())[0])
        node = Node(
            address=data['address'],
            data=str(data),
            added_time=datetime.datetime.utcnow()
        )
        found = Node.query.filter_by(address=node.address).first()
        if found is None:
            db.session.add(node)
            db.session.commit()
        elif found.added_time < datetime.datetime.utcnow() - datetime.timedelta(minutes=1):
            found.data = str(data)
            found.added_time = datetime.datetime.utcnow()
            db.session.add(found)
            db.session.commit()
        return "post"
    else:
        nodes = Node.query.all()
        ret = []
        for node in nodes:
            print(node.data.replace("'", "\""))
            ret.append(json.loads(node.data.replace("'", "\"")))
        return jsonify(ret)


with app.test_request_context():
    print(url_for('network'))
    db.create_all()
