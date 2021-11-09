from flask import Flask, url_for, request, jsonify
from flask_sqlalchemy import SQLAlchemy
import json
import datetime

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:////tmp/test.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)


class Node(db.Model):
    address = db.Column(db.String(80), primary_key=True, nullable=False)
    out_links = db.Column(db.Text(), nullable=False)
    in_links = db.Column(db.Text(), nullable=False)
    not_accepting_conn_links = db.Column(db.Text(), nullable=False)
    none_p2p_links = db.Column(db.Text(), nullable=False)
    is_accepting_connections = db.Column(db.Text(), nullable=False)
    added_time = db.Column(db.DateTime(), nullable=False)

    def toJSON(self):
        # TODO: Make this safe... probably better to store in multiple tables
        data = {
            'address': self.address,
            'out_links': eval(self.out_links),
            'in_links': eval(self.in_links),
            'not_accepting_conn_links': eval(self.not_accepting_conn_links),
            'none_p2p_links': eval(self.none_p2p_links),
            'is_accepting_connections': self.is_accepting_connections,
            'added_time': self.added_time.isoformat()
        }
        return data


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
            out_links=str(data['out_links']),
            in_links=str(data['in_links']),
            not_accepting_conn_links=str(data['not_accepting_conn_links']),
            none_p2p_links=str(data['none_p2p_links']),
            is_accepting_connections=data['is_accepting_connections'],
            added_time=datetime.datetime.utcnow()
        )
        found = Node.query.filter_by(address=node.address).first()
        if found is None:
            db.session.add(node)
            db.session.commit()
        elif found.added_time < datetime.datetime.utcnow() - datetime.timedelta(minutes=1):
            found.out_links = str(data['out_links'])
            found.in_links = str(data['in_links'])
            found.not_accepting_conn_links = str(data['not_accepting_conn_links'])
            found.none_p2p_links = str(data['none_p2p_links'])
            found.is_accepting_connections = data['is_accepting_connections']
            found.added_time = datetime.datetime.utcnow()
            db.session.add(found)
            db.session.commit()
        return "post"
    else:
        nodes = Node.query.all()
        ret = []
        for node in nodes:
            ret.append(node.toJSON())
        return jsonify(ret)


with app.test_request_context():
    print(url_for('network'))
    # db.create_all()
