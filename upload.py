from flask import Flask, request
import os
import datetime
app = Flask(__name__)


@app.route('/upload/<student_id>/<filename>', methods=['PUT'])
def upload(student_id, filename):
    time = datetime.datetime.now()
    wd = os.path.normpath(os.getcwd())
    dirpath = os.path.abspath(os.path.normpath(student_id))
    dirpath = os.path.join(dirpath, time.isoformat())
    filepath = os.path.join(dirpath, filename)
    assert filepath.startswith(wd), "Can't write anywhere else than in a subfolder: {filepath} not in {wd}".format(filepath=filepath, wd=wd)
    assert len(request.data) < 10000000, "{} bytes is too much, please don't kill my hard drive".format(len(request.data))
    os.makedirs(dirpath)
    with open(filepath, "w") as f:
        f.write(request.data.decode('utf-8'))
    return 'Correctly wrote {} bytes in {}'.format(len(request.data), filepath)

if __name__ == '__main__':
    app.run(host='0.0.0.0')
