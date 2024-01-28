from flask import Flask, request

app = Flask(__name__)

@app.route('/forms/enroll', methods=['POST'])
def enroll():
    print("Received data:")
    for key in request.form:
        print(f"{key}: {request.form[key]}")
    return "Form data received."

if __name__ == '__main__':
    app.run(debug=True)
