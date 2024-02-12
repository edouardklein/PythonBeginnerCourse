from flask import Flask, request, redirect, render_template
from datetime import datetime
from contextlib import contextmanager
import fcntl

app = Flask(__name__)

@contextmanager
def locked_file(file_path, mode='a'):
    """ Context manager for locking a file. """
    with open(file_path, mode) as file:
        fcntl.flock(file.fileno(), fcntl.LOCK_EX)
        try:
            yield file
        finally:
            fcntl.flock(file.fileno(), fcntl.LOCK_UN)

@app.route('/forms/enroll', methods=['POST'])
def enroll():
    for key in request.form:
        app.logger.info(f"{key}: {request.form[key]}")
    return "Form data received."

@app.route('/forms/subscribe', methods=['POST'])
def subscribe():
    if "subscriber_email" not in request.form:
        return redirect('/')
    with locked_file("/var/lib/handsonpython/subscribers.txt", 'a') as f:
        f.write(request.form['subscriber_email'] + "\n")
    return redirect("../subscribed.html")

def disable_radio(file_path, booking_date):
    # Read the content of the HTML file
    with open(file_path, 'r') as file:
        lines = file.readlines()

    # Modify the corresponding line
    new_lines = []
    for line in lines:
        if booking_date in line:
            # Add the 'disabled' attribute to the input tag
            line = line.replace(f'value="{booking_date}"', f'value="{booking_date}" disabled')
            # Append '[Already booked]' to the label content
            line = line.replace('</label>', ' [Already booked]</label>')
        new_lines.append(line)

    # Write the modified content back to the file
    with open(file_path, 'w') as file:
        file.writelines(new_lines)

@app.route('/forms/ooo', methods=['POST'])
def ooo():
    if "subscriber_email" not in request.form:
        return redirect('/')
    if 'date' not in request.form:
        return redirect("../missingdate.html")
    with locked_file("/var/lib/handsonpython/ooo.txt", 'a') as f:
        f.write(f"{request.form['date']}, {request.form['subscriber_email']} \n")
    human_readable_date = datetime.strptime(
        request.form['date'],
        '%Y%m%dT%H:%M').strftime("%B %d, %Y at %H:%M")
    return render_template('ooo.html', date=human_readable_date)

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')
