from flask import Flask, request, redirect, render_template
from datetime import datetime
from contextlib import contextmanager
import fcntl
import os

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

def decrease_seats(month):
    """Decrease the number of available seats for the given session."""
    current_dir = os.path.dirname(os.path.abspath(__file__))
    main_html_file = f'{current_dir}/index.html'
    with locked_file(main_html_file, 'r+') as f:
        lines = f.readlines()
        new_lines = []
        for line in lines:
            if f'{month}_SEATS_LEFT'.upper() in line:
                seats_left = int(line.split('-->')[1].split(' ')[0])
                if seats_left > 1:
                    line = line.replace(f'{seats_left}', f'{seats_left - 1}')
                elif seats_left == 1:
                    line = '<td colspan=2>FULLY BOOKED</td>'
            new_lines.append(line)

        f.seek(0)
        f.writelines(new_lines)

@app.route('/forms/payed', methods=['POST'])
def payed():
    # - FIXME check stripe's secret
    # - FIXME use stripe API to detect failure
    app.logger.info(request.form)
    if request.form['success'] != 'true':
        return redirect('../payment_failed.html')

    # - Add payer email to the list of valid referrers
    # FIXME: use stripe API to get the email
    email = "toto@example.com"
    with locked_file("/var/lib/handsonpython/referrers.txt", 'a') as f:
        f.write(f"{email}\n")

    # Decrease the number of available seats
    # FIXME: get the session back from stripe, and validate it
    session = 'march'
    decrease_seats(session)

    # - add payer to referrer's leaderbord
    # FIXME: use stripe API to get the referrer and the amount
    referrer = 'remi@a'
    amount = 42
    with locked_file("/var/lib/handsonpython/payouts.txt", 'a') as f:
        f.write(f"{datetime.now().isoformat()},{email},{session},{referrer},{amount}\n")

    return render_template('payed.html', email=email)



def valid_referrer(email):
    """Return True iff the given email is an authorized referrer"""
    with locked_file("/var/lib/handsonpython/referrers.txt", 'r') as f:
        for line in f.readlines():
            if email.lower().strip() == line.lower().strip():
                return True
    return False

@app.route('/forms/enroll', methods=['POST'])
def enroll():
    discount = 0
    referrer = None
    app.logger.info(request)
    if 'referrer_email' in request.form \
       and request.form['referrer_email'] \
       and not valid_referrer(request.form['referrer_email']):
        return render_template('enroll_bad_ref.html', email=request.form['referrer_email'])
    elif 'referrer_email' in request.form \
       and request.form['referrer_email'] \
       and valid_referrer(request.form['referrer_email']):
        discount += 10  # Referrer discount
        referrer = request.form['referrer_email']

    if 'session' not in request.form \
       or not request.form['session'] \
       or request.form['session'] not in ['march', 'april']:
        return redirect('/enroll_bad_session.html')
    if request.form['session'] == 'april':
        discount += 10  # Early bird discount

    if 'community_boost' in request.form \
       and request.form['community_boost'] == 'on':
        discount += 20  # Community discount
    return redirect(f'../stripe{discount}.html?session={request.form["session"]}&referrer={referrer if referrer is not None else "0"}')

@app.route('/forms/subscribe', methods=['POST'])
def subscribe():
    if "subscriber_email" not in request.form \
       or not request.form["subscriber_email"]:
        return redirect('/submissingemail.html')
    with locked_file("/var/lib/handsonpython/subscribers.txt", 'a') as f:
        f.write(f"{datetime.now().isoformat()}, {request.form['subscriber_email']}\n")
    return render_template('subscribed.html', email=request.form['subscriber_email'])

def disable_radio(booking_date):
    """Disable the radio button for the given one on one date, and add an
    'Already booked' text to its label"""
    current_dir = os.path.dirname(os.path.abspath(__file__))
    main_html_file = f'{current_dir}/index.html'
    with locked_file(main_html_file, 'r+') as f:
        lines = f.readlines()
        new_lines = []
        for line in lines:
            if booking_date in line:
                # Add the 'disabled' attribute to the input tag
                line = line.replace(f'value="{booking_date}"', f'value="{booking_date}" disabled')
                # Append '[Already booked]' to the label content
                line = line.replace('Paris</label>', 'Paris [Already booked]</label>')
            new_lines.append(line)

        f.seek(0)
        f.writelines(new_lines)

@app.route('/forms/ooo', methods=['POST'])
def ooo():
    if "subscriber_email" not in request.form \
       or not request.form["subscriber_email"]:
        return redirect('/ooomissingemail.html')
    if 'date' not in request.form:
        return redirect("../ooomissingdate.html")
    with locked_file("/var/lib/handsonpython/ooo.txt", 'a') as f:
        f.write(f"{request.form['date']}, {request.form['subscriber_email']} \n")
    human_readable_date = datetime.strptime(
        request.form['date'],
        '%Y%m%dT%H:%M').strftime("%B %d, %Y at %H:%M, Europe/Paris")
    disable_radio(request.form['date'])
    return render_template('ooo.html', date=human_readable_date, email=request.form['subscriber_email'])

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')


# def rest_enroll_form():
#     """Dummy function to keep form testing code that I, for now, run interactively"""
#     ref = [None, '', ]
