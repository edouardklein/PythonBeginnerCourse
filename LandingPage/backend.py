from flask import Flask, request, redirect, render_template
import stripe
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

@app.route('/forms/payed/<session_id>')
def payed(session_id):
    app.logger.info(request)
    try:
        checkout_session = stripe.checkout.Session.retrieve(session_id)
        app.logger.info(checkout_session)
        customer_email = checkout_session['customer_details']['email']
        with locked_file("/var/lib/handsonpython/referrers.txt", 'a') as f:
            f.write(f"{customer_email}\n")

        month = request.args.get('utm_content')
        assert month in ['june', 'july']
        decrease_seats(month)

        referrer = request.args.get('utm_medium')
        if referrer:
            amount = 42
            with locked_file("/var/lib/handsonpython/payouts.txt", 'a') as f:
                f.write(f"{datetime.now().isoformat()},{customer_email},{month},{referrer},{amount}\n")
        return render_template('payed.html', email=customer_email)
    except Exception as e:
        app.logger.warning(e)
        return redirect('../payment_failed.html')


def valid_referrer(email):
    """Return True iff the given email is an authorized referrer"""
    with locked_file("/var/lib/handsonpython/referrers.txt", 'r') as f:
        for line in f.readlines():
            if email.lower().strip() == line.lower().strip():
                return True
    return False

STRIPE_URL = {
    0:  "https://buy.stripe.com/cN28zxaaKcSj8VO28b",
    10: "https://buy.stripe.com/8wM2b9ciSaKb1tmdQU",
    20: "https://buy.stripe.com/5kA8zx3Mm2dF8VO4gl",
    30: "https://buy.stripe.com/14k8zxgz8bOf0pi006",
    40: "https://buy.stripe.com/14k4jh4QqaKb9ZS6ov",
    }
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
       or request.form['session'] not in ['june', 'july']:
        return redirect('/enroll_bad_session.html')
    if request.form['session'] == 'july':
        discount += 10  # Early bird discount

    if 'community_boost' in request.form \
       and request.form['community_boost'] == 'on':
        discount += 20  # Community discount
    return redirect(f'{STRIPE_URL[discount]}?utm_content={request.form["session"]}&utm_medium={referrer if referrer is not None else "0"}')

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
        with locked_file("/var/lib/handsonpython/ooo.txt", 'a') as f:
            f.write(f"did not select a date, {request.form['subscriber_email']} \n")
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
