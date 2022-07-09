import signal
import contextlib


class SignalAlarmException(Exception):
    """
    Generic Exception for handling timeouts with SIGALRM
    """
    pass


def _signal_timeout_handler(signum, frame):
    """
    Simply raises a SignalAlarmException which works as a timeout implementation.
    """
    raise SignalAlarmException()


def run_with_timeout(time_limit, fn, *args):
    """

    """
    with _timeout(time_limit):
        try:
            return fn(*args)
        except SignalAlarmException:
            return None


@contextlib.contextmanager
def _timeout(seconds=5):
    """

    """
    original_handler = signal.signal(signal.SIGALRM, _signal_timeout_handler)
    signal.alarm(seconds)

    try:
        yield
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, original_handler)
