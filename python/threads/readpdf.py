from timeout import run_with_timeout
import PyPDF2
import time


def fun_ret(a1):
    time.sleep(a1)
    return 1


def fun_more_args(a1, a2, a3):
    time.sleep(a1 + a2 + a3)
    return 3


def fun_no_ret(a1):
    time.sleep(a1)


def fun_no_args():
    time.sleep(3)


time_start = time.perf_counter()

print("Running fun_ret...")
retval = run_with_timeout(1, fun_ret, 5)
print("Done! retval: ", retval)

print("Running fun_no_ret...")
run_with_timeout(3, fun_no_ret, 1)
print("Done!")

print("Running fun_more_args...")
retval = run_with_timeout(3, fun_more_args, 1, 0, 0)
print("Done! retval: ", retval)

print("Running fun_no_args...")
run_with_timeout(1, fun_no_args)
print("Done!")

time_stop = time.perf_counter()

time_elapsed = time_stop - time_start
print("Time elapsed: ", time_elapsed)
