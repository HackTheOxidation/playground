import time
import multiprocessing


def wait(seconds):
    time.sleep(seconds)


process = multiprocessing.Process(target=wait, args=(10,))

time_start = time.perf_counter()

process.start()
print("Started process. Waiting 2 seconds for time out.")

process.join(2)
process.terminate()
print("Process stopped.")

time_stop = time.perf_counter()

time_elapsed = time_stop - time_start

print(f"Process exit code: {process.exitcode}. Time elapsed: {time_elapsed}.")
