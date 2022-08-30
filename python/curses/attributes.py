import curses
from curses import wrapper
from time import sleep

def main(stdscr):
    stdscr.clear()

    for i in range(11):
        hashes = i * '#'
        dots = (10 - i) * '.'
        stdscr.addstr(0, 0, f"Loading: [{hashes}{dots}]",
                curses.A_REVERSE)
        stdscr.refresh()
        sleep(0.5)

    stdscr.addstr(1, 0, f"Done.")
    stdscr.refresh()
    
    stdscr.getkey()

wrapper(main)
