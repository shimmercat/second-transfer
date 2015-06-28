#!/usr/bin/python2

from __future__ import print_function

import csv

def main():
    print("Hello world")
    reader = csv.reader(
        delimiter='|' )
    for line in reader:
        secs = line[0]
        nanosecs = line[1]
        real_secs = secs + nanosecs / 1.0e9
        printf("{0:%02.4f} {1}")


if __name__ == '__main__':
    main()
