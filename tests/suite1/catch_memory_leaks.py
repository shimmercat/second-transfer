
from enum import Enum

import numpy as np
import math
from scipy.stats import t as studentt

class MemoryType(Enum):
    VIRTUAL_SIZE = 1
    RESIDENT = 2
    SHARE = 3
    TEXT = 4
    LIB = 5
    DATA = 6
    DT = 7


class LeakDetector(object):
    def __init__(self, process_pid ):
        self._pid = process_pid
        self._record = []

    def sample(self):
        with open("/proc/{0}/statm".format(self._pid), "r", encoding="ascii") as fin:
            contents = fin.readline()
        numbers = list( map(int, contents.split(" ")) )
        self._record.append(numbers)

    def leaks(self, sample_size=5, use_memory_type=MemoryType.DATA, significance=0.05):
        if 2*sample_size > len(self._record):
            raise ValueError("Too litle data")
        latest_sample = np.array(self._record[:-sample_size])
        prelatest_sample  = np.array(self._record[-2*sample_size: -sample_size])
        column_index = use_memory_type.value-1

        u2 = latest_sample[:, column_index].mean()
        s2 = latest_sample[:, column_index].std()

        u1 = prelatest_sample[:, column_index].mean()
        s1 = prelatest_sample[:, column_index].std()

        denominator = s1*s1 / sample_size + s2*s2 / sample_size
        if denominator <= 0:
            return False
        t = (u1 - u2) / math.sqrt( denominator )
        dist = studentt(sample_size - 1)
        prob =  dist.cdf( t )
        one_tail = prob if prob < 0.5 else 1.0 - prob
        return one_tail < significance