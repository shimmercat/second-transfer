
Issues
======

These are some issues which are relatively easy to solve but
according to priorities I should not solve them now :-(

Space leak
----------

There are some hastables at Framer.hs whose entries are not cleared
on stream termination. There is code already for that, namely the function
`finishFlowControlForStream` but somehow it breaks the session... I
think that some operation is done where the dictionary entries
are required. Given time, it should be straightforward to fix this
issue. A workaround that could also ameliorate slowloris risks is to
cut sessions after a fixed time.
