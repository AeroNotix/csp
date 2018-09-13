CSP
===


This implements CSP by providing channels.

I am well, well, aware that there are other implementations but they
all fail in a very simple way: their implementations of `select` all
thrash the CPU when none of the clauses are met.

This makes `select` not useful in the general sense. Essentially uses
of `select` in other libraries act as a CPU warmer when none of the
send/recv clauses are met in a single cycle.

This library aims to deal with that.
