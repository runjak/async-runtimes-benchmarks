#!/bin/sh

/usr/bin/time -f "%M" ./Main 1
/usr/bin/time -f "%M" ./Main 10000
/usr/bin/time -f "%M" ./Main 100000
/usr/bin/time -f "%M" ./Main 1000000
