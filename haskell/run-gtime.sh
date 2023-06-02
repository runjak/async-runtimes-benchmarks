#!/bin/sh

/opt/homebrew/opt/gnu-time/bin/gtime -f "%M" ./Main 1
/opt/homebrew/opt/gnu-time/bin/gtime -f "%M" ./Main 10000
/opt/homebrew/opt/gnu-time/bin/gtime -f "%M" ./Main 100000
/opt/homebrew/opt/gnu-time/bin/gtime -f "%M" ./Main 1000000
