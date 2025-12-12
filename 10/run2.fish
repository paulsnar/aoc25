#!/bin/fish

set -l total 0
while read -l line
	set -l re (venv/bin/python sol2.py $line)
	echo "$re"
	set -l inc (echo $re | sed -e 's/ /+/g' | bc)
	set total (expr $total + $inc)
end
echo $total
