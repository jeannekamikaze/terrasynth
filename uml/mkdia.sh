#!/bin/sh

for file in `ls *.txt`; do
	echo "Processing $file..."
	java -jar plantuml.jar $file
done
