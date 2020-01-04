#!/bin/bash

# Source code is together with all the other Java solutions, this
# script is just to make it possible to run with the other olpd
# challenge implementations.

javac -Xlint:unchecked -source 11 -target 11 Day03.java
java -ea -cp . Day03
