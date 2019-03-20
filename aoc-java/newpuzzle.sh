#!/bin/bash

YEAR=$1; shift
DAY=$1; shift

cat >src/aoc$YEAR/Day$DAY.java <<EOF
package aoc$YEAR;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.FileReader;

import org.junit.Test;

public class Day$DAY {
    @Test
    public void testDay$DAY() throws Exception {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/$YEAR/day$DAY.txt"))) {
            String line;
	    while ((line = reader.readLine()) != null) {
	    	  String s[] = line.split(" ");
		  // TODO continue
	    }
	}
    }       
}
EOF
