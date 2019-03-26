#! /usr/bin/env bash

openscad='/Users/michi/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD'

parallel "$openscad" -o {.}.stl {} ::: output/*.scad
