#!/bin/bash
pyflakes "$1"
pep8 --ignore=E501 --repeat "$1"
true
