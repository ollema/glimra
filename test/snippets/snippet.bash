#!/bin/bash

my_var="Hello, World!"  # this is a comment

function greet() {
    echo "$my_var"
}

if [ "$1" == "test" ]; then
    greet
fi
