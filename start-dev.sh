#!/bin/bash

node=${1}
if [ -z ${node} ]; then
    node="rot-dev"
fi

erl -sname ${node} -pa deps/*/ebin ebin -s lager -s sync -s rot
