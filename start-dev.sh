#!/bin/bash

erl -sname rot-dev -pa deps/*/ebin apps/*/ebin -s lager -s sync -s rots -s rotc
