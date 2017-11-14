#!/bin/sh
# wget https://github.com/trizen/clyrics/archive/master.zip
# rm -Rf clyrics-master/*
# unzip master.zip
# rm master.zip
# cpan
# cpan -u
# cpan -Ti WWW::Mechanize
# cpan -Ti LWP::Protocol::https
./clyrics-master/clyrics "$1 - $2"
