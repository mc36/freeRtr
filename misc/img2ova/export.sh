#!/bin/bash
doer()
{
echo doing $1...
vboxmanage export $1 --output $1.ova
}

doer rtr-kfreebsd32
doer rtr-kfreebsd64
doer rtr-linux32
doer rtr-linux64
