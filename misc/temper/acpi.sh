#!/bin/sh
acpi -t | cut -d' ' -f4
echo done
