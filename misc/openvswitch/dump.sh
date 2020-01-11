#!/bin/bash
ovs-ofctl -O OpenFlow11 dump-groups sw
ovs-ofctl dump-flows sw
sleep 1
