#!/usr/bin/python3

###############################################################################
#
# Copyright 2019-present GEANT RARE project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed On an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
###############################################################################

from ..bf_gbl_env.cst_env import *

# import re, subprocess, os

# tmp bf shell scripts
FREERTR_BFSHELL_SCRIPT = "/tmp/sh_tna_ports.bfsh"

# Parse the bf shell show -a command
def tnaparser():
    port_list = []
    "Parse show -a command from bfshell"
    generatebfshell(None, None, True)
    tna_ports = subprocess.run(
        ["/opt/bf_switchd/install/bin/bfshell -f {}".format(FREERTR_BFSHELL_SCRIPT)],
        stdout=subprocess.PIPE,
        shell=True,
    )
    for tna_line in (str(tna_ports.stdout)).split("\\n"):
        tnasplit = tna_line.split("|")

        if re.match("^\d+\/\d+\s+$", tnasplit[0]):
            port_list.append(
                TNAPort(
                    tnasplit[0],
                    tnasplit[1],
                    tnasplit[2],
                    tnasplit[3],
                    tnasplit[4],
                    tnasplit[5],
                    tnasplit[6],
                    tnasplit[7],
                    tnasplit[8],
                    tnasplit[9],
                    tnasplit[10],
                    tnasplit[11],
                    tnasplit[12],
                    tnasplit[13],
                )
            )
    return port_list


# Geneate the bf shell script required to get ports and mac addresses
def generatebfshell(port, channel, allport):
    "Generate the bf shell required to extract ports and mac addresses"
    try:
        bf_shell = open(FREERTR_BFSHELL_SCRIPT, "w")
        if allport:
            bf_shell.write(
                """ucli
pm
show -a
..
end
exit
"""
            )
        else:
            bf_shell.write(
                """ucli
bf_pltfm
chss_mgmt
port_mac_get {0} {1}
..
..
end
exit
""".format(
                    port, channel
                )
            )
    finally:
        bf_shell.close()
    return


def getmacaddr(portlist):
    tnaportlist = portlist
    "Execute then get the mac addr from the generated bfshell"
    for tnaport in tnaportlist:
        portchan = tnaport.port.split("/")
        port = portchan[0]
        chann = portchan[1]
        generatebfshell(port, chann, False)
        result = subprocess.run(
            [
                "/opt/bf_switchd/install/bin/bfshell -f {}".format(
                    FREERTR_BFSHELL_SCRIPT
                )
            ],
            stdout=subprocess.PIPE,
            shell=True,
        )
        re_mac = re.compile(r"(?:[0-9a-fA-F]:?){12}")
        tnaport.macaddr = re.findall(re_mac, str(result.stdout))[0]
    return tnaportlist


class TNAPort:
    def __init__(
        self,
        port,
        mac,
        dp,
        ppt,
        speed,
        fec,
        an,
        kr,
        rdy,
        adm,
        opr,
        lpbk,
        framesrx,
        framestx,
        macaddr=None,
    ):
        self.port = port
        self.mac = mac
        self.dp = dp
        self.ppt = ppt
        self.speed = speed
        self.fec = fec
        self.an = an
        self.kr = kr
        self.rdy = rdy
        self.adm = adm
        self.opr = opr
        self.lpbk = lpbk
        self.framesrx = framesrx
        self.framestx = framestx
        self.macaddr = macaddr
