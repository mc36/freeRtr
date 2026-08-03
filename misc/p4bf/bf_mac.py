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

import re, subprocess, os

SDE = os.environ.get("SDE", "/opt/bf_switchd")
SDE_INSTALL = os.environ.get("SDE_INSTALL", SDE + "/install")
FREERTR = os.environ.get("FREERTR", SDE + "/opt/freertr")
FREERTR_CONFIG = os.environ.get("FREERTR_CONFIG", "/etc/freertr")

# File result (witch mac addresses)
FREERTR_CONFIG_TNA_MAC = FREERTR_CONFIG + "/tna_ports_mac.cfg"

# tmp bf shell scripts
FREERTR_BFSHELL_SCRIPT = "/tmp/sh_tna_ports.bfsh"


tnaportlist = []

# Parse the bf shell show -a command
def tnaparser():
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
            tnaportlist.append(
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
    return


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


def getmacaddr():
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
    return


def generatetnaporttemp():
    "Generate the final file with mac addresses"
    tna_ports_mac = None
    try:
        tna_ports_mac = open(FREERTR_CONFIG_TNA_MAC, "w")
        tna_ports_mac.write(
            """-----+----+---+----+-------+----+--+--+---+---+---+--------+----------------+----------------+-----------------+-
PORT |MAC |D_P|P/PT|SPEED  |FEC |AN|KR|RDY|ADM|OPR|LPBK    |FRAMES RX       |FRAMES TX       |MACADDR          |E
-----+----+---+----+-------+----+--+--+---+---+---+--------+----------------+----------------+-----------------+-
"""
        )
        tnaport_index = 0
        for tnaport in tnaportlist:
            tnaport_index += 1
            tna_ports_mac.write(
                "{0}|{1}|{2}|{3}|{4}|{5}|{6}|{7}|{8}|{9}|{10}|{11}|{12}|{13}|{14}|-\n".format(
                    tnaport.port,
                    tnaport.mac,
                    tnaport.dp,
                    tnaport.ppt,
                    tnaport.speed,
                    tnaport.fec,
                    tnaport.an,
                    tnaport.kr,
                    tnaport.rdy,
                    tnaport.adm,
                    tnaport.opr,
                    tnaport.lpbk,
                    tnaport.framesrx,
                    tnaport.framestx,
                    tnaport.macaddr,
                )
            )
            if tnaport_index % 40 == 0:
                tna_ports_mac.write(
                    """-----+----+---+----+-------+----+--+--+---+---+---+--------+----------------+----------------+-----------------+-
PORT |MAC |D_P|P/PT|SPEED  |FEC |AN|KR|RDY|ADM|OPR|LPBK    |FRAMES RX       |FRAMES TX       |MACADDR          |E
-----+----+---+----+-------+----+--+--+---+---+---+--------+----------------+----------------+-----------------+-
"""
                )
    finally:
        tna_ports_mac.close()
    return


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


if os.path.exists(FREERTR_BFSHELL_SCRIPT):
    print(
        "The file {} does exist. Delete it before running the script".format(
            FREERTR_BFSHELL_SCRIPT
        )
    )
    exit()
tnaparser()
getmacaddr()
generatetnaporttemp()
os.remove(FREERTR_BFSHELL_SCRIPT)
print("Done, file saved at {}".format(FREERTR_CONFIG_TNA_MAC))
