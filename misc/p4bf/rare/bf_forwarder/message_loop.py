from logging import exception
from ..bf_gbl_env.cst_env import *


def run(self):
    logger.warning("BfForwarder - Main")
    logger.warning("BfForwarder - Entering message loop")
    while not self.die:

        try:
            line = self.file_r.readline(8192)
        except Exception as e:
            e = sys.exc_info()[0]
            logger.warning(
                "%s - exited with code [%s]" % (self.class_name, _Exception())
            )
            self.tearDown()

        if len(line) == 0:
            logger.warning("BfForwarder - Empty message from control plane")
            logger.warning(
                "BfForwarder: connection with control plane lost, quitting ..."
            )
            os._exit(0)
            self.bfgc.interface.tear_down_stream()
            self.file.close()
            sys.exit(0)
            continue

        splt = line.split(" ")
        cmds = splt[0].split("_")
        mode = 0
        if len(cmds) > 1:
            if cmds[1] == "add":
                mode = 1
            if cmds[1] == "mod":
                mode = 2
            if cmds[1] == "del":
                mode = 3

        if cmds[0] != "keepalive" or not self.no_log_keepalive:
            logger.warning("rx: %s", splt)

        if cmds[0] == "route4":
            addr = splt[1].split("/")
            self.writeForwardRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
            )
            continue

        if cmds[0] == "labroute4":
            addr = splt[1].split("/")
            self.writeGlobRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
            )
            continue

        if cmds[0] == "vpnroute4":
            addr = splt[1].split("/")
            self.writeVpnRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if cmds[0] == "srvroute4":
            addr = splt[1].split("/")
            self.writeSrvRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                splt[5],
            )
            continue

        if cmds[0] == "polroute4":
            addr = splt[1].split("/")
            self.writePolkaRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                splt[5],
            )
            continue

        if cmds[0] == "myaddr4":
            addr = splt[1].split("/")
            self.writeMyaddrRules4(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "cpulabel":
            self.writeCpuMplsRules(
                mode,
                int(splt[1]),
            )
            continue

        if cmds[0] == "label4":
            self.writeMplsRules(
                mode,
                int(splt[1]),
                int(splt[4]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "unlabel4":
            self.writeUnMplsRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "mylabel4":
            self.writeMyMplsRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "mysrv4":
            self.writeMySrv4rules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
            )
            continue

        if cmds[0] == "nshfwd":
            self.writeNshFwdRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                int(splt[6]),
                int(splt[7]),
            )
            continue

        if cmds[0] == "nshloc":
            self.writeNshLocRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "neigh4":
            self.writeNexthopRules(
                mode,
                int(splt[1]),
                splt[3],
                splt[5],
                int(splt[6]),
            )
            self.writeNeighborRules4(
                mode,
                splt[2],
                int(splt[1]),
                int(splt[4]),
            )
            continue

        if cmds[0] == "hairpin":
            self.writeHairpinRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "portvrf":
            self.writeVrfRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "nhop2port":
            self.writeNhop2portRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "portvlan":
            self.writeVlanRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "bundlevlan":
            self.writeBunVlanRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "xconnect":
            self.writeXconnRules(
                mode,
                int(splt[1]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if cmds[0] == "portbridge":
            self.writeBrprtRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "bridgemac":
            self.writeBrmacRules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
            )
            continue

        if cmds[0] == "routedmac":
            self.writeRoumacRules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
                int(splt[4]),
            )
            continue

        if cmds[0] == "bridgelabel":
            self.writeBrlabRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "bridgevpls":
            self.writeBrvplsRules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if cmds[0] == "route6":
            addr = splt[1].split("/")
            self.writeForwardRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
            )
            continue

        if cmds[0] == "labroute6":
            addr = splt[1].split("/")
            self.writeGlobRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
            )
            continue

        if cmds[0] == "vpnroute6":
            addr = splt[1].split("/")
            self.writeVpnRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if cmds[0] == "srvroute6":
            addr = splt[1].split("/")
            self.writeSrvRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                splt[5],
            )
            continue

        if cmds[0] == "polroute6":
            addr = splt[1].split("/")
            self.writePolkaRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                splt[5],
            )
            continue

        if cmds[0] == "myaddr6":
            addr = splt[1].split("/")
            self.writeMyaddrRules6(
                mode,
                addr[0],
                int(addr[1]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "label6":
            self.writeMplsRules(
                mode,
                int(splt[1]),
                int(splt[4]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "unlabel6":
            self.writeUnMplsRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "mylabel6":
            self.writeMyMplsRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "mysrv6":
            self.writeMySrv6rules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
            )
            continue

        if cmds[0] == "neigh6":
            self.writeNexthopRules(
                mode,
                int(splt[1]),
                splt[3],
                splt[5],
                int(splt[6]),
            )
            self.writeNeighborRules6(
                mode,
                splt[2],
                int(splt[1]),
                int(splt[4]),
            )
            continue

        if cmds[0] == "sgttag":
            self.writeSgtTagRules(
                mode,
                int(splt[1]),
            )
            continue

        if cmds[0] == "sgtset":
            self.writeSgtSetRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "copp4":
            self.writeCopp4Rules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
                int(splt[4]),
                splt[5],
                splt[6],
                splt[7],
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
            )
            continue

        if cmds[0] == "copp6":
            self.writeCopp6Rules(
                mode,
                int(splt[1]),
                splt[2],
                int(splt[3]),
                int(splt[4]),
                splt[5],
                splt[6],
                splt[7],
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
            )
            continue

        if cmds[0] == "natcfg4":
            self.writeNatCfgRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "natcfg6":
            self.writeNatCfgRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "nattrns4":
            self.writeNatTrnsRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
                splt[7],
                int(splt[8]),
                splt[9],
                int(splt[10]),
            )
            continue

        if cmds[0] == "nattrns6":
            self.writeNatTrnsRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
                splt[7],
                int(splt[8]),
                splt[9],
                int(splt[10]),
            )
            continue

        if cmds[0] == "inspect4":
            self.writeInspectRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue

        if cmds[0] == "inspect6":
            self.writeInspectRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue

        if cmds[0] == "pbr4norm":
            self.writePbrNormRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr6norm":
            self.writePbrNormRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr4vrf":
            self.writePbrVrfRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr6vrf":
            self.writePbrVrfRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr4hop":
            self.writePbrHopRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr6hop":
            self.writePbrHopRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
                splt[10],
                splt[11],
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
            )
            continue

        if cmds[0] == "pbr4lab":
            self.writePbrLabRules4(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[7]),
                int(splt[8]),
                splt[9],
                splt[10],
                splt[11],
                splt[12],
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
                int(splt[22]),
            )
            continue

        if cmds[0] == "pbr6lab":
            self.writePbrLabRules6(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[7]),
                int(splt[8]),
                splt[9],
                splt[10],
                splt[11],
                splt[12],
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
                int(splt[22]),
            )
            continue

        if cmds[0] == "gre4":
            self.writeGre4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
            )
            continue

        if cmds[0] == "gre6":
            self.writeGre6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
            )
            continue

        if cmds[0] == "ipip4":
            self.writeIpip4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
            )
            continue

        if cmds[0] == "ipip6":
            self.writeIpip6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
            )
            continue

        if cmds[0] == "pppoe":
            self.writePppoeRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
            )
            continue

        if cmds[0] == "l2tp4":
            self.writeL2tp4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "l2tp6":
            self.writeL2tp6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "gtp4":
            self.writeGtp4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "gtp6":
            self.writeGtp6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                splt[6],
                int(splt[7]),
                splt[8],
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "bridgevxlan4":
            self.writeVxlan4rules(
                mode,
                int(splt[1]),
                splt[2],
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
            )
            continue

        if cmds[0] == "bridgevxlan6":
            self.writeVxlan6rules(
                mode,
                int(splt[1]),
                splt[2],
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
            )
            continue

        if cmds[0] == "bridgepckoudp4":
            self.writePckoudp4rules(
                mode,
                int(splt[1]),
                splt[2],
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
            )
            continue

        if cmds[0] == "bridgepckoudp6":
            self.writePckoudp6rules(
                mode,
                int(splt[1]),
                splt[2],
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
            )
            continue

        if cmds[0] == "inqos":
            self.writeInQosRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "outqos":
            self.writeOutQosRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "inqos4":
            self.writeInQos4Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                int(splt[5]),
                int(splt[6]),
                splt[7],
                splt[8],
                splt[9],
                splt[10],
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
            )
            continue

        if cmds[0] == "outqos4":
            self.writeOutQos4Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                int(splt[5]),
                int(splt[6]),
                splt[7],
                splt[8],
                splt[9],
                splt[10],
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
            )
            continue

        if cmds[0] == "inqos6":
            self.writeInQos6Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                int(splt[5]),
                int(splt[6]),
                splt[7],
                splt[8],
                splt[9],
                splt[10],
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
            )
            continue

        if cmds[0] == "outqos6":
            self.writeOutQos6Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                int(splt[5]),
                int(splt[6]),
                splt[7],
                splt[8],
                splt[9],
                splt[10],
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
            )
            continue

        if cmds[0] == "flowspec4":
            self.writeFlowspec4Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                int(splt[7]),
                int(splt[8]),
                splt[9],
                splt[10],
                splt[11],
                splt[12],
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
                int(splt[22]),
            )
            continue

        if cmds[0] == "flowspec6":
            self.writeFlowspec6Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                int(splt[7]),
                int(splt[8]),
                splt[9],
                splt[10],
                splt[11],
                splt[12],
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
                int(splt[20]),
                int(splt[21]),
                int(splt[22]),
            )
            continue

        if cmds[0] == "inacl4":
            self.writeInAcl4Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "outacl4":
            self.writeOutAcl4Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "inacl6":
            self.writeInAcl6Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "outacl6":
            self.writeOutAcl6Rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
                splt[8],
                splt[9],
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "mlocal4":
            self.writeMlocal4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue

        if cmds[0] == "mlocal6":
            self.writeMlocal6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue

        if cmds[0] == "mroute4":
            self.writeMroute4rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
            )
            continue

        if cmds[0] == "mroute6":
            self.writeMroute6rules(
                mode,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                splt[8],
                splt[9],
            )
            continue

        if cmds[0] == "mlabroute4":
            self.writeMlabRouteRules(
                mode,
                "4",
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
            )
            continue

        if cmds[0] == "mlabroute6":
            self.writeMlabRouteRules(
                mode,
                "6",
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
            )
            continue

        if cmds[0] == "duplabloc4":
            self.writeDupLabLocRules(
                mode,
                "4",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue

        if cmds[0] == "duplabloc6":
            self.writeDupLabLocRules(
                mode,
                "6",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue

        if cmds[0] == "duplabel4":
            self.writeDupLabelRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue

        if cmds[0] == "duplabel6":
            self.writeDupLabelRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue

        if cmds[0] == "bierlabel4":
            self.writeBierLabelRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
            )
            continue

        if cmds[0] == "bierlabel6":
            self.writeBierLabelRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
            )
            continue

        if cmds[0] == "bierlabloc4":
            self.writeBierLabLocRules(
                mode,
                "4",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "bierlabloc6":
            self.writeBierLabLocRules(
                mode,
                "6",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
            )
            continue

        if cmds[0] == "mbierroute4":
            self.writeMbierRouteRules(
                mode,
                "4",
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "mbierroute6":
            self.writeMbierRouteRules(
                mode,
                "6",
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
                int(splt[8]),
                int(splt[9]),
                int(splt[10]),
                int(splt[11]),
                int(splt[12]),
                int(splt[13]),
                int(splt[14]),
                int(splt[15]),
                int(splt[16]),
                int(splt[17]),
                int(splt[18]),
                int(splt[19]),
            )
            continue

        if cmds[0] == "polkapoly":
            self.writePolkaPolyRules(
                mode,
                int(splt[2]),
            )
            continue

        if cmds[0] == "polkaidx":
            self.writePolkaIndexRules(
                mode,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if cmds[0] == "polkaown":
            self.writePolkaOwnRules(
                mode,
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "mtu":
            self.setPortMTU(
                int(splt[1]),
                int(splt[2]),
            )
            continue

        if cmds[0] == "state":
            if self.platform == "stordis_bf2556x_1t" and int(splt[1]) in SAL_PORT_ID:
                self._setPortAdmStatusBF2556X1T(
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
            else:
                self._setPortAdmStatus(
                    int(splt[1]),
                    int(splt[2]),
                    int(splt[3]),
                    int(splt[4]),
                    int(splt[5]),
                    int(splt[6]),
                )
            continue

        if cmds[0] == "bundlelist":
            self.setBundleAdmStatus(
                mode,
                int(splt[1]),
                list(splt[2:]),
            )
            continue
