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
        if not ("portbundle_" in splt[0]):
            if splt[0] != "keepalive" or not self.no_log_keepalive:
                logger.warning("rx: %s", splt)
        else:
            continue
        if splt[0] == "route4_add":
            addr = splt[1].split("/")
            self.writeForwardRules4(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue
        if splt[0] == "route4_mod":
            addr = splt[1].split("/")
            self.writeForwardRules4(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue
        if splt[0] == "route4_del":
            addr = splt[1].split("/")
            self.writeForwardRules4(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue

        if splt[0] == "labroute4_add":
            addr = splt[1].split("/")
            self.writeGlobRules4(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue
        if splt[0] == "labroute4_mod":
            addr = splt[1].split("/")
            self.writeGlobRules4(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue
        if splt[0] == "labroute4_del":
            addr = splt[1].split("/")
            self.writeGlobRules4(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue

        if splt[0] == "vpnroute4_add":
            addr = splt[1].split("/")
            self.writeVpnRules4(
                1,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue
        if splt[0] == "vpnroute4_mod":
            addr = splt[1].split("/")
            self.writeVpnRules4(
                2,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue
        if splt[0] == "vpnroute4_del":
            addr = splt[1].split("/")
            self.writeVpnRules4(
                3,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if splt[0] == "srvroute4_add":
            addr = splt[1].split("/")
            self.writeSrvRules4(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "srvroute4_mod":
            addr = splt[1].split("/")
            self.writeSrvRules4(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "srvroute4_del":
            addr = splt[1].split("/")
            self.writeSrvRules4(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue

        if splt[0] == "polroute4_add":
            addr = splt[1].split("/")
            self.writePolkaRules4(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "polroute4_mod":
            addr = splt[1].split("/")
            self.writePolkaRules4(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "polroute4_del":
            addr = splt[1].split("/")
            self.writePolkaRules4(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue

        if splt[0] == "myaddr4_add":
            addr = splt[1].split("/")
            self.writeMyaddrRules4(1, addr[0], int(addr[1]), int(splt[3]))
            continue
        if splt[0] == "myaddr4_mod":
            addr = splt[1].split("/")
            self.writeMyaddrRules4(2, addr[0], int(addr[1]), int(splt[3]))
            continue
        if splt[0] == "myaddr4_del":
            addr = splt[1].split("/")
            self.writeMyaddrRules4(3, addr[0], int(addr[1]), int(splt[3]))
            continue

        if splt[0] == "cpulabel_add":
            self.writeCpuMplsRules(1, int(splt[1]))
            continue
        if splt[0] == "cpulabel_mod":
            self.writeCpuMplsRules(2, int(splt[1]))
            continue
        if splt[0] == "cpulabel_del":
            self.writeCpuMplsRules(3, int(splt[1]))
            continue

        if splt[0] == "label4_add":
            self.writeMplsRules(1, int(splt[1]), int(splt[4]), int(splt[2]))
            continue
        if splt[0] == "label4_mod":
            self.writeMplsRules(2, int(splt[1]), int(splt[4]), int(splt[2]))
            continue
        if splt[0] == "label4_del":
            self.writeMplsRules(3, int(splt[1]), int(splt[4]), int(splt[2]))
            continue

        if splt[0] == "unlabel4_add":
            self.writeUnMplsRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "unlabel4_mod":
            self.writeUnMplsRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "unlabel4_del":
            self.writeUnMplsRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "mylabel4_add":
            self.writeMyMplsRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "mylabel4_mod":
            self.writeMyMplsRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "mylabel4_del":
            self.writeMyMplsRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "mysrv4_add":
            self.writeMySrv4rules(1, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "mysrv4_mod":
            self.writeMySrv4rules(2, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "mysrv4_del":
            self.writeMySrv4rules(3, int(splt[1]), splt[2], int(splt[3]))
            continue

        if splt[0] == "nshfwd_add":
            self.writeNshFwdRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "nshfwd_mod":
            self.writeNshFwdRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "nshfwd_del":
            self.writeNshFwdRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
                splt[5],
                int(splt[6]),
                int(splt[7]),
            )
            continue

        if splt[0] == "nshloc_add":
            self.writeNshLocRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "nshloc_mod":
            self.writeNshLocRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "nshloc_del":
            self.writeNshLocRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
            continue

        if splt[0] == "neigh4_add":
            self.writeNexthopRules(1, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules4(1, splt[2], int(splt[1]), int(splt[4]))
            continue
        if splt[0] == "neigh4_mod":
            self.writeNexthopRules(2, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules4(2, splt[2], int(splt[1]), int(splt[4]))
            continue
        if splt[0] == "neigh4_del":
            self.writeNexthopRules(3, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules4(3, splt[2], int(splt[1]), int(splt[4]))
            continue

        if splt[0] == "hairpin_add":
            self.writeHairpinRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "hairpin_mod":
            self.writeHairpinRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "hairpin_del":
            self.writeHairpinRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "portvrf_add":
            self.writeVrfRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "portvrf_mod":
            self.writeVrfRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "portvrf_del":
            self.writeVrfRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "nhop2port_add":
            self.writeNhop2portRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "nhop2port_mod":
            self.writeNhop2portRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "nhop2port_del":
            self.writeNhop2portRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
            continue

        if splt[0] == "portvlan_add":
            self.writeVlanRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "portvlan_mod":
            self.writeVlanRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "portvlan_del":
            self.writeVlanRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
            continue

        if splt[0] == "bundlevlan_add":
            self.writeBunVlanRules(1, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "bundlevlan_mod":
            self.writeBunVlanRules(2, int(splt[1]), int(splt[2]), int(splt[3]))
            continue
        if splt[0] == "bundlevlan_del":
            self.writeBunVlanRules(3, int(splt[1]), int(splt[2]), int(splt[3]))
            continue

        if splt[0] == "xconnect_add":
            self.writeXconnRules(
                1, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue
        if splt[0] == "xconnect_mod":
            self.writeXconnRules(
                2, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue
        if splt[0] == "xconnect_del":
            self.writeXconnRules(
                3, int(splt[1]), int(splt[3]), int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue

        if splt[0] == "portbridge_add":
            self.writeBrprtRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "portbridge_mod":
            self.writeBrprtRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "portbridge_del":
            self.writeBrprtRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "bridgemac_add":
            self.writeBrmacRules(1, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "bridgemac_mod":
            self.writeBrmacRules(2, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "bridgemac_del":
            self.writeBrmacRules(3, int(splt[1]), splt[2], int(splt[3]))
            continue

        if splt[0] == "routedmac_add":
            self.writeRoumacRules(1, int(splt[1]), splt[2], int(splt[3]), int(splt[4]))
            continue
        if splt[0] == "routedmac_mod":
            selfwriteRoumacRules(2, int(splt[1]), splt[2], int(splt[3]), int(splt[4]))
            continue
        if splt[0] == "routedmac_del":
            self.writeRoumacRules(3, int(splt[1]), splt[2], int(splt[3]), int(splt[4]))
            continue

        if splt[0] == "bridgelabel_add":
            self.writeBrlabRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "bridgelabel_mod":
            self.writeBrlabRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "bridgelabel_del":
            self.writeBrlabRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "bridgevpls_add":
            self.writeBrvplsRules(
                1, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue
        if splt[0] == "bridgevpls_mod":
            self.writeBrvplsRules(
                2, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue
        if splt[0] == "bridgevpls_del":
            self.writeBrvplsRules(
                3, int(splt[1]), splt[2], int(splt[4]), int(splt[5]), int(splt[6])
            )
            continue

        if splt[0] == "route6_add":
            addr = splt[1].split("/")
            self.writeForwardRules6(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue
        if splt[0] == "route6_mod":
            addr = splt[1].split("/")
            self.writeForwardRules6(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue
        if splt[0] == "route6_del":
            addr = splt[1].split("/")
            self.writeForwardRules6(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4])
            )
            continue

        if splt[0] == "labroute6_add":
            addr = splt[1].split("/")
            self.writeGlobRules6(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue
        if splt[0] == "labroute6_mod":
            addr = splt[1].split("/")
            self.writeGlobRules6(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue
        if splt[0] == "labroute6_del":
            addr = splt[1].split("/")
            self.writeGlobRules6(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), int(splt[5])
            )
            continue

        if splt[0] == "vpnroute6_add":
            addr = splt[1].split("/")
            self.writeVpnRules6(
                1,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue
        if splt[0] == "vpnroute6_mod":
            addr = splt[1].split("/")
            self.writeVpnRules6(
                2,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue
        if splt[0] == "vpnroute6_del":
            addr = splt[1].split("/")
            self.writeVpnRules6(
                3,
                addr[0],
                int(addr[1]),
                int(splt[2]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
            )
            continue

        if splt[0] == "srvroute6_add":
            addr = splt[1].split("/")
            self.writeSrvRules6(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "srvroute6_mod":
            addr = splt[1].split("/")
            self.writeSrvRules6(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "srvroute6_del":
            addr = splt[1].split("/")
            self.writeSrvRules6(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue

        if splt[0] == "polroute6_add":
            addr = splt[1].split("/")
            self.writePolkaRules6(
                1, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "polroute6_mod":
            addr = splt[1].split("/")
            self.writePolkaRules6(
                2, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue
        if splt[0] == "polroute6_del":
            addr = splt[1].split("/")
            self.writePolkaRules6(
                3, addr[0], int(addr[1]), int(splt[2]), int(splt[4]), splt[5]
            )
            continue

        if splt[0] == "myaddr6_add":
            addr = splt[1].split("/")
            self.writeMyaddrRules6(1, addr[0], int(addr[1]), int(splt[3]))
            continue
        if splt[0] == "myaddr6_mod":
            addr = splt[1].split("/")
            self.writeMyaddrRules6(2, addr[0], int(addr[1]), int(splt[3]))
            continue
        if splt[0] == "myaddr6_del":
            addr = splt[1].split("/")
            self.writeMyaddrRules6(3, addr[0], int(addr[1]), int(splt[3]))
            continue

        if splt[0] == "label6_add":
            self.writeMplsRules(1, int(splt[1]), int(splt[4]), int(splt[2]))
            continue
        if splt[0] == "label6_mod":
            self.writeMplsRules(2, int(splt[1]), int(splt[4]), int(splt[2]))
            continue
        if splt[0] == "label6_del":
            self.writeMplsRules(3, int(splt[1]), int(splt[4]), int(splt[2]))
            continue

        if splt[0] == "unlabel6_add":
            self.writeUnMplsRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "unlabel6_mod":
            self.writeUnMplsRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "unlabel6_del":
            self.writeUnMplsRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "mylabel6_add":
            self.writeMyMplsRules(1, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "mylabel6_mod":
            self.writeMyMplsRules(2, int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "mylabel6_del":
            self.writeMyMplsRules(3, int(splt[1]), int(splt[2]))
            continue

        if splt[0] == "mysrv6_add":
            self.writeMySrv6rules(1, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "mysrv6_mod":
            self.writeMySrv6rules(2, int(splt[1]), splt[2], int(splt[3]))
            continue
        if splt[0] == "mysrv6_del":
            self.writeMySrv6rules(3, int(splt[1]), splt[2], int(splt[3]))
            continue

        if splt[0] == "neigh6_add":
            self.writeNexthopRules(1, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules6(1, splt[2], int(splt[1]), int(splt[4]))
            continue
        if splt[0] == "neigh6_mod":
            self.writeNexthopRules(2, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules6(2, splt[2], int(splt[1]), int(splt[4]))
            continue
        if splt[0] == "neigh6_del":
            self.writeNexthopRules(3, int(splt[1]), splt[3], splt[5], int(splt[6]))
            self.writeNeighborRules6(3, splt[2], int(splt[1]), int(splt[4]))
            continue

        if splt[0] == "copp4_add":
            self.writeCopp4Rules(
                1,
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
            )
            continue
        if splt[0] == "copp4_mod":
            self.writeCopp4Rules(
                2,
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
            )
            continue
        if splt[0] == "copp4_del":
            self.writeCopp4Rules(
                3,
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
            )
            continue

        if splt[0] == "copp6_add":
            self.writeCopp6Rules(
                1,
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
            )
            continue
        if splt[0] == "copp6_mod":
            self.writeCopp6Rules(
                2,
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
            )
            continue
        if splt[0] == "copp6_del":
            self.writeCopp6Rules(
                3,
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
            )
            continue

        if splt[0] == "natcfg4_add":
            self.writeNatCfgRules4(
                1,
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
            )
            continue
        if splt[0] == "natcfg4_mod":
            self.writeNatCfgRules4(
                2,
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
            )
            continue
        if splt[0] == "natcfg4_del":
            self.writeNatCfgRules4(
                3,
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
            )
            continue

        if splt[0] == "natcfg6_add":
            self.writeNatCfgRules6(
                1,
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
            )
            continue
        if splt[0] == "natcfg6_mod":
            self.writeNatCfgRules6(
                2,
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
            )
            continue
        if splt[0] == "natcfg6_del":
            self.writeNatCfgRules6(
                3,
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
            )
            continue

        if splt[0] == "nattrns4_add":
            self.writeNatTrnsRules4(
                1,
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
        if splt[0] == "nattrns4_mod":
            self.writeNatTrnsRules4(
                2,
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
        if splt[0] == "nattrns4_del":
            self.writeNatTrnsRules4(
                3,
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

        if splt[0] == "nattrns6_add":
            self.writeNatTrnsRules6(
                1,
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
        if splt[0] == "nattrns6_mod":
            self.writeNatTrnsRules6(
                2,
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
        if splt[0] == "nattrns6_del":
            self.writeNatTrnsRules6(
                3,
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

        if splt[0] == "inspect4_add":
            self.writeInspectRules4(
                1,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue
        if splt[0] == "inspect4_mod":
            self.writeInspectRules4(
                2,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue
        if splt[0] == "inspect4_del":
            self.writeInspectRules4(
                3,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue

        if splt[0] == "inspect6_add":
            self.writeInspectRules6(
                1,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue
        if splt[0] == "inspect6_mod":
            self.writeInspectRules6(
                2,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue
        if splt[0] == "inspect6_del":
            self.writeInspectRules6(
                3,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                int(splt[4]),
                splt[5],
                int(splt[6]),
            )
            continue

        if splt[0] == "pbr4norm_add":
            self.writePbrNormRules4(
                1,
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
            )
            continue
        if splt[0] == "pbr4norm_mod":
            self.writePbrNormRules4(
                2,
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
            )
            continue
        if splt[0] == "pbr4norm_del":
            self.writePbrNormRules4(
                3,
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
            )
            continue

        if splt[0] == "pbr6norm_add":
            self.writePbrNormRules6(
                1,
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
            )
            continue
        if splt[0] == "pbr6norm_mod":
            self.writePbrNormRules6(
                2,
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
            )
            continue
        if splt[0] == "pbr6norm_del":
            self.writePbrNormRules6(
                3,
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
            )
            continue

        if splt[0] == "pbr4vrf_add":
            self.writePbrVrfRules4(
                1,
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
            )
            continue
        if splt[0] == "pbr4vrf_mod":
            self.writePbrVrfRules4(
                2,
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
            )
            continue
        if splt[0] == "pbr4vrf_del":
            self.writePbrVrfRules4(
                3,
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
            )
            continue

        if splt[0] == "pbr6vrf_add":
            self.writePbrVrfRules6(
                1,
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
            )
            continue
        if splt[0] == "pbr6vrf_mod":
            self.writePbrVrfRules6(
                2,
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
            )
            continue
        if splt[0] == "pbr6vrf_del":
            self.writePbrVrfRules6(
                3,
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
            )
            continue

        if splt[0] == "pbr4hop_add":
            self.writePbrHopRules4(
                1,
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
            )
            continue
        if splt[0] == "pbr4hop_mod":
            self.writePbrHopRules4(
                2,
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
            )
            continue
        if splt[0] == "pbr4hop_del":
            self.writePbrHopRules4(
                3,
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
            )
            continue

        if splt[0] == "pbr6hop_add":
            self.writePbrHopRules6(
                1,
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
            )
            continue
        if splt[0] == "pbr6hop_mod":
            self.writePbrHopRules6(
                2,
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
            )
            continue
        if splt[0] == "pbr6hop_del":
            self.writePbrHopRules6(
                3,
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
            )
            continue

        if splt[0] == "pbr4lab_add":
            self.writePbrLabRules4(
                1,
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
            )
            continue
        if splt[0] == "pbr4lab_mod":
            self.writePbrLabRules4(
                2,
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
            )
            continue
        if splt[0] == "pbr4lab_del":
            self.writePbrLabRules4(
                3,
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
            )
            continue

        if splt[0] == "pbr6lab_add":
            self.writePbrLabRules6(
                1,
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
            )
            continue
        if splt[0] == "pbr6lab_mod":
            self.writePbrLabRules6(
                2,
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
            )
            continue
        if splt[0] == "pbr6lab_del":
            self.writePbrLabRules6(
                3,
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
            )
            continue

        if splt[0] == "gre4_add":
            self.writeGre4rules(
                1,
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
        if splt[0] == "gre4_mod":
            self.writeGre4rules(
                2,
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
        if splt[0] == "gre4_del":
            self.writeGre4rules(
                3,
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

        if splt[0] == "gre6_add":
            self.writeGre6rules(
                1,
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
        if splt[0] == "gre6_mod":
            self.writeGre6rules(
                2,
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
        if splt[0] == "gre6_del":
            self.writeGre6rules(
                3,
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

        if splt[0] == "ipip4_add":
            self.writeIpip4rules(
                1,
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
        if splt[0] == "ipip4_mod":
            self.writeIpip4rules(
                2,
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
        if splt[0] == "ipip4_del":
            self.writeIpip4rules(
                3,
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

        if splt[0] == "ipip6_add":
            self.writeIpip6rules(
                1,
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
        if splt[0] == "ipip6_mod":
            self.writeIpip6rules(
                2,
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
        if splt[0] == "ipip6_del":
            self.writeIpip6rules(
                3,
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

        if splt[0] == "pppoe_add":
            self.writePppoeRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
            )
            continue
        if splt[0] == "pppoe_mod":
            self.writePppoeRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
            )
            continue
        if splt[0] == "pppoe_del":
            self.writePppoeRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                splt[6],
                splt[7],
            )
            continue

        if splt[0] == "l2tp4_add":
            self.writeL2tp4rules(
                1,
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
        if splt[0] == "l2tp4_mod":
            self.writeL2tp4rules(
                2,
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
        if splt[0] == "l2tp4_del":
            self.writeL2tp4rules(
                3,
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

        if splt[0] == "l2tp6_add":
            self.writeL2tp6rules(
                1,
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
        if splt[0] == "l2tp6_mod":
            self.writeL2tp6rules(
                2,
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
        if splt[0] == "l2tp6_del":
            self.writeL2tp6rules(
                3,
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

        if splt[0] == "bridgevxlan4_add":
            self.writeVxlan4rules(
                1,
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
        if splt[0] == "bridgevxlan4_mod":
            self.writeVxlan4rules(
                2,
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
        if splt[0] == "bridgevxlan4_del":
            self.writeVxlan4rules(
                3,
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

        if splt[0] == "bridgevxlan6_add":
            self.writeVxlan6rules(
                1,
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
        if splt[0] == "bridgevxlan6_mod":
            self.writeVxlan6rules(
                2,
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
        if splt[0] == "bridgevxlan6_del":
            self.writeVxlan6rules(
                3,
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

        if splt[0] == "bridgepckoudp4_add":
            self.writePckoudp4rules(
                1,
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
        if splt[0] == "bridgepckoudp4_mod":
            self.writePckoudp4rules(
                2,
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
        if splt[0] == "bridgepckoudp4_del":
            self.writePckoudp4rules(
                3,
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

        if splt[0] == "bridgepckoudp6_add":
            self.writePckoudp6rules(
                1,
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
        if splt[0] == "bridgepckoudp6_mod":
            self.writePckoudp6rules(
                2,
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
        if splt[0] == "bridgepckoudp6_del":
            self.writePckoudp6rules(
                3,
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

        if splt[0] == "inqos_add":
            self.writeInQosRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "inqos_mod":
            self.writeInQosRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "inqos_del":
            self.writeInQosRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "outqos_add":
            self.writeOutQosRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "outqos_mod":
            self.writeOutQosRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "outqos_del":
            self.writeOutQosRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if splt[0] == "inqos4_add":
            self.writeInQos4Rules(
                1,
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
            )
            continue
        if splt[0] == "inqos4_mod":
            self.writeInQos4Rules(
                2,
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
            )
            continue
        if splt[0] == "inqos4_del":
            self.writeInQos4Rules(
                3,
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
            )
            continue
        if splt[0] == "outqos4_add":
            self.writeOutQos4Rules(
                1,
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
            )
            continue
        if splt[0] == "outqos4_mod":
            self.writeOutQos4Rules(
                2,
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
            )
            continue
        if splt[0] == "outqos4_del":
            self.writeOutQos4Rules(
                3,
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
            )
            continue
        if splt[0] == "inqos6_add":
            self.writeInQos6Rules(
                1,
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
            )
            continue
        if splt[0] == "inqos6_mod":
            self.writeInQos6Rules(
                2,
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
            )
            continue
        if splt[0] == "inqos6_del":
            self.writeInQos6Rules(
                3,
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
            )
            continue
        if splt[0] == "outqos6_add":
            self.writeOutQos6Rules(
                1,
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
            )
            continue
        if splt[0] == "outqos6_mod":
            self.writeOutQos6Rules(
                2,
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
            )
            continue
        if splt[0] == "outqos6_del":
            self.writeOutQos6Rules(
                3,
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
            )
            continue

        if splt[0] == "flowspec4_add":
            self.writeFlowspec4Rules(
                1,
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
            )
            continue
        if splt[0] == "flowspec4_mod":
            self.writeFlowspec4Rules(
                2,
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
            )
            continue
        if splt[0] == "flowspec4_del":
            self.writeFlowspec4Rules(
                3,
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
            )
            continue
        if splt[0] == "flowspec6_add":
            self.writeFlowspec6Rules(
                1,
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
            )
            continue
        if splt[0] == "flowspec6_mod":
            self.writeFlowspec6Rules(
                2,
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
            )
            continue
        if splt[0] == "flowspec6_del":
            self.writeFlowspec6Rules(
                3,
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
            )
            continue

        if splt[0] == "inacl4_add":
            self.writeInAcl4Rules(
                1,
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
            )
            continue
        if splt[0] == "inacl4_mod":
            self.writeInAcl4Rules(
                2,
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
            )
            continue
        if splt[0] == "inacl4_del":
            self.writeInAcl4Rules(
                3,
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
            )
            continue
        if splt[0] == "outacl4_add":
            self.writeOutAcl4Rules(
                1,
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
            )
            continue
        if splt[0] == "outacl4_mod":
            self.writeOutAcl4Rules(
                2,
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
            )
            continue
        if splt[0] == "outacl4_del":
            self.writeOutAcl4Rules(
                3,
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
            )
            continue
        if splt[0] == "inacl6_add":
            self.writeInAcl6Rules(
                1,
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
            )
            continue
        if splt[0] == "inacl6_mod":
            self.writeInAcl6Rules(
                2,
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
            )
            continue
        if splt[0] == "inacl6_del":
            self.writeInAcl6Rules(
                3,
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
            )
            continue
        if splt[0] == "outacl6_add":
            self.writeOutAcl6Rules(
                1,
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
            )
            continue
        if splt[0] == "outacl6_mod":
            self.writeOutAcl6Rules(
                2,
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
            )
            continue
        if splt[0] == "outacl6_del":
            self.writeOutAcl6Rules(
                3,
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
            )
            continue

        if splt[0] == "mlocal4_add":
            self.writeMlocal4rules(
                1,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue
        if splt[0] == "mlocal4_mod":
            self.writeMlocal4rules(
                2,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue
        if splt[0] == "mlocal4_del":
            self.writeMlocal4rules(
                3,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue
        if splt[0] == "mlocal6_add":
            self.writeMlocal6rules(
                1,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue
        if splt[0] == "mlocal6_mod":
            self.writeMlocal6rules(
                2,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue
        if splt[0] == "mlocal6_del":
            self.writeMlocal6rules(
                3,
                int(splt[1]),
                int(splt[2]),
                splt[3],
                splt[4],
                int(splt[5]),
                splt[6],
            )
            continue

        if splt[0] == "mroute4_add":
            self.writeMroute4rules(
                1,
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
        if splt[0] == "mroute4_mod":
            self.writeMroute4rules(
                2,
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
        if splt[0] == "mroute4_del":
            self.writeMroute4rules(
                3,
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
        if splt[0] == "mroute6_add":
            self.writeMroute6rules(
                1,
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
        if splt[0] == "mroute6_mod":
            self.writeMroute6rules(
                2,
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
        if splt[0] == "mroute6_del":
            self.writeMroute6rules(
                3,
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

        if splt[0] == "mlabroute4_add":
            self.writeMlabRouteRules(
                1,
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
        if splt[0] == "mlabroute4_mod":
            self.writeMlabRouteRules(
                2,
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
        if splt[0] == "mlabroute4_del":
            self.writeMlabRouteRules(
                3,
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

        if splt[0] == "mlabroute6_add":
            self.writeMlabRouteRules(
                1,
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
        if splt[0] == "mlabroute6_mod":
            self.writeMlabRouteRules(
                2,
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
        if splt[0] == "mlabroute6_del":
            self.writeMlabRouteRules(
                3,
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

        if splt[0] == "duplabloc4_add":
            self.writeDupLabLocRules(
                1,
                "4",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue
        if splt[0] == "duplabloc4_mod":
            self.writeDupLabLocRules(
                2,
                "4",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue
        if splt[0] == "duplabloc4_del":
            self.writeDupLabLocRules(
                3,
                "4",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue
        if splt[0] == "duplabloc6_add":
            self.writeDupLabLocRules(
                1,
                "6",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue
        if splt[0] == "duplabloc6_mod":
            self.writeDupLabLocRules(
                2,
                "6",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue
        if splt[0] == "duplabloc6_del":
            self.writeDupLabLocRules(
                3,
                "6",
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                splt[4],
            )
            continue

        if splt[0] == "duplabel4_add":
            self.writeDupLabelRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "duplabel4_mod":
            self.writeDupLabelRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "duplabel4_del":
            self.writeDupLabelRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "duplabel6_add":
            self.writeDupLabelRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "duplabel6_mod":
            self.writeDupLabelRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue
        if splt[0] == "duplabel6_del":
            self.writeDupLabelRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
                int(splt[4]),
                int(splt[5]),
                int(splt[6]),
                int(splt[7]),
            )
            continue

        if splt[0] == "bierlabel4_add":
            self.writeBierLabelRules(
                1,
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
        if splt[0] == "bierlabel4_mod":
            self.writeBierLabelRules(
                2,
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
        if splt[0] == "bierlabel4_del":
            self.writeBierLabelRules(
                3,
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
        if splt[0] == "bierlabel6_add":
            self.writeBierLabelRules(
                1,
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
        if splt[0] == "bierlabel6_mod":
            self.writeBierLabelRules(
                2,
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
        if splt[0] == "bierlabel6_del":
            self.writeBierLabelRules(
                3,
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

        if splt[0] == "bierlabloc4_add":
            self.writeBierLabLocRules(
                1,
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
        if splt[0] == "bierlabloc4_mod":
            self.writeBierLabLocRules(
                2,
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
        if splt[0] == "bierlabloc4_del":
            self.writeBierLabLocRules(
                3,
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
        if splt[0] == "bierlabloc6_add":
            self.writeBierLabLocRules(
                1,
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
        if splt[0] == "bierlabloc6_mod":
            self.writeBierLabLocRules(
                2,
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
        if splt[0] == "bierlabloc6_del":
            self.writeBierLabLocRules(
                3,
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

        if splt[0] == "mbierroute4_add":
            self.writeMbierRouteRules(
                1,
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
        if splt[0] == "mbierroute4_mod":
            self.writeMbierRouteRules(
                2,
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
        if splt[0] == "mbierroute4_del":
            self.writeMbierRouteRules(
                3,
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

        if splt[0] == "mbierroute6_add":
            self.writeMbierRouteRules(
                1,
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
        if splt[0] == "mbierroute6_mod":
            self.writeMbierRouteRules(
                2,
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
        if splt[0] == "mbierroute6_del":
            self.writeMbierRouteRules(
                3,
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

        if splt[0] == "polkapoly_add":
            self.writePolkaPolyRules(
                1,
                int(splt[2]),
            )
            continue
        if splt[0] == "polkapoly_mod":
            self.writePolkaPolyRules(
                2,
                int(splt[2]),
            )
            continue
        if splt[0] == "polkapoly_del":
            self.writePolkaPolyRules(
                3,
                int(splt[2]),
            )
            continue

        if splt[0] == "polkaidx_add":
            self.writePolkaIndexRules(
                1,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "polkaidx_mod":
            self.writePolkaIndexRules(
                2,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue
        if splt[0] == "polkaidx_del":
            self.writePolkaIndexRules(
                3,
                int(splt[1]),
                int(splt[2]),
                int(splt[3]),
            )
            continue

        if splt[0] == "polkaown_add":
            self.writePolkaOwnRules(
                1,
                int(splt[1]),
                int(splt[2]),
            )
            continue
        if splt[0] == "polkaown_mod":
            self.writePolkaOwnRules(
                2,
                int(splt[1]),
                int(splt[2]),
            )
            continue
        if splt[0] == "polkaown_del":
            self.writePolkaOwnRules(
                3,
                int(splt[1]),
                int(splt[2]),
            )
            continue
        if splt[0] == "mtu":
            self.setPortMTU(int(splt[1]), int(splt[2]))
            continue
        if splt[0] == "state":
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
        if splt[0] == "bundlelist_add":
            self.setBundleAdmStatus(1, int(splt[1]), list(splt[2:]))
            continue
        if splt[0] == "bundlelist_mod":
            self.setBundleAdmStatus(2, int(splt[1]), list(splt[2:]))
            continue
        if splt[0] == "bundlelist_del":
            self.setBundleAdmStatus(3, int(splt[1]), list(splt[2:]))
            continue
