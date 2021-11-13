package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgScrpt;
import net.freertr.ip.ipFwd;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabNatCfgN;
import net.freertr.tab.tabNatTraN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * unicast to multicast server
 *
 * @author matecsaba
 */
public class servUni2multi extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servUni2multi() {
    }

    /**
     * default port
     */
    public static final int portNum = 2262;

    /**
     * timeout value
     */
    public int timeout = 60000;

    /**
     * source interface
     */
    public cfgIfc sourceI;

    /**
     * source port
     */
    public int sourceP = -1;

    /**
     * target ipv4 range
     */
    public addrPrefix<addrIP> target4;

    /**
     * target ipv6 range
     */
    public addrPrefix<addrIP> target6;

    /**
     * target port
     */
    public int targetP = 1234;

    /**
     * log hits
     */
    public boolean logging;

    /**
     * script to execute
     */
    public cfgScrpt script;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server uni2multi .*! port " + portNum,
        "server uni2multi .*! protocol " + proto2string(protoAllDgrm),
        "server uni2multi .*! source port -1",
        "server uni2multi .*! target port 1234",
        "server uni2multi .*! no logging",
        "server uni2multi .*! no script",
        "server uni2multi .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (sourceI == null) {
            l.add(beg + "no source interface");
        } else {
            l.add(beg + "source interface " + sourceI.name);
        }
        l.add(beg + "source port " + sourceP);
        if (target4 == null) {
            l.add(beg + "no target ipv4");
        } else {
            l.add(beg + "target ipv4 " + addrPrefix.ip2str(target4));
        }
        if (target6 == null) {
            l.add(beg + "no target ipv6");
        } else {
            l.add(beg + "target ipv6 " + addrPrefix.ip2str(target6));
        }
        if (script == null) {
            l.add(beg + "no script");
        } else {
            l.add(beg + "script " + script.name);
        }
        l.add(beg + "target port " + targetP);
        l.add(beg + "timeout " + timeout);
        cmds.cfgLine(l, !logging, beg, "logging", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            negated = true;
            s = cmd.word();
        }
        if (s.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("source")) {
            s = cmd.word();
            if (s.equals("port")) {
                sourceP = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("interface")) {
                if (negated) {
                    sourceI = null;
                    return false;
                }
                sourceI = cfgAll.ifcFind(cmd.word(), false);
                if (sourceI == null) {
                    cmd.error("no such interface");
                    return false;
                }
                return false;
            }
            return false;
        }
        if (s.equals("target")) {
            s = cmd.word();
            if (s.equals("port")) {
                targetP = bits.str2num(cmd.word());
                return false;
            }
            if (s.equals("ipv4")) {
                if (negated) {
                    target4 = null;
                    return false;
                }
                addrPrefix<addrIPv4> pfx = new addrPrefix<addrIPv4>(new addrIPv4(), 0);
                if (pfx.fromString(cmd.word())) {
                    cmd.error("bad prefix");
                    return false;
                }
                target4 = addrPrefix.ip4toIP(pfx);
                return false;
            }
            if (s.equals("ipv6")) {
                if (negated) {
                    target6 = null;
                    return false;
                }
                addrPrefix<addrIPv6> pfx = new addrPrefix<addrIPv6>(new addrIPv6(), 0);
                if (pfx.fromString(cmd.word())) {
                    cmd.error("bad prefix");
                    return false;
                }
                target6 = addrPrefix.ip6toIP(pfx);
                return false;
            }
            return true;
        }
        if (s.equals("script")) {
            if (negated) {
                script = null;
                return false;
            }
            script = cfgAll.scrptFind(cmd.word(), false);
            if (script == null) {
                cmd.error("no such script");
                return false;
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  timeout                      timeout of client");
        l.add(null, "2 .    <num>                      milliseconds");
        l.add(null, "1 2  source                       specify translated source");
        l.add(null, "2 3    interface                  interface to use");
        l.add(null, "3 .      <name:ifc>               name of interface");
        l.add(null, "2 3    port                       port number");
        l.add(null, "3 .      <num>                    number");
        l.add(null, "1 2  target                       specify translated target");
        l.add(null, "2 3    port                       port number");
        l.add(null, "3 .      <num>                    number");
        l.add(null, "2 3    ipv4                       ipv4 range");
        l.add(null, "3 .      <addr>                   prefix");
        l.add(null, "2 3    ipv6                       ipv6 range");
        l.add(null, "3 .      <addr>                   prefix");
        l.add(null, "1 .  logging                      log translations");
        l.add(null, "1 2  script                       script to invoke");
        l.add(null, "2 .    <name:scr>                 name of script");
    }

    public String srvName() {
        return "uni2multi";
    }

    public int srvPort() {
        return portNum;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return false;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        id.setClosing();
        if (sourceI == null) {
            return true;
        }
        addrPrefix<addrIP> pfx;
        if (id.peerAddr.isIPv4()) {
            pfx = target4;
        } else {
            pfx = target6;
        }
        if (pfx == null) {
            return true;
        }
        addrIP src = sourceI.getFwdIfc(id.peerAddr).addr;
        if (src == null) {
            return true;
        }
        ipFwd fwd = srvVrf.getFwd(id.peerAddr);
        tabNatCfgN natC = new tabNatCfgN();
        tabNatTraN natT = natC.createEntry(pck, fwd.icmpCore);
        natT.newSrcAddr = src.copyBytes();
        if (sourceP == -1) {
            natT.newSrcPort = bits.random(0xe000, 0xf000);
        }
        if (sourceP > 0) {
            natT.newSrcPort = sourceP;
        }
        addrIP trg = new addrIP();
        trg.fillRandom();
        trg.setAnd(trg, pfx.wildcard);
        trg.setOr(trg, pfx.network);
        natT.newTrgAddr = trg;
        if (targetP == -1) {
            natT.newTrgPort = bits.random(0x1000, 0xf000);
        }
        if (targetP > 0) {
            natT.newTrgPort = targetP;
        }
        natT.logEnd = logging;
        natT.timeout = timeout;
        natT.reverse = natT;
        fwd.natTrns.add(natT);
        fwd.tableChanger();
        if (logging) {
            logger.info("created translation " + natT);
        }
        if (script != null) {
            script.doRound(bits.str2lst("set remote " + trg));
        }
        return false;
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

}
