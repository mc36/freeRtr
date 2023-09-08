package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgScrpt;
import net.freertr.clnt.clntDns;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * honeypot server
 *
 */
public class servHoneyPot extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servHoneyPot() {
    }

    /**
     * port number
     */
    public static final int port = 22;

    /**
     * script to run
     */
    public cfgScrpt script;

    /**
     * resolve addresses
     */
    public boolean resolve;

    /**
     * resolve ipv4 prefixes
     */
    public ipRtr router4;

    /**
     * resolve ipv6 prefixes
     */
    public ipRtr router6;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm),
        "server honeypot .*! no script",
        "server honeypot .*! no router4",
        "server honeypot .*! no router6",
        "server honeypot .*! no resolve"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "honeypot";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        if (script == null) {
            lst.add(beg + "no script");
        } else {
            lst.add(beg + "script " + script.name);
        }
        if (router4 == null) {
            lst.add(beg + "no router4");
        } else {
            lst.add(beg + "router4 " + router4.routerGetName());
        }
        if (router6 == null) {
            lst.add(beg + "no router6");
        } else {
            lst.add(beg + "router6 " + router6.routerGetName());
        }
        cmds.cfgLine(lst, !resolve, beg, "resolve", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("script")) {
            script = cfgAll.scrptFind(cmd.word(), false);
            return false;
        }
        if (s.equals("resolve")) {
            resolve = true;
            return false;
        }
        if (s.equals("router4")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return false;
            }
            router4 = rtr.getRouter();
            return false;
        }
        if (s.equals("router6")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return false;
            }
            router6 = rtr.getRouter();
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("script")) {
            script = null;
            return false;
        }
        if (s.equals("resolve")) {
            resolve = false;
            return false;
        }
        if (s.equals("router4")) {
            router4 = null;
            return false;
        }
        if (s.equals("router6")) {
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return false;
            }
            router6 = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  script                       script to execute");
        l.add(null, "2 .    <name:scr>                 script name");
        l.add(null, "1 .  resolve                      resolve addresses");
        l.add(null, "1 2  router4                      lookup addresses");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, "3 .         <num:rtr>       process id");
        l.add(null, "1 2  router6                      lookup addresses");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, "3 .         <num:rtr>       process id");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        new servHoneyPotConn(this, pipe, id.peerAddr.copyBytes(), id.portRem);
        return false;
    }

}

class servHoneyPotConn implements Runnable {

    private servHoneyPot lower;

    private pipeSide pipe;

    private addrIP addr;

    private int port;

    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
        new Thread(this).start();
    }

    public void run() {
        String s = addr + " - " + port;
        if (lower.resolve) {
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
            s = s + " - " + clnt.getPTR();
        }
        ipRtr rtr = null;
        if (addr.isIPv4()) {
            rtr = lower.router4;
        } else {
            rtr = lower.router6;
        }
        if (rtr != null) {
            tabRouteEntry<addrIP> ntry = rtr.routerComputedU.route(addr);
            if (ntry != null) {
                s = s + " - " + addrPrefix.ip2str(ntry.prefix) + " - " + ntry.best.asPathStr() + " - " + ntry.best.asInfoStr() + " - " + ntry.best.asNameStr();
            }
        }
        pipe.linePut("you (" + s + ") have been logged!");
        pipe.setClose();
        logger.info("honeypot hit from " + s);
        if (lower.script == null) {
            return;
        }
        lower.script.doRound(bits.str2lst("set remote " + addr));
    }

}
