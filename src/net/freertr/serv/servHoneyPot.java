package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntDns;
import net.freertr.enc.enc7bit;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
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
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm),
        "server honeypot .*! no script",
        "server honeypot .*! no router4",
        "server honeypot .*! no router6",
        "server honeypot .*! no route-details",
        "server honeypot .*! no route-hacked",
        "server honeypot .*! no route-distinguisher",
        "server honeypot .*! no route-vrf",
        "server honeypot .*! no resolve"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ip information configuration
     */
    public servGenIpInf ipInfo = new servGenIpInf();

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
        return genStrmStart(this, new pipeLine(128 * 1024, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        ipInfo.doGetCfg(beg, lst, filter);
    }

    public boolean srvCfgStr(cmds cmd) {
        return ipInfo.doCfgStr(cmd);
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
        l.add(null, "1 .  route-details                print prefix details");
        l.add(null, "1 .  route-hacked                 hackerize prefix details");
        l.add(null, "1 2  route-distinguisher          rd to use");
        l.add(null, "2 .    <rd>                       rd in ASnum:IDnum format");
        l.add(null, "1 2  route-vrf                    vrf to use");
        l.add(null, "2 .    <name:vrf>                 name of table");
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
        logger.info("honeypot hit from " + addr + " " + port);
        String s = lower.ipInfo.getRoute1liner(addr, port);
        pipe.linePut("you (" + s + ") have been logged!");
        List<String> lst = lower.ipInfo.getRouteDetails(addr, port);
        byte[] res = servGenIpInf.getRouteAscii(lst);
        pipe.morePut(res, 0, res.length);
        pipe.setClose();
        lower.ipInfo.doScript(addr);
    }

}
