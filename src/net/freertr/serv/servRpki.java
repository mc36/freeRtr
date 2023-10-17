package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.pack.packHolder;
import net.freertr.rtr.rtrRpkiSpeak;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouautN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * resource public key infrastructure (rfc6810) server
 *
 * @author matecsaba
 */
public class servRpki extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servRpki() {
    }

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server rpki .*! port " + rtrRpkiSpeak.portNum,
        "server rpki .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ipv4 prefixes
     */
    public tabGen<tabRouautN> pref4 = new tabGen<tabRouautN>();

    /**
     * ipv6 prefixes
     */
    public tabGen<tabRouautN> pref6 = new tabGen<tabRouautN>();

    /**
     * sequence
     */
    public int sequence;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "rpki";
    }

    public int srvPort() {
        return rtrRpkiSpeak.portNum;
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
        for (int i = 0; i < pref4.size(); i++) {
            lst.add(beg + "prefix " + pref4.get(i).toConfig());
        }
        for (int i = 0; i < pref6.size(); i++) {
            lst.add(beg + "prefix " + pref6.get(i).toConfig());
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("prefix")) {
            tabRouautN prf = new tabRouautN();
            if (prf.fromString(cmd)) {
                cmd.error("bad prefix");
                return false;
            }
            prf.srcIP = new addrIP();
            if (prf.prefix.network.isIPv4()) {
                pref4.put(prf);
            } else {
                pref6.put(prf);
            }
            sequence++;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("prefix")) {
            tabRouautN prf = new tabRouautN();
            if (prf.fromString(cmd)) {
                cmd.error("bad prefix");
                return false;
            }
            if (prf.prefix.network.isIPv4()) {
                pref4.del(prf);
            } else {
                pref6.del(prf);
            }
            sequence++;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  prefix                       setup a prefix");
        l.add(null, "2 3    <net/mask>                 network in perfix/mask format");
        l.add(null, "3 4      <num>                    maximum prefix length");
        l.add(null, "4 5,.      <num>                  as number");
        l.add(null, "5 .          [num]                preference");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servRpkiConn(this, pipe, id.peerAddr);
        return false;
    }

}

class servRpkiConn implements Runnable {

    public final servRpki lower;

    public final pipeSide conn;

    public final addrIP peer;

    public int session;

    public servRpkiConn(servRpki parent, pipeSide pipe, addrIP rem) {
        lower = parent;
        conn = pipe;
        peer = rem;
        session = bits.randomW();
        new Thread(this).start();
    }

    public void run() {
        if (debugger.servRpkiTraf) {
            logger.debug("starting " + peer);
        }
        try {
            rtrRpkiSpeak pck = new rtrRpkiSpeak(new packHolder(true, true), conn);
            for (;;) {
                if (pck.doOneServRnd(lower.sequence, session, lower.pref4, lower.pref6)) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
        if (debugger.servRpkiTraf) {
            logger.debug("stoping " + peer);
        }
    }

}
