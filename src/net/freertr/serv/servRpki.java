package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.pack.packHolder;
import net.freertr.rtr.rtrRpkiSpeak;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
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
    public tabGen<servRpkiEntry> pref4 = new tabGen<servRpkiEntry>();

    /**
     * ipv6 prefixes
     */
    public tabGen<servRpkiEntry> pref6 = new tabGen<servRpkiEntry>();

    /**
     * sequence
     */
    public int seq;

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
            lst.add(beg + "prefix4 " + pref4.get(i));
        }
        for (int i = 0; i < pref6.size(); i++) {
            lst.add(beg + "prefix6 " + pref6.get(i));
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("prefix4")) {
            servRpkiEntry prf = new servRpkiEntry();
            if (prf.fromString(cmd)) {
                return false;
            }
            pref4.put(prf);
            seq++;
            return false;
        }
        if (s.equals("prefix6")) {
            servRpkiEntry prf = new servRpkiEntry();
            if (prf.fromString(cmd)) {
                return false;
            }
            pref6.put(prf);
            seq++;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("prefix4")) {
            servRpkiEntry prf = new servRpkiEntry();
            if (prf.fromString(cmd)) {
                return false;
            }
            pref4.del(prf);
            seq++;
            return false;
        }
        if (s.equals("prefix6")) {
            servRpkiEntry prf = new servRpkiEntry();
            if (prf.fromString(cmd)) {
                return false;
            }
            pref6.del(prf);
            seq++;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  prefix4                      set ipv4 prefix");
        l.add(null, "2 3    <net/mask>                 network in perfix/mask format");
        l.add(null, "3 4      <num>                    maximum prefix length");
        l.add(null, "4 .        <num>                  as number");
        l.add(null, "1 2  prefix6                      set ipv6 prefix");
        l.add(null, "2 3    <net/mask>                 network in perfix/mask format");
        l.add(null, "3 4      <num>                    maximum prefix length");
        l.add(null, "4 .        <num>                  as number");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servRpkiConn(this, pipe);
        return false;
    }

}

class servRpkiEntry implements Comparator<servRpkiEntry> {

    public addrPrefix<addrIP> pref;

    public int max;

    public int as;

    public int compare(servRpkiEntry o1, servRpkiEntry o2) {
        return o1.pref.compare(o1.pref, o2.pref);
    }

    public boolean fromString(cmds cmd) {
        pref = addrPrefix.str2ip(cmd.word());
        if (pref == null) {
            return true;
        }
        max = bits.str2num(cmd.word());
        as = bits.str2num(cmd.word());
        return false;
    }

    public String toString() {
        return pref + " " + max + " " + as;
    }

}

class servRpkiConn implements Runnable {

    public final servRpki lower;

    public final pipeSide conn;

    public int session;

    public servRpkiConn(servRpki parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        session = bits.randomW();
        new Thread(this).start();
    }

    public void run() {
        try {
            rtrRpkiSpeak pck = new rtrRpkiSpeak(new packHolder(true, true), conn);
            for (;;) {
                if (doRound(pck)) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
    }

    private boolean doRound(rtrRpkiSpeak pck) {
        if (pck.recvPack()) {
            return true;
        }
        if (debugger.servRpkiTraf) {
            logger.debug("rx " + pck.dump());
        }
        switch (pck.typ) {
            case rtrRpkiSpeak.msgSerialQuery:
                if (pck.serial != lower.seq) {
                    pck.typ = rtrRpkiSpeak.msgCacheReset;
                    pck.sendPack();
                    if (debugger.servRpkiTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                    return false;
                }
                pck.typ = rtrRpkiSpeak.msgCacheReply;
                pck.sess = session;
                pck.sendPack();
                if (debugger.servRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                pck.typ = rtrRpkiSpeak.msgEndData;
                pck.sendPack();
                if (debugger.servRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                return false;
            case rtrRpkiSpeak.msgResetQuery:
                pck.typ = rtrRpkiSpeak.msgCacheReply;
                pck.sess = session;
                pck.sendPack();
                if (debugger.servRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                for (int i = 0; i < lower.pref4.size(); i++) {
                    servRpkiEntry ntry = lower.pref4.get(i);
                    pck.typ = rtrRpkiSpeak.msgIpv4addr;
                    pck.pref = ntry.pref;
                    pck.max = ntry.max;
                    pck.as = ntry.as;
                    pck.sendPack();
                    if (debugger.servRpkiTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                }
                for (int i = 0; i < lower.pref6.size(); i++) {
                    servRpkiEntry ntry = lower.pref6.get(i);
                    pck.typ = rtrRpkiSpeak.msgIpv6addr;
                    pck.pref = ntry.pref;
                    pck.max = ntry.max;
                    pck.as = ntry.as;
                    pck.sendPack();
                    if (debugger.servRpkiTraf) {
                        logger.debug("tx " + pck.dump());
                    }
                }
                pck.typ = rtrRpkiSpeak.msgEndData;
                pck.serial = lower.seq;
                pck.sendPack();
                if (debugger.servRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                return false;
            default:
                return false;
        }
    }

}
