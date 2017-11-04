package serv;

import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import addr.addrType;
import java.util.Comparator;
import java.util.List;
import pack.packRpki;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * resource public key infrastructure (rfc6810) server
 *
 * @author matecsaba
 */
public class servRpki extends servGeneric implements prtServS {

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server rpki .*! port " + packRpki.portNum,
        "server rpki .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ipv4 prefixes
     */
    public tabGen<servRpkiEntry<addrIPv4>> pref4 = new tabGen<servRpkiEntry<addrIPv4>>();

    /**
     * ipv6 prefixes
     */
    public tabGen<servRpkiEntry<addrIPv6>> pref6 = new tabGen<servRpkiEntry<addrIPv6>>();

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
        return packRpki.portNum;
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

    public void srvShRun(String beg, List<String> lst) {
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
            servRpkiEntry<addrIPv4> prf = new servRpkiEntry<addrIPv4>();
            if (prf.fromString(new addrIPv4(), cmd)) {
                return false;
            }
            pref4.put(prf);
            seq++;
            return false;
        }
        if (s.equals("prefix6")) {
            servRpkiEntry<addrIPv6> prf = new servRpkiEntry<addrIPv6>();
            if (prf.fromString(new addrIPv6(), cmd)) {
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
            servRpkiEntry<addrIPv4> prf = new servRpkiEntry<addrIPv4>();
            if (prf.fromString(new addrIPv4(), cmd)) {
                return false;
            }
            pref4.del(prf);
            seq++;
            return false;
        }
        if (s.equals("prefix6")) {
            servRpkiEntry<addrIPv6> prf = new servRpkiEntry<addrIPv6>();
            if (prf.fromString(new addrIPv6(), cmd)) {
                return false;
            }
            pref6.del(prf);
            seq++;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  prefix4                      set ipv4 prefix");
        l.add("2 3    <net/mask>                 network in perfix/mask format");
        l.add("3 4      <num>                    maximum prefix length");
        l.add("4 .        <num>                  as number");
        l.add("1 2  prefix6                      set ipv6 prefix");
        l.add("2 3    <net/mask>                 network in perfix/mask format");
        l.add("3 4      <num>                    maximum prefix length");
        l.add("4 .        <num>                  as number");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servRpkiConn(this, pipe);
        return false;
    }

}

class servRpkiEntry<T extends addrType> implements Comparator<servRpkiEntry<T>> {

    public addrPrefix<T> pref;

    public int max;

    public int as;

    public int compare(servRpkiEntry<T> o1, servRpkiEntry<T> o2) {
        return o1.pref.compare(o1.pref, o2.pref);
    }

    public boolean fromString(T adr, cmds cmd) {
        pref = new addrPrefix<T>(adr, 0);
        if (pref.fromString(cmd.word())) {
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

    public servRpki lower;

    public pipeSide conn;

    public int session;

    public servRpkiConn(servRpki parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        session = bits.randomW();
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                packRpki pck = new packRpki();
                if (pck.recvPack(conn)) {
                    break;
                }
                if (debugger.servRpkiTraf) {
                    logger.debug("rx " + pck.dump());
                }
                switch (pck.typ) {
                    case packRpki.msgSerialQuery:
                        if (pck.serial != lower.seq) {
                            pck.typ = packRpki.msgCacheReset;
                            pck.sendPack(conn);
                            if (debugger.servRpkiTraf) {
                                logger.debug("tx " + pck.dump());
                            }
                        } else {
                            pck.typ = packRpki.msgCacheReply;
                            pck.sess = session;
                            pck.sendPack(conn);
                            if (debugger.servRpkiTraf) {
                                logger.debug("tx " + pck.dump());
                            }
                            pck.typ = packRpki.msgEndData;
                            pck.sendPack(conn);
                            if (debugger.servRpkiTraf) {
                                logger.debug("tx " + pck.dump());
                            }
                        }
                        break;
                    case packRpki.msgResetQuery:
                        pck.typ = packRpki.msgCacheReply;
                        pck.sess = session;
                        pck.sendPack(conn);
                        if (debugger.servRpkiTraf) {
                            logger.debug("tx " + pck.dump());
                        }
                        for (int i = 0; i < lower.pref4.size(); i++) {
                            servRpkiEntry<addrIPv4> ntry = lower.pref4.get(i);
                            pck.typ = packRpki.msgIpv4addr;
                            pck.pref4 = ntry.pref;
                            pck.max = ntry.max;
                            pck.as = ntry.as;
                            pck.sendPack(conn);
                            if (debugger.servRpkiTraf) {
                                logger.debug("tx " + pck.dump());
                            }
                        }
                        for (int i = 0; i < lower.pref6.size(); i++) {
                            servRpkiEntry<addrIPv6> ntry = lower.pref6.get(i);
                            pck.typ = packRpki.msgIpv6addr;
                            pck.pref6 = ntry.pref;
                            pck.max = ntry.max;
                            pck.as = ntry.as;
                            pck.sendPack(conn);
                            if (debugger.servRpkiTraf) {
                                logger.debug("tx " + pck.dump());
                            }
                        }
                        pck.typ = packRpki.msgEndData;
                        pck.serial = lower.seq;
                        pck.sendPack(conn);
                        if (debugger.servRpkiTraf) {
                            logger.debug("tx " + pck.dump());
                        }
                        break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClose();
    }

}
