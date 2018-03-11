package serv;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgDial;
import java.util.Comparator;
import java.util.List;
import pack.packRtp;

import pack.packSip;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import snd.sndConnect;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * session initiation protocol (rfc3261) server
 *
 * @author matecsaba
 */
public class servSip extends servGeneric implements prtServS {

    private tabGen<servSipDoer> users = new tabGen<servSipDoer>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server sip .*! port " + packSip.port,
        "server sip .*! protocol " + proto2string(protoAllDgrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "sip";
    }

    public int srvPort() {
        return packSip.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 180000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servSipDoer(this, pipe, id);
        return false;
    }

    /**
     * add client
     *
     * @param clnt client to add
     */
    protected void addClient(servSipDoer clnt) {
        users.put(clnt);
    }

    /**
     * delete client
     *
     * @param clnt client to add
     */
    protected void delClient(servSipDoer clnt) {
        users.del(clnt);
    }

    /**
     * get data port number
     *
     * @return port number
     */
    protected int getDataPort() {
        return (srvPort - 3) & 0xfffe;
    }

    /**
     * find client
     *
     * @param s name of client
     * @return client, null if not found
     */
    protected servSipDoer findClient(String s) {
        s = s.toLowerCase();
        for (int i = 0; i < users.size(); i++) {
            servSipDoer ntry = users.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.user.equals(s)) {
                return ntry;
            }
        }
        return null;
    }

}

class servSipDoer implements Runnable, Comparator<servSipDoer> {

    protected String user = "";

    private servSip lower;

    private pipeSide pipe;

    private prtGenConn conn;

    public servSipDoer(servSip parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        if (conn == null) {
            return;
        }
        new Thread(this).start();
    }

    public int compare(servSipDoer o1, servSipDoer o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    protected void sendPack(packSip src) {
        packSip pck = src.byteCopy(pipe);
        pck.writeDown();
    }

    private String getPeerContact() {
        return uniResLoc.addr2str(conn.peerAddr, conn.portRem);
    }

    private String getMyContact() {
        return "<sip:pbx@" + uniResLoc.addr2str(conn.iface.addr, conn.portLoc) + ">";
    }

    public void run() {
        if (debugger.servSipTraf) {
            logger.debug("started");
        }
        lower.addClient(this);
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        lower.delClient(this);
        if (debugger.servSipTraf) {
            logger.debug("stopped");
        }
    }

    private void doer() {
        packSip rx = new packSip(pipe);
        for (;;) {
            if (rx.readUp()) {
                return;
            }
            if (debugger.servSipTraf) {
                rx.dump("rx");
            }
            String src = rx.headerGet("From", 1);
            String trg = rx.headerGet("To", 1);
            String a = rx.command.trim();
            int i = a.indexOf(" ");
            if (i < 0) {
                continue;
            }
            a = a.substring(0, i).trim().toLowerCase();
            uniResLoc url = uniResLoc.parseOne("null://");
            if (a.equals("register") || a.equals("subscribe")) {
                url.fromString(uniResLoc.fromEmail(trg));
                user = url.username.toLowerCase();
                packSip tx = new packSip(pipe);
                tx.makeOk(rx, null, 120);
                tx.copyHeader(rx, "Contact");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("options")) {
                packSip tx = new packSip(pipe);
                tx.makeOk(rx, null, 0);
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            boolean rply = a.startsWith("sip/");
            if (rply) {
                a = src;
                src = trg;
                trg = a;
                a = "";
            }
            url.fromString(uniResLoc.fromEmail(trg));
            servSipDoer clnt = lower.findClient(url.username);
            if (clnt != null) {
                a = rx.headerGet("Contact", 1);
                if (a.length() < 1) {
                    rx.header.add("Contact: <sip:peer@" + getPeerContact() + ">");
                }
                rx.headerSet("Contact", 1, getMyContact());
                a = rx.headerGet("Via", 1);
                if (a.length() < 1) {
                    rx.header.add("Via: SIP/2.0/UDP " + getPeerContact() + ";rport;branch=" + bits.randomD());
                }
                if (debugger.servSipTraf) {
                    rx.dump("fwd");
                }
                clnt.sendPack(rx);
                continue;
            }
            if (rply) {
                continue;
            }
            if (a.equals("bye") || a.equals("cancel") || a.equals("ack")) {
                continue;
            }
            packSip tx = new packSip(pipe);
            if (!a.equals("invite")) {
                tx.makeErr(rx, null, "cant handle");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            addrIP adr = new addrIP();
            int prt = rx.sdpGetMediaEP(adr);
            if (prt < 1) {
                tx.makeErr(rx, null, "no endpoint");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            String via = rx.headerGet("Via", 1);
            if (via.length() < 1) {
                via = "SIP/2.0/UDP " + getPeerContact() + ";rport;branch=" + bits.randomD();
            }
            String cid = rx.headerGet("Call-Id", 1);
            if (cid.length() < 1) {
                cid = "" + bits.randomD();
            }
            String cnt = rx.headerGet("Contact", 1);
            if (cnt.length() < 1) {
                cnt = "sip:" + getPeerContact();
            } else {
                cnt = uniResLoc.fromEmail(cnt);
            }
            tx.makeNumeric("100 trying", rx, getMyContact());
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            trg += ";tag=" + bits.randomD();
            rx.headerSet("To", 1, trg);
            tx.makeNumeric("180 ringing", rx, getMyContact());
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            cfgDial peer = cfgAll.dialFind(src, trg, null);
            if (peer == null) {
                tx.makeErr(rx, null, "no such number");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (peer.makeCall(src, trg)) {
                tx.makeErr(rx, null, "failed to make call");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            tx.makeOk(rx, getMyContact(), 0);
            tx.makeSdp(conn.iface.addr, lower.getDataPort(), peer.getCodec());
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            packRtp data = new packRtp();
            if (data.startConnect(lower.srvVrf.getUdp(adr), new pipeLine(32768, true), conn.iface, lower.getDataPort(), adr, prt)) {
                tx.makeReq("BYE", cnt, trg, src, null, via, cid, bits.randomD(), 0);
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                peer.stopCall();
                continue;
            }
            sndConnect conner = new sndConnect(data, peer.getCall());
            for (;;) {
                if (conner.isClosed() != 0) {
                    break;
                }
                if (rx.isClosed() != 0) {
                    break;
                }
                if (rx.ready2rx() < 1) {
                    bits.sleep(1000);
                    continue;
                }
                if (rx.readUp()) {
                    return;
                }
                if (debugger.servSipTraf) {
                    rx.dump("rx");
                }
                a = rx.command.trim();
                i = a.indexOf(" ");
                if (i < 0) {
                    continue;
                }
                a = a.substring(0, i).trim().toLowerCase();
                if (a.equals("register") || a.equals("subscribe")) {
                    url.fromString(uniResLoc.fromEmail(trg));
                    user = url.username.toLowerCase();
                    tx.makeOk(rx, null, 120);
                    tx.copyHeader(rx, "Contact");
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                if (a.equals("options")) {
                    tx.makeOk(rx, null, 0);
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                if (a.equals("bye") || a.equals("cancel")) {
                    tx.makeOk(rx, null, 0);
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    break;
                }
            }
            conner.setClose();
            tx.makeReq("BYE", cnt, trg, src, null, via, cid, bits.randomD(), 0);
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            peer.stopCall();
        }
    }

}
