package serv;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgDial;
import cfg.cfgTrnsltn;
import java.util.ArrayList;
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
        "server sip .*! protocol " + proto2string(protoAllDgrm),
        "server sip .*! no mypeer",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * my peer
     */
    public String myPeer;

    /**
     * translate out src
     */
    public List<cfgTrnsltn> trnsOutSrc = new ArrayList<cfgTrnsltn>();

    /**
     * translate out dst
     */
    public List<cfgTrnsltn> trnsOutDst = new ArrayList<cfgTrnsltn>();

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
        cmds.cfgLine(lst, myPeer == null, beg, "mypeer", myPeer);
        for (int i = 0; i < trnsOutSrc.size(); i++) {
            lst.add(cmds.tabulator + "translate-out-calling " + trnsOutSrc.get(i).name);
        }
        for (int i = 0; i < trnsOutDst.size(); i++) {
            lst.add(cmds.tabulator + "translate-out-called " + trnsOutDst.get(i).name);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals("no");
        if (negated) {
            a = cmd.word();
        }
        if (a.equals("mypeer")) {
            myPeer = cmd.word();
            if (negated) {
                myPeer = null;
            }
            return false;
        }
        if (a.equals("translate-out-calling")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return false;
            }
            if (negated) {
                trnsOutSrc.remove(rule);
            } else {
                trnsOutSrc.add(rule);
            }
            return false;
        }
        if (a.equals("translate-out-called")) {
            cfgTrnsltn rule = cfgAll.trnsltnFind(cmd.word(), false);
            if (rule == null) {
                cmd.error("no such rule");
                return false;
            }
            if (negated) {
                trnsOutDst.remove(rule);
            } else {
                trnsOutDst.add(rule);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2    mypeer                  discard this dial peer on outgoing calls");
        l.add("2 .      <name>                dial peer name");
        l.add("1 2    translate-out-calling   translate outgoing calling string");
        l.add("2 .      <name>                rule name");
        l.add("1 2    translate-out-called    translate outgoing called string");
        l.add("2 .      <name>                rule name");
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

    private String getMyVia() {
        String a;
        if (conn.proto == null) {
            a = "UDP";
        } else {
            a = "TCP";
        }
        return "SIP/2.0/" + a + " " + uniResLoc.addr2str(conn.iface.addr, conn.portLoc);
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

    private void doMsg(packSip rx, packSip tx) {
        String src = rx.headerGet("From", 1);
        String trg = rx.headerGet("To", 1);
        src = cfgTrnsltn.doTranslate(lower.trnsOutSrc, src);
        trg = cfgTrnsltn.doTranslate(lower.trnsOutDst, trg);
        tx.makeNumeric("100 trying", rx, getMyContact());
        if (debugger.servSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        cfgDial per = null;
        if (lower.myPeer != null) {
            per = cfgAll.dialFind(lower.myPeer, false);
        }
        per = cfgAll.dialFind(src, trg, per);
        if (per == null) {
            tx.makeErr(rx, null, "no such number");
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return;
        }
        if (per.sendMsg(src, trg, rx.content)) {
            tx.makeErr(rx, null, "not went out");
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return;
        }
        tx.makeOk(rx, null, 0);
        if (debugger.servSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
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
                if (compare(clnt, this) == 0) {
                    packSip tx = new packSip(pipe);
                    tx.makeErr(rx, null, "packet to yourself");
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                if (user.length() < 1) {
                    packSip tx = new packSip(pipe);
                    tx.makeErr(rx, null, "not registered");
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                a = rx.headerGet("Contact", 1);
                if (a.length() < 1) {
                    a = "<sip:peer@" + getPeerContact() + ">";
                    rx.header.add("Contact: " + a);
                }
                rx.headerSet("Contact", 1, getMyContact());
                a = rx.headerGet("Via", 1);
                if (a.length() < 1) {
                    a = "SIP/2.0/UDP " + getPeerContact() + ";rport;branch=" + bits.randomD();
                    rx.header.add("Via: " + a);
                }
                rx.headerSet("Via", 1, packSip.updateVia(a, getMyVia()));
                if (debugger.servSipTraf) {
                    rx.dump("fwd");
                }
                clnt.sendPack(rx);
                continue;
            }
            if (rply) {
                continue;
            }
            if (a.equals("ack")) {
                continue;
            }
            packSip tx = new packSip(pipe);
            if (a.equals("bye") || a.equals("cancel")) {
                tx.makeOk(rx, null, 0);
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("message")) {
                doMsg(rx, tx);
                continue;
            }
            if (a.equals("notify")) {
                tx.makeOk(rx, null, 0);
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
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
            a = rx.headerGet("CSeq", 1) + " ";
            int csq = a.indexOf(" ");
            csq = bits.str2num(a.substring(0, csq).trim());
            a = rx.headerGet("Contact", 1);
            if (a.length() < 1) {
                a = "<sip:peer@" + getPeerContact() + ">";
                rx.header.add("Contact: " + a);
            }
            rx.headerSet("Contact", 1, getMyContact());
            String via = rx.headerGet("Via", 1);
            if (via.length() < 1) {
                via = "SIP/2.0/UDP " + getPeerContact() + ";rport;branch=" + bits.randomD();
                rx.header.add("Via: " + via);
            }
            via = packSip.updateVia(via, getMyVia());
            rx.headerSet("Via", 1, via);
            String cid = rx.headerGet("Call-Id", 1);
            if (cid.length() < 1) {
                cid = "" + bits.randomD();
            }
            String cnt = uniResLoc.fromEmail(getMyContact());
            tx.makeNumeric("100 trying", rx, getMyContact());
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            trg = packSip.updateTag(trg);
            rx.headerSet("To", 1, trg);
            tx.makeNumeric("180 ringing", rx, getMyContact());
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            String newSrc = cfgTrnsltn.doTranslate(lower.trnsOutSrc, src);
            String newTrg = cfgTrnsltn.doTranslate(lower.trnsOutDst, trg);
            cfgDial peer = null;
            if (lower.myPeer != null) {
                peer = cfgAll.dialFind(lower.myPeer, false);
            }
            peer = cfgAll.dialFind(newSrc, newTrg, peer);
            if (peer == null) {
                tx.makeErr(rx, null, "no such number");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            String rcd = peer.makeCall(newSrc, newTrg);
            if (rcd == null) {
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
                tx.makeReq("BYE", cnt, trg, src, getMyContact(), via, cid, csq + 1, 0);
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                peer.stopCall(rcd);
                continue;
            }
            sndConnect conner = new sndConnect(data, peer.getCall(rcd), peer.getCodec(), peer.getCodec());
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
                if (a.startsWith("sip/")) {
                    continue;
                }
                if (a.equals("ack")) {
                    continue;
                }
                if (a.equals("register") || a.equals("subscribe")) {
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
                if (a.equals("message")) {
                    doMsg(rx, tx);
                    continue;
                }
                if (a.equals("notify")) {
                    tx.makeOk(rx, null, 0);
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                if (a.equals("invite")) {
                    a = rx.headerGet("Call-Id", 1);
                    if (!a.equals(cid)) {
                        tx.makeErr(rx, null, "only one call");
                        if (debugger.servSipTraf) {
                            tx.dump("tx");
                        }
                        tx.writeDown();
                        continue;
                    }
                    rx.headerSet("Contact", 1, getMyContact());
                    rx.headerSet("Via", 1, via);
                    tx.makeNumeric("100 trying", rx, getMyContact());
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    rx.headerSet("To", 1, trg);
                    tx.makeNumeric("180 ringing", rx, getMyContact());
                    if (debugger.servSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    tx.makeOk(rx, getMyContact(), 0);
                    tx.makeSdp(conn.iface.addr, lower.getDataPort(), peer.getCodec());
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
                tx.makeErr(rx, null, "bad method");
                if (debugger.servSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
            }
            conner.setClose();
            tx.makeReq("BYE", cnt, trg, src, getMyContact(), via, cid, csq + 1, 0);
            if (debugger.servSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            peer.stopCall(rcd);
        }
    }

}
