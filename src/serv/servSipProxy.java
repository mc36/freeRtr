package serv;

import java.util.Comparator;
import java.util.List;

import pack.packSip;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * session initiation protocol (rfc3261) server
 *
 * @author matecsaba
 */
public class servSipProxy extends servGeneric implements prtServS {

    private tabGen<servSipProxyDoer> users = new tabGen<servSipProxyDoer>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server sipproxy .*! port " + packSip.port,
        "server sipproxy .*! protocol " + proto2string(protoAllDgrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "sipproxy";
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
        new servSipProxyDoer(this, pipe, id);
        return false;
    }

    /**
     * add client
     *
     * @param clnt client to add
     */
    protected void addClient(servSipProxyDoer clnt) {
        users.add(clnt);
    }

    /**
     * delete client
     *
     * @param clnt client to add
     */
    protected void delClient(servSipProxyDoer clnt) {
        users.del(clnt);
    }

    /**
     * find client
     *
     * @param s name of client
     * @return client, null if not found
     */
    protected servSipProxyDoer findClient(String s) {
        servSipProxyDoer clnt = new servSipProxyDoer(null, null, null);
        clnt.user = s.toLowerCase();
        return users.find(clnt);
    }

}

class servSipProxyDoer implements Runnable, Comparator<servSipProxyDoer> {

    protected String user = "";

    private boolean reged = false;

    private servSipProxy lower;

    private pipeSide pipe;

    private prtGenConn conn;

    public servSipProxyDoer(servSipProxy parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        if (conn == null) {
            return;
        }
        new Thread(this).start();
    }

    public int compare(servSipProxyDoer o1, servSipProxyDoer o2) {
        return o1.user.compareTo(o2.user);
    }

    protected void sendPack(packSip src) {
        packSip pck = new packSip(pipe);
        pck.byteCopy(src);
        pck.writeDown();
    }

    public void run() {
        if (debugger.servSipProxyTraf) {
            logger.debug("started");
        }
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        if (reged) {
            lower.delClient(this);
        }
        if (debugger.servSipProxyTraf) {
            logger.debug("stopped");
        }
    }

    private void doer() {
        packSip rx = new packSip(pipe);
        for (;;) {
            uniResLoc url = uniResLoc.parseOne("null://");
            if (rx.readUp()) {
                return;
            }
            if (debugger.servSipProxyTraf) {
                rx.dump("rx");
            }
            String src = rx.headerGet("from", 1);
            String trg = rx.headerGet("to", 1);
            String a = rx.command.trim();
            int i = a.indexOf(" ");
            if (i < 0) {
                continue;
            }
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("register")) {
                if (!reged) {
                    url.fromString(uniResLoc.fromEmail(trg));
                    user = url.username.toLowerCase();
                    reged = true;
                    lower.addClient(this);
                }
                packSip tx = new packSip(pipe);
                tx.makeOk(rx, null, 120);
                tx.copyHeader(rx, "contact");
                tx.writeDown();
                if (debugger.servSipProxyTraf) {
                    tx.dump("tx");
                }
                continue;
            }
            if (a.equals("options")) {
                packSip tx = new packSip(pipe);
                tx.makeOk(rx, null, 0);
                tx.writeDown();
                if (debugger.servSipProxyTraf) {
                    tx.dump("tx");
                }
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
            servSipProxyDoer clnt = lower.findClient(url.username);
            if (clnt != null) {
                clnt.sendPack(rx);
                continue;
            }
            if (rply) {
                continue;
            }
            packSip tx = new packSip(pipe);
            tx.makeNoExt(rx, null);
            tx.writeDown();
            if (debugger.servSipProxyTraf) {
                tx.dump("tx");
            }
        }
    }

}
