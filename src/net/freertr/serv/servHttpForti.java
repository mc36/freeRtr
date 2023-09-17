package net.freertr.serv;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrType;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgIfc;
import net.freertr.enc.encBase64;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcPpp;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packForti;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * http fortigate connection
 *
 * @author matecsaba
 */
public class servHttpForti implements Runnable, ifcDn {

    private final servHttpConn lower;

    private cfgIfc clnd = null;

    private counter cntr = new counter();

    private final pipeSide pipe;

    private ifcUp upper = new ifcNull();

    /**
     * create instance
     *
     * @param conn connection
     */
    public servHttpForti(servHttpConn conn) {
        lower = conn;
        pipe = conn.pipe;
    }

    protected void serveReq(servHttpHost gotHost) {
        String pn = lower.gotUrl.toPathName();
        if (pn.equals("remote/logincheck")) {
            lower.addHdr("Set-Cookie: SVPNCOOKIE=" + encBase64.encodeString(lower.gotUrl.getParam("username") + "|" + lower.gotUrl.getParam("credential")) + "; path=/; secure; httponly");
            lower.sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("remote/index")) {
            lower.sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("remote/fortisslvpn")) {
            lower.sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
            return;
        }
        if (!pn.equals("remote/fortisslvpn_xml")) {
            lower.sendRespError(404, "not found");
            return;
        }
        if (lower.gotCook.size() < 1) {
            lower.sendRespError(401, "unauthorized");
            return;
        }
        String a = lower.gotCook.get(0);
        int i = a.indexOf("=");
        if (i < 0) {
            lower.sendRespError(401, "unauthorized");
            return;
        }
        a = encBase64.decodeString(a.substring(i + 1, a.length()));
        if (a == null) {
            lower.sendRespError(401, "unauthorized");
            return;
        }
        i = a.indexOf("|");
        if (i < 0) {
            lower.sendRespError(401, "unauthorized");
            return;
        }
        authResult res = gotHost.authenticList.authUserPass(a.substring(0, i), a.substring(i + 1, a.length()));
        if (res.result != authResult.authSuccessful) {
            lower.sendRespError(401, "unauthorized");
            return;
        }
        lower.sendTextHeader("200 OK", "text/html", "<html></html>".getBytes());
        for (;;) {
            a = pipe.lineGet(1);
            if (debugger.servHttpTraf) {
                logger.debug("rx '" + a + "'");
            }
            if (a.length() < 1) {
                break;
            }
        }
        clnd = gotHost.allowForti.cloneStart(this);
        new Thread(this).start();
        lower.gotKeep = false;
        lower.pipe = null;
    }

    public void run() {
        if (clnd.ip4polA != null) {
            clnd.addr4changed(clnd.addr4, clnd.mask4, clnd.ip4polA);
        }
        if (clnd.ip6polA != null) {
            clnd.addr6changed(clnd.addr6, clnd.mask6, clnd.ip6polA);
        }
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        clnd.cloneStop();
    }

    private boolean doRound() {
        packForti pckS = new packForti(pipe);
        packHolder pckB = new packHolder(true, true);
        if (pckS.recvPack(pckB)) {
            return true;
        }
        pckB.msbPutW(0, ifcPpp.preamble);
        pckB.putSkip(2);
        pckB.merge2beg();
        upper.recvPack(pckB);
        return false;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        packForti ps = new packForti(pipe);
        pck.putDefaults();
        pck.getSkip(2);
        ps.sendPack(pck);
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeDn() {
        pipe.setClose();
    }

    public void flapped() {
        pipe.setClose();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public String toString() {
        return "forti";
    }

}
