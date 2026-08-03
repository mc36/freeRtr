package org.freertr.serv;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgIfc;
import org.freertr.enc.encUrl;
import org.freertr.enc.encXml;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packAnyconn;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * http anyconnect connection
 *
 * @author matecsaba
 */
public class servHttpAnyconn implements Runnable, ifcDn {

    private cfgIfc clnd = null;

    private counter cntr = new counter();

    private final pipeSide pipe;

    private final servHttpConn lower;

    private ifcUp upper = new ifcNull();

    /**
     * create instance
     *
     * @param conn lower layer
     */
    public servHttpAnyconn(servHttpConn conn) {
        lower = conn;
        pipe = conn.pipe;
    }

    /**
     * start the server
     *
     * @param cfg config to use
     */
    protected void doStart(servHttpHost cfg) {
        clnd = cfg.allowAnyconn.cloneStart(this);
        lower.addHdr("X-CSTP-Version: 1");
        if (clnd.ip4polA != null) {
            lower.addHdr("X-CSTP-Address: " + clnd.ip4polA);
            lower.addHdr("X-CSTP-Netmask: 255.255.255.255");
        }
        if (clnd.ip6polA != null) {
            lower.addHdr("X-CSTP-Address-IP6: " + clnd.ip6polA + "/128");
        }
        lower.addHdr("X-CSTP-Keep: false");
        lower.addHdr("X-CSTP-Lease-Duration: 43200");
        lower.addHdr("X-CSTP-MTU: 1500");
        lower.addHdr("X-CSTP-DPD: 300");
        lower.addHdr("X-CSTP-Disconnected-Timeout: 2100");
        lower.addHdr("X-CSTP-Idle-Timeout: 2100");
        lower.addHdr("X-CSTP-Session-Timeout: 0");
        lower.addHdr("X-CSTP-Keepalive: 30");
        lower.sendRespHeader("200 ok", -1, null);
        lower.gotKeep = false;
        lower.pipe = null;
        logger.startThread(this);
    }

    /**
     * serve one request
     * @param cfg config to use
     */
    protected void serveReq(servHttpHost cfg) {
        lower.gotUrl.port = lower.lower.srvPort;
        if (lower.lower.secProto != 0) {
            lower.gotUrl.proto = "https";
        } else {
            lower.gotUrl.proto = "http";
        }
        lower.addHdr("Set-Cookie: webvpncontext=00@defctx; path=/; Secure");
        String pn = lower.gotUrl.toPathName();
        if (pn.length() <= 0) {
            lower.addHdr("Location: " + lower.gotUrl.toURL(true, false, false, false) + "webvpn.html");
            lower.sendRespHeader("303 see other", -1, "text/html");
            return;
        }
        if (pn.equals("1/index.html")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("1/Linux")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("1/Linux_64")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("1/Windows")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("1/Darwin_i386")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.equals("1/VPNManifest.xml")) {
            lower.sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<vpn rev=\"1.0\">\n</vpn>\n").getBytes());
            return;
        }
        if (pn.equals("1/binaries/update.txt")) {
            lower.sendTextHeader("200 ok", "text/html", "0,0,0000\\n".getBytes());
            return;
        }
        if (pn.equals("logout")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (pn.startsWith(" CSCOT /")) {
            lower.sendTextHeader("200 ok", "text/html", "<html></html>".getBytes());
            return;
        }
        if (!pn.equals("webvpn.html")) {
            lower.sendRespError(404, "not found");
            return;
        }
        String s = new String(lower.gotBytes);
        lower.gotBytes = new byte[0];
        encUrl srv = encUrl.parseOne("http://x/y?" + s);
        lower.gotUrl.param.addAll(srv.param);
        lower.gotUrl.username = lower.gotUrl.getParam("username");
        lower.gotUrl.password = lower.gotUrl.getParam("password");
        if (lower.gotUrl.username == null) {
            lower.gotUrl.username = "";
        }
        if (lower.gotUrl.password == null) {
            lower.gotUrl.password = "";
        }
        authResult res = cfg.authenticList.authUserPass(lower.gotUrl.username, lower.gotUrl.password);
        if (res.result != authResult.authSuccessful) {
            lower.addHdr("X-Transcend-Version: 1");
            lower.sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<auth id=\"main\"><title>login</title><message>enter username and password</message><form method=\"post\" action=\"webvpn.html\"><input type=\"text\" label=\"username:\" name=\"username\" value=\"\" /><input type=\"password\" label=\"password:\" name=\"password\" value=\"\" /><input type=\"submit\" name=\"login\" value=\"login\" /></form></auth>").getBytes());
            return;
        }
        lower.addHdr("Set-Cookie: webvpn=00@0168430307@00071@3702439125@3326207229@defctx; path=/; Secure");
        lower.addHdr("Set-Cookie: webvpnc=bu:0/&p:t&iu:1/&sh:%s; path=/; Secure");
        lower.sendTextHeader("200 ok", "text/xml", (encXml.header + "\n<auth id=\"success\"><title>vpn</title><message>success</message><success/></auth>").getBytes());
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
        packAnyconn pckS = new packAnyconn(pipe);
        packHolder pckB = new packHolder(true, true);
        if (pckS.recvPack(pckB)) {
            return true;
        }
        switch (pckS.msgTyp) {
            case packAnyconn.typData:
                int i = ifcEther.guessEtherType(pckB);
                if (i < 0) {
                    logger.info("got bad protocol from " + lower.peer);
                    break;
                }
                pckB.msbPutW(0, i); // ethertype
                pckB.putSkip(2);
                pckB.merge2beg();
                upper.recvPack(pckB);
                break;
            case packAnyconn.typDpdReq:
                pckS.msgTyp = packAnyconn.typDpdRep;
                pckS.sendPack(pckB);
                break;
            case packAnyconn.typDpdRep:
                break;
            case packAnyconn.typKeep:
                break;
            case packAnyconn.typTerm:
                return true;
            case packAnyconn.typDisc:
                return true;
            default:
                logger.info("unknown type: " + pckS.msgTyp);
                break;
        }
        return false;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        packAnyconn ps = new packAnyconn(pipe);
        ps.msgTyp = packAnyconn.typData;
        pck.putDefaults();
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
        return "anyconn";
    }

}
