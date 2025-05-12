package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.sec.secHttp2;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.enc.encUrl;
import org.freertr.user.userFlash;
import org.freertr.util.logger;

/**
 * http connection
 *
 * @author matecsaba
 */
public class servHttpConn implements Runnable {

    /**
     * parent
     */
    protected final servHttp lower;

    /**
     * connection
     */
    protected final prtGenConn conn;

    /**
     * address
     */
    protected final addrIP peer;

    /**
     * pipeline
     */
    protected pipeSide pipe;

    /**
     * got command
     */
    protected String gotCmd;

    /**
     * got head request
     */
    protected boolean gotHead;

    /**
     * got url
     */
    protected encUrl gotUrl;

    /**
     * got authentication
     */
    protected String gotAuth;

    /**
     * got host
     */
    protected servHttpHost gotHost;

    /**
     * got protocol
     */
    protected int gotVer;

    /**
     * got keepalive
     */
    protected boolean gotKeep;

    /**
     * got depth
     */
    protected boolean gotDepth;

    /**
     * got compression, 1=deflate, 2=gzip
     */
    protected int gotCompr;

    /**
     * got destination
     */
    protected String gotDstntn;

    /**
     * got cookies
     */
    protected final List<String> gotCook = new ArrayList<String>();

    /**
     * got content
     */
    protected byte[] gotBytes;

    /**
     * got agent
     */
    protected String gotAgent;

    /**
     * got referrer
     */
    protected String gotReferer;

    /**
     * got websocket
     */
    protected String gotWebsock;

    /**
     * got range
     */
    protected String gotRange;

    /**
     * through tls port
     */
    private boolean secured;

    /**
     * headers to send
     */
    private List<String> headers = new ArrayList<String>();

    /**
     * create instance
     *
     * @param parent parent
     * @param stream pipeline
     * @param id connection
     */
    public servHttpConn(servHttp parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        pipe = stream;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        peer = new addrIP();
        peer.setAddr(id.peerAddr);
        conn = id;
        secured = id.portLoc == lower.secondPort;
        new Thread(this).start();
    }

    public String toString() {
        return "" + peer;
    }

    /**
     * send one line
     *
     * @param a line to send
     */
    protected void sendLn(String a) {
        if (debugger.servHttpTraf) {
            logger.debug("tx '" + a + "'");
        }
        pipe.linePut(a);
    }

    /**
     * add one header
     *
     * @param s header string
     */
    protected void addHdr(String s) {
        headers.add(s);
    }

    /**
     * close pipe
     */
    protected void clsPip() {
        if (pipe == null) {
            return;
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

    /**
     * send response header
     *
     * @param head reply code and text
     * @param size payload size
     * @param type content type
     */
    protected void sendRespHeader(String head, long size, String type) {
        if (head != null) {
            sendLn("HTTP/" + (gotVer / 10) + "." + (gotVer % 10) + " " + head);
        }
        sendLn("Server: " + cfgInit.versionAgent);
        if (type != null) {
            sendLn("Content-Type: " + type);
        }
        if (!gotKeep || lower.singleRequest) {
            sendLn("Connection: Close");
        } else {
            sendLn("Connection: Keep-Alive");
            sendLn("Keep-Alive: TimeOut=60, Max=25");
            if (size < 0) {
                size = 0;
            }
        }
        if (size >= 0) {
            sendLn("Content-Length: " + size);
        }
        for (int i = 0; i < headers.size(); i++) {
            sendLn(headers.get(i));
        }
        sendLn("");
    }

    /**
     * send response header
     *
     * @param head reply code and text
     * @param type content type
     * @param buf1 content type
     */
    protected void sendTextHeader(String head, String type, byte[] buf1) {
        if (gotHead) {
            sendRespHeader(head, buf1.length, type);
            return;
        }
        if (gotCompr == 0) {
            sendRespHeader(head, buf1.length, type);
            pipe.morePut(buf1, 0, buf1.length);
            return;
        }
        Deflater cmp;
        String enc;
        byte[] buf3;
        byte[] buf4;
        switch (gotCompr) {
            case 1:
                cmp = new Deflater();
                enc = "deflate";
                buf3 = new byte[0];
                buf4 = new byte[0];
                break;
            case 2:
                cmp = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
                enc = "gzip";
                buf3 = userFlash.getGzipHdr();
                buf4 = userFlash.getGzipTrl(buf1);
                break;
            default:
                sendRespHeader(head, buf1.length, type);
                pipe.morePut(buf1, 0, buf1.length);
                return;
        }
        cmp.setInput(buf1);
        cmp.finish();
        byte[] buf2 = new byte[buf1.length];
        int i = cmp.deflate(buf2);
        if (i >= buf2.length) {
            sendRespHeader(head, buf1.length, type);
            pipe.morePut(buf1, 0, buf1.length);
            return;
        }
        headers.add("Content-Encoding: " + enc);
        sendRespHeader(head, buf3.length + i + buf4.length, type);
        pipe.morePut(buf3, 0, buf3.length);
        pipe.morePut(buf2, 0, i);
        pipe.morePut(buf4, 0, buf4.length);
    }

    /**
     * send error response
     *
     * @param code reply code
     * @param text error message
     */
    protected void sendRespError(int code, String text) {
        gotKeep = false;
        String s;
        if (lower.error == null) {
            s = servHttp.htmlHead + servHttpUtil.getStyle(this) + "<title>error</title></head><body>error: " + text + "</body></html>";
        } else {
            s = "" + lower.error;
            s = s.replaceAll("<errorcode>", "" + code);
            s = s.replaceAll("<errortext>", "" + text);
        }
        sendRespHeader(code + " " + text, s.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(s);
    }

    /**
     * send redirect response
     *
     * @param where new location
     */
    protected void sendFoundAt(String where) {
        gotKeep = false;
        String s = servHttp.htmlHead + servHttpUtil.getStyle(this) + "<title>moved</title></head><body>moved to <a href=\"" + where + "\">" + where + "</a>. you will be redirected.</body></html>\n";
        headers.add("Location: " + where);
        sendRespHeader("301 moved", s.length(), "text/html");
        if (gotHead) {
            return;
        }
        pipe.strPut(s);
    }

    private boolean readRequest() {
        gotUrl = encUrl.parseOne("null://");
        gotVer = 0;
        gotHead = false;
        gotKeep = false;
        gotDepth = false;
        gotCompr = 0;
        int gotSize = 0;
        String gotType = "";
        String gotExpect = null;
        String gotUpgrade = null;
        gotAuth = null;
        gotCook.clear();
        headers.clear();
        gotBytes = null;
        gotDstntn = null;
        gotAgent = "";
        gotRange = null;
        gotReferer = "";
        gotWebsock = null;
        gotCmd = "";
        if (pipe == null) {
            return true;
        }
        gotCmd = pipe.lineGet(1);
        if (gotCmd.length() < 1) {
            return true;
        }
        if (debugger.servHttpTraf) {
            logger.debug("rx '" + gotCmd + "'");
        }
        if (gotCmd.equals(secHttp2.magicCmd)) {
            gotCmd = "";
            secHttp2 ht2 = new secHttp2(pipe, new pipeLine(lower.bufSiz, false));
            if (ht2.startServer(true)) {
                return true;
            }
            pipe = ht2.getPipe();
            gotCmd = pipe.lineGet(1);
            if (gotCmd.length() < 1) {
                return true;
            }
            if (debugger.servHttpTraf) {
                logger.debug("rx '" + gotCmd + "'");
            }
        }
        int i = gotCmd.toLowerCase().lastIndexOf(" http/");
        if (i > 0) {
            String s = gotCmd.substring(i + 6, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            i = s.indexOf(".");
            if (i < 0) {
                gotVer = bits.str2num(s) * 10;
            } else {
                gotVer = bits.str2num(s.substring(0, i) + s.substring(i + 1, s.length()));
            }
        }
        if ((gotVer < 10) || (gotVer > 11)) {
            gotVer = 10;
        }
        i = gotCmd.indexOf(" ");
        if (i > 0) {
            String s = gotCmd.substring(i + 1, gotCmd.length());
            gotCmd = gotCmd.substring(0, i);
            gotUrl.fromString(s);
        }
        gotCmd = gotCmd.trim().toLowerCase();
        for (;;) {
            String s = pipe.lineGet(1);
            i = s.indexOf(":");
            String a;
            if (i < 0) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i);
                s = s.substring(i + 1, s.length());
            }
            a = a.trim().toLowerCase();
            s = s.trim();
            if (debugger.servHttpTraf) {
                logger.debug("rx " + a + ":'" + s + "'");
            }
            if (a.length() < 1) {
                break;
            }
            if (a.equals("connection")) {
                s = s.toLowerCase();
                gotKeep |= s.indexOf("keep") >= 0;
                gotKeep |= s.indexOf("te") >= 0;
                continue;
            }
            if (a.equals("depth")) {
                gotDepth |= s.indexOf("1") >= 0;
                continue;
            }
            if (a.equals("accept-encoding")) {
                if (s.indexOf("deflate") >= 0) {
                    gotCompr = 1;
                }
                if (s.indexOf("gzip") >= 0) {
                    gotCompr = 2;
                }
                continue;
            }
            if (a.equals("content-length")) {
                gotSize = bits.str2num(s);
                continue;
            }
            if (a.equals("content-type")) {
                gotType = s;
                continue;
            }
            if (a.equals("destination")) {
                gotDstntn = s;
                continue;
            }
            if (a.equals("range")) {
                gotRange = s;
                continue;
            }
            if (a.equals("x-forwarded-for")) {
                i = s.indexOf(",");
                if (i >= 0) {
                    s = s.substring(0, i);
                }
                addrIP adr = new addrIP();
                if (adr.fromString(s)) {
                    continue;
                }
                peer.setAddr(adr);
                continue;
            }
            if (a.equals("user-agent")) {
                gotAgent = s;
                continue;
            }
            if (a.equals("expect")) {
                gotExpect = s;
                continue;
            }
            if (a.equals("upgrade")) {
                gotUpgrade = s;
                continue;
            }
            if (a.equals("sec-websocket-key")) {
                gotWebsock = s;
                continue;
            }
            if (a.equals("referer")) {
                gotReferer = s;
                continue;
            }
            if (a.equals("authorization")) {
                gotAuth = s;
                continue;
            }
            if (a.equals("cookie")) {
                for (;;) {
                    s = s.trim();
                    i = s.indexOf(";");
                    if (i < 0) {
                        break;
                    }
                    gotCook.add(s.substring(0, i).trim());
                    s = s.substring(i + 1, s.length()).trim();
                }
                if (s.length() > 0) {
                    gotCook.add(s);
                }
                continue;
            }
            if (a.equals("host")) {
                encUrl srv = encUrl.parseOne("http://" + s + "/");
                gotUrl.server = srv.server;
                if (gotUrl.port < 0) {
                    gotUrl.port = srv.port;
                }
                if (gotUrl.proto.length() < 1) {
                    gotUrl.proto = srv.proto;
                }
                continue;
            }
        }
        if (gotExpect != null) {
            sendRespHeader(gotExpect.replaceAll("-", " "), 0, "text/plain");
        }
        if (gotSize > 0) {
            gotBytes = new byte[gotSize];
            if (pipe.moreGet(gotBytes, 0, gotBytes.length) < gotSize) {
                return true;
            }
            if (debugger.servHttpTraf) {
                logger.debug("readed " + gotBytes.length + " bytes");
                if (debugger.clntHttpTraf) {
                    logger.debug(bits.byteDump(gotBytes, 0, -1));
                }
            }
        } else {
            gotBytes = new byte[0];
        }
        if (gotType.equals("application/x-www-form-urlencoded")) {
            String s = new String(gotBytes);
            gotBytes = new byte[0];
            encUrl srv = encUrl.parseOne("http://x/y?" + s);
            gotUrl.param.addAll(srv.param);
        }
        gotUrl.normalizePath();
        if (gotUpgrade == null) {
            return false;
        }
        if (gotUpgrade.toLowerCase().startsWith("h2")) {
            headers.add("Upgrade: " + gotUpgrade);
            headers.add("Connection: Upgrade");
            sendRespHeader("101 switch protocol", -1, null);
            secHttp2 ht2 = new secHttp2(pipe, new pipeLine(lower.bufSiz, false));
            if (ht2.startServer(false)) {
                return true;
            }
            pipe = ht2.getPipe();
            return false;
        }
        if (!gotUpgrade.toLowerCase().startsWith("tls/")) {
            return false;
        }
        if (lower.noneSecKeys()) {
            return false;
        }
        headers.add("Upgrade: " + gotUpgrade + ", HTTP/1.1");
        sendRespHeader("101 switch protocol", -1, null);
        headers.clear();
        pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(lower.bufSiz, false), null);
        if (res == null) {
            return true;
        }
        res.lineRx = pipeSide.modTyp.modeCRtryLF;
        res.lineTx = pipeSide.modTyp.modeCRLF;
        pipe = res;
        secured = true;
        return false;
    }

    public void run() {
        if (secured) {
            pipeSide res = null;
            try {
                res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(lower.bufSiz, false), null);
            } catch (Exception e) {
                logger.traceback(e);
                clsPip();
                return;
            }
            if (res == null) {
                clsPip();
                return;
            }
            res.lineRx = pipeSide.modTyp.modeCRtryLF;
            res.lineTx = pipeSide.modTyp.modeCRLF;
            pipe = res;
        }
        for (;;) {
            try {
                if (readRequest()) {
                    break;
                }
            } catch (Exception e) {
                logger.traceback(e);
                break;
            }
            if (debugger.servHttpTraf) {
                logger.debug("peer=" + peer + " cmd=" + gotCmd + " ver=" + gotVer + " url=" + gotUrl.dump() + " keep=" + gotKeep + " " + bits.lst2str(gotCook, " "));
            }
            try {
                if (lower.findBot(gotAgent)) {
                    sendRespError(404, "never found");
                    break;
                }
                gotHost = lower.findHost(gotUrl.server);
                if (servHttpUtil.doConnect(this)) {
                    return;
                }
            } catch (Exception e) {
                logger.traceback(e);
                break;
            }
            if (debugger.servHttpTraf) {
                logger.debug("host=" + gotHost);
            }
            try {
                if (gotHost == null) {
                    sendRespError(404, "host not found");
                    break;
                }
                gotHost.serveRequest(this);
            } catch (Exception e) {
                logger.traceback(e, gotUrl.dump() + " " + gotHost + " " + peer);
                break;
            }
            if (!gotKeep) {
                break;
            }
            if (lower.singleRequest) {
                break;
            }
        }
        clsPip();
    }

}
