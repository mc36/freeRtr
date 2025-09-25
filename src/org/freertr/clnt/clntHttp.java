package org.freertr.clnt;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.encBase64;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashMd5;
import org.freertr.cry.cryHashSha1;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.sec.secClient;
import org.freertr.sec.secHttp2;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.enc.encXmlEntry;
import org.freertr.enc.encUrl;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * hypertext transfer protocol (rfc2616) client
 *
 * @author matecsaba
 */
public class clntHttp {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    /**
     * pipeline in use
     */
    public pipeSide pipe;

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * client pubkey
     */
    public byte[] pubkey;

    private pipeProgress cons;

    private RandomAccessFile fr;

    private boolean debug;

    /**
     * content length
     */
    public long cntLen;

    /**
     * chunked
     */
    public boolean chnkd;

    /**
     * keepalive
     */
    public boolean kepAliv;

    /**
     * cookies
     */
    public List<encXmlEntry> cookies;

    /**
     * cookies
     */
    public List<encXmlEntry> headers;

    /**
     * get user agent string
     *
     * @return user agent
     */
    public static String getAgentLine() {
        if (cfgAll.httpAgent != null) {
            return cfgAll.httpAgent;
        } else {
            return cfgInit.versionAgent + " (" + cfgInit.getKernelName() + ", " + cfgInit.getVMname() + ", " + cfgInit.getHWfwd1liner() + ")";
        }
    }

    /**
     * get basic authorization line
     *
     * @param usr username
     * @param pwd password
     * @return authorization header, null if not needed
     */
    public static String getAuthor(String usr, String pwd) {
        if (usr == null) {
            return null;
        }
        if (pwd == null) {
            return null;
        }
        if ((usr.length() + pwd.length()) < 1) {
            return null;
        }
        String s = encBase64.encodeString(usr + ":" + pwd);
        return "Authorization: Basic " + s;
    }

    /**
     * get digest authorization line
     *
     * @param req request
     * @param met method
     * @param url url
     * @param usr username
     * @param pwd password
     * @return authorization header, null if not needed
     */
    public static String getAuthor(String req, String met, String url, String usr, String pwd) {
        if (usr == null) {
            return null;
        }
        if (pwd == null) {
            return null;
        }
        if ((usr.length() + pwd.length()) < 1) {
            return null;
        }
        String realm = null;
        String nonce = null;
        String opaque = null;
        String algo = "MD5";
        String qop = null;
        for (;;) {
            if (req.length() < 1) {
                break;
            }
            int i = req.indexOf("=");
            if (i < 0) {
                break;
            }
            String a = req.substring(0, i).trim();
            req = req.substring(i + 1, req.length()).trim();
            String s;
            if (req.startsWith("\"")) {
                req = req.substring(1, req.length());
                i = req.indexOf("\"");
                s = req.substring(0, i);
                req = req.substring(i + 1, req.length()).trim();
                if (req.startsWith(",")) {
                    req = req.substring(1, req.length()).trim();
                }
            } else {
                i = req.indexOf(",");
                if (i < 0) {
                    s = req;
                    req = "";
                } else {
                    s = req.substring(0, i);
                    req = req.substring(i + 1, req.length()).trim();
                }
            }
            a = a.trim().toLowerCase();
            if (a.equals("realm")) {
                realm = s;
                continue;
            }
            if (a.equals("nonce")) {
                nonce = s;
                continue;
            }
            if (a.equals("opaque")) {
                opaque = s;
                continue;
            }
            if (a.equals("algorithm")) {
                algo = s;
                continue;
            }
            if (a.equals("qop")) {
                qop = s;
                continue;
            }
        }
        if (realm == null) {
            return null;
        }
        if (nonce == null) {
            return null;
        }
        algo = algo.trim().toLowerCase();
        cryHashGeneric h = null;
        if (algo.startsWith("md5")) {
            h = new cryHashMd5();
        }
        if (algo.startsWith("sha1")) {
            h = new cryHashSha1();
        }
        if (h == null) {
            return null;
        }
        String cnonce = "" + bits.randomD();
        String nc = "00000001";
        h.init();
        h.update(usr.getBytes());
        h.update(":".getBytes());
        h.update(realm.getBytes());
        h.update(":".getBytes());
        h.update(pwd.getBytes());
        if (algo.endsWith("-sess")) {
            h.update(":".getBytes());
            h.update(nonce.getBytes());
            h.update(":".getBytes());
            h.update(cnonce.getBytes());
        }
        byte[] a1 = h.finish();
        h.init();
        h.update(met.getBytes());
        h.update(":".getBytes());
        h.update(url.getBytes());
        byte[] a2 = h.finish();
        h.init();
        h.update(bits.toHex(a1).getBytes());
        h.update(":".getBytes());
        h.update(nonce.getBytes());
        h.update(":".getBytes());
        if (qop != null) {
            h.update(nc.getBytes());
            h.update(":".getBytes());
            h.update(cnonce.getBytes());
            h.update(":".getBytes());
            h.update(qop.getBytes());
            h.update(":".getBytes());
        }
        h.update(bits.toHex(a2).getBytes());
        String s = bits.toHex(h.finish());
        s = "Authorization: Digest username=\"" + usr + "\",realm=\"" + realm + "\",nonce=\"" + nonce + "\",uri=\"" + url + "\",algorithm=" + h.getName() + ",response=\"" + s + "\"";
        if (opaque != null) {
            s += ",opaque=\"" + opaque + "\"";
        }
        if (qop == null) {
            return s;
        }
        return s + ",qop=" + qop + ",nc=" + nc + ",cnonce=\"" + cnonce + "\"";
    }

    /**
     * create new client
     *
     * @param console console to use
     * @param vrfPrx proxy
     * @param pubKey pubkey to use
     * @param debugging debug enabled
     */
    public clntHttp(pipeSide console, clntProxy vrfPrx, byte[] pubKey, boolean debugging) {
        proxy = vrfPrx;
        pubkey = pubKey;
        cons = new pipeProgress(pipeDiscard.needAny(console));
        debug = debugger.clntHttpTraf | debugging;
    }

    /**
     * clean up state
     */
    public void cleanUp() {
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
    }

    private void doDebug(String s) {
        if (!debug) {
            return;
        }
        logger.debug(s);
        if (cons == null) {
            return;
        }
        cons.debugStat(s);
    }

    /**
     * send line
     *
     * @param s line
     */
    public void sendLine(String s) {
        doDebug("tx:" + s);
        cons.debugTx(s);
        pipe.linePut(s);
    }

    private void sendAuth(encUrl url) {
        String s = getAuthor(url.username, url.password);
        if (s == null) {
            return;
        }
        sendLine(s);
    }

    private String getLine() {
        String s = pipe.lineGet(1);
        if (s == null) {
            s = "";
        }
        doDebug("rx:" + s);
        cons.debugRx(s);
        return s;
    }

    /**
     * create new connection
     *
     * @param url url to connect
     * @return false on success, true on error
     */
    public boolean doConnect(encUrl url) {
        if (pipe != null) {
            pipe.setClose();
        }
        if (proxy == null) {
            return true;
        }
        doDebug("resolving " + url.toURL(true, false, false, true));
        addrIP trg = clntDns.justResolv(url.server, proxy.prefer);
        if (trg == null) {
            return true;
        }
        doDebug("connecting " + trg + " " + url.getPort(0));
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, url.getPort(0), "http");
        if (pipe == null) {
            return true;
        }
        pipe.settingsAdd(pipeSetting.origin, url.server);
        doDebug("securing " + url.toURL(true, false, false, true));
        pipe = secClient.openSec(pipe, url.getSecurity(), pubkey, url.username, url.password);
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        if (!url.proto.equals("http2")) {
            return false;
        }
        url.proto = "http";
        secHttp2 ht2 = new secHttp2(pipe, new pipeLine(65536, false));
        if (ht2.startClient()) {
            return true;
        }
        pipe = ht2.getPipe();
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        return false;
    }

    /**
     * receive headers
     *
     * @param src url to connect
     * @return false on proceed, true on location
     */
    public boolean doHeaders(encUrl src) {
        cntLen = -1;
        chnkd = false;
        kepAliv = false;
        cookies = new ArrayList<encXmlEntry>();
        headers = new ArrayList<encXmlEntry>();
        String loc = null;
        String res = getLine();
        int i = res.indexOf(" ");
        if (i >= 0) {
            res = res.substring(i, res.length()).trim();
        }
        i = res.indexOf(" ");
        if (i >= 0) {
            res = res.substring(0, i).trim();
        }
        for (;;) {
            String s = getLine();
            if (s.length() < 1) {
                break;
            }
            s = s.trim();
            String a = "";
            i = s.indexOf(":");
            if (i > 0) {
                a = s.substring(0, i).trim().toLowerCase();
                s = s.substring(i + 1, s.length()).trim();
            }
            if (a.equals("location")) {
                loc = s;
                continue;
            }
            if (a.equals("connection")) {
                kepAliv = s.equals("keep-alive");
                continue;
            }
            if (a.equals("content-length")) {
                cntLen = bits.str2long(s);
                cons.setMax(cntLen);
                continue;
            }
            if (a.equals("transfer-encoding")) {
                chnkd = s.equals("chunked");
                continue;
            }
            if (a.equals("set-cookie")) {
                i = s.indexOf(";");
                if (i < 0) {
                    continue;
                }
                String p = s.substring(i + 1, s.length()).trim();
                s = s.substring(0, i).trim();
                i = s.indexOf("=");
                if (i < 0) {
                    continue;
                }
                a = s.substring(0, i).trim();
                s = s.substring(i + 1, s.length()).trim();
                cookies.add(new encXmlEntry(null, a, p, s));
            }
            headers.add(new encXmlEntry(null, a, null, s));
        }
        if (loc == null) {
            return false;
        }
        if (loc.startsWith("/")) {
            src.fromPathname(loc);
        } else {
            src.fromString(loc);
        }
        return true;
    }

    private static void bytes2array(List<Byte> res, byte[] buf, int siz) {
        for (int i = 0; i < siz; i++) {
            res.add(buf[i]);
        }
    }

    /**
     * receive body
     *
     * @return bytes read, null on error
     */
    public List<Byte> doBody() {
        List<Byte> res = new ArrayList<Byte>();
        if (chnkd) {
            for (;;) {
                byte[] buf = getChunk();
                if (buf == null) {
                    break;
                }
                if (buf.length < 1) {
                    break;
                }
                bytes2array(res, buf, buf.length);
            }
            return res;
        }
        if (cntLen >= 0) {
            byte[] buf = new byte[(int) cntLen];
            if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                return null;
            }
            bytes2array(res, buf, buf.length);
            return res;
        }
        for (;;) {
            final int max = 8192;
            byte[] buf = new byte[max];
            int siz = pipe.moreGet(buf, 0, max);
            if (siz < 1) {
                break;
            }
            bytes2array(res, buf, siz);
        }
        return res;
    }

    private byte[] getChunk() {
        String s = "0";
        for (;;) {
            if (pipe.ready2rx() < 1) {
                if (pipe.isClosed() != 0) {
                    break;
                }
            }
            s = getLine();
            break;
        }
        int i = bits.fromHex(s);
        if (i < 1) {
            return new byte[0];
        }
        byte[] buf = new byte[i];
        if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        return buf;
    }

    private boolean doDown(encUrl src, long pos) {
        try {
            fr.seek(pos);
        } catch (Exception e) {
            return true;
        }
        if (doConnect(src)) {
            return true;
        }
        sendLine("GET " + src.toURL(false, false, true, true) + " HTTP/1.1");
        sendLine("User-Agent: " + getAgentLine());
        sendLine("Host: " + src.server);
        sendLine("Accept: */*");
        sendLine("Accept-Language: en,*");
        sendLine("Accept-Charset: iso-8859-1, *");
        sendLine("Accept-Encoding: identity");
        if (pos > 0) {
            sendLine("Range: bytes=" + pos + "-");
        }
        sendLine("Connection: Close");
        sendAuth(src);
        sendLine("");
        if (doHeaders(src)) {
            return true;
        }
        pos = 0;
        cons.debugStat("receiving " + cons.getMax() + " bytes");
        if (chnkd) {
            for (;;) {
                byte[] buf = getChunk();
                if (buf == null) {
                    return true;
                }
                if (buf.length < 1) {
                    break;
                }
                pos += buf.length;
                cons.setCurr(pos);
                try {
                    fr.write(buf, 0, buf.length);
                } catch (Exception ex) {
                    return true;
                }
            }
        } else {
            for (;;) {
                final int max = 8192;
                byte[] buf = new byte[max];
                int siz = pipe.moreGet(buf, 0, max);
                if (siz < 1) {
                    break;
                }
                pos += siz;
                cons.setCurr(pos);
                try {
                    fr.write(buf, 0, siz);
                } catch (Exception ex) {
                    return true;
                }
            }
        }
        cons.debugRes(pos + " bytes done");
        if (cntLen < 0) {
            return false;
        }
        return pos < cntLen;
    }

    /**
     * download one file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean download(encUrl src, File trg) {
        cntrStart.add(1);
        try {
            trg.createNewFile();
            fr = new RandomAccessFile(trg, "rw");
            fr.setLength(0);
        } catch (Exception e) {
            cntrError.add(1);
            return true;
        }
        int ret = 0;
        long pos = 0;
        long ops = -1;
        for (;;) {
            ret++;
            if (ret > 5) {
                cntrError.add(1);
                return true;
            }
            try {
                pos = fr.length();
            } catch (Exception e) {
                cntrError.add(1);
                return true;
            }
            if (pos != ops) {
                ret = 0;
            }
            ops = pos;
            if (!doDown(src, pos)) {
                cntrStop.add(1);
                return false;
            }
        }
    }

    /**
     * upload one file
     *
     * @param trg source
     * @param src target
     * @return result code
     */
    public boolean upload(encUrl trg, File src) {
        cntrStart.add(1);
        if (doConnect(trg)) {
            cntrError.add(1);
            return true;
        }
        long pos = 0;
        long siz = 0;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            cntrError.add(1);
            return true;
        }
        sendLine("PUT " + trg.toURL(false, false, true, true) + " HTTP/1.1");
        sendLine("User-Agent: " + getAgentLine());
        sendLine("Host: " + trg.server);
        sendLine("Accept: */*");
        sendLine("Accept-Language: en,*");
        sendLine("Accept-Charset: iso-8859-1, *");
        sendLine("Accept-Encoding: identity");
        sendLine("Connection: Close");
        sendLine("Content-Length: " + siz);
        sendLine("Content-Type: application/octet-stream");
        sendAuth(trg);
        sendLine("");
        cons.setMax(siz);
        cons.debugStat("sending " + cons.getMax() + " bytes");
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte[] buf = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                cntrError.add(1);
                return true;
            }
            if (pipe.morePut(buf, 0, rndi) < rndi) {
                cntrError.add(1);
                return true;
            }
            cons.setCurr(pos);
        }
        for (;;) {
            String s = getLine();
            if (s.length() < 1) {
                break;
            }
        }
        pipe.setClose();
        cons.debugRes(pos + " bytes done");
        cntrStop.add(1);
        return false;
    }

}
