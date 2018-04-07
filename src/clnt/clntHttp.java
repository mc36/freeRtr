package clnt;

import cry.cryBase64;
import cry.cryHashGeneric;
import cry.cryHashMd5;
import cry.cryHashSha1;
import java.io.File;
import java.io.RandomAccessFile;
import pipe.pipeDiscard;
import pipe.pipeProgress;
import pipe.pipeSide;
import serv.servGeneric;
import serv.servHttp;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;
import util.uniResLoc;
import util.version;

/**
 * hypertext transfer protocol (rfc2616) client
 *
 * @author matecsaba
 */
public class clntHttp {

    private pipeSide pipe;

    private pipeProgress cons;

    private RandomAccessFile fr;

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
        String s = cryBase64.encodeString(usr + ":" + pwd);
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
            System.out.println(a + "|" + s);
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
     */
    public clntHttp(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
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

    private void sendLine(String s) {
        if (debugger.clntHttpTraf) {
            logger.debug("tx:" + s);
        }
        cons.debugTx(s);
        pipe.linePut(s);
    }

    private void sendAuth(uniResLoc url) {
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
        if (debugger.clntHttpTraf) {
            logger.debug("rx:" + s);
        }
        cons.debugRx(s);
        return s;
    }

    private boolean doConnect(uniResLoc url) {
        if (pipe != null) {
            pipe.setClose();
        }
        if (!url.proto.equals("https")) {
            pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, url.server, url.getPort(new servHttp().srvPort()), "http");
            return pipe == null;
        }
        userTerminal t = new userTerminal(cons);
        pipe = t.resolvAndConn(servGeneric.protoTcp, url.server, url.getPort(servHttp.securePort), "https");
        if (pipe == null) {
            return true;
        }
        pipe = t.startSecurity(servGeneric.protoTls, null, null);
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        return false;
    }

    private boolean doDown(uniResLoc src, long pos) {
        try {
            fr.seek(pos);
        } catch (Exception e) {
            return true;
        }
        if (doConnect(src)) {
            return true;
        }
        sendLine("GET " + src.toURL(false, true) + " HTTP/1.1");
        sendLine("User-Agent: " + version.usrAgnt);
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
        int ctl = -1;
        for (;;) {
            String s = getLine();
            if (s.length() < 1) {
                break;
            }
            s = s.trim();
            String a = "";
            int i = s.indexOf(":");
            if (i > 0) {
                a = s.substring(0, i).trim().toLowerCase();
                s = s.substring(i + 1, s.length()).trim();
            }
            if (a.equals("content-length")) {
                ctl = bits.str2num(s);
                cons.setMax(ctl);
            }
        }
        cons.debugStat("receiving " + cons.getMax() + " bytes");
        pos = 0;
        for (;;) {
            final int max = 8192;
            byte buf[] = new byte[max];
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
        cons.debugRes(pos + " bytes done");
        if (ctl < 0) {
            return false;
        }
        return pos < ctl;
    }

    /**
     * download one file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean download(uniResLoc src, File trg) {
        try {
            trg.createNewFile();
            fr = new RandomAccessFile(trg, "rw");
            fr.setLength(0);
        } catch (Exception e) {
            return true;
        }
        int ret = 0;
        long pos = 0;
        long ops = -1;
        for (;;) {
            ret++;
            if (ret > 5) {
                return true;
            }
            try {
                pos = fr.length();
            } catch (Exception e) {
                return true;
            }
            if (pos != ops) {
                ret = 0;
            }
            ops = pos;
            if (!doDown(src, pos)) {
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
    public boolean upload(uniResLoc trg, File src) {
        if (doConnect(trg)) {
            return true;
        }
        long pos = 0;
        long siz = 0;
        try {
            fr = new RandomAccessFile(src, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        sendLine("PUT " + trg.toURL(false, true) + " HTTP/1.1");
        sendLine("User-Agent: " + version.usrAgnt);
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
            byte buf[] = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            if (pipe.morePut(buf, 0, rndi) < rndi) {
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
        return false;
    }

}
