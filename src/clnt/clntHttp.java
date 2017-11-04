package clnt;

import cry.cryBase64;
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
     * get authorization line
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
            pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, url.server, url.getPort(new servHttp().srvPort()));
            return pipe == null;
        }
        userTerminal t = new userTerminal(cons);
        pipe = t.resolvAndConn(servGeneric.protoTcp, url.server, servHttp.securePort);
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
