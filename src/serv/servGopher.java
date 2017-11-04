package serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userFlash;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;
import util.version;

/**
 * gopher (rfc1436) server
 *
 * @author matecsaba
 */
public class servGopher extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int portNum = 70;

    /**
     * root folder
     */
    public String rootFolder = "/";

    /**
     * name of server
     */
    public String serverName = "";

    /**
     * directory listing allowed
     */
    public boolean dirListing = false;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server gopher .*! port " + portNum,
        "server gopher .*! protocol " + proto2string(protoAllStrm),
        "server gopher .*! no dirlist"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servGopherConn(this, pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        cmds.cfgLine(l, !dirListing, beg, "dirlist", "");
        l.add(beg + "path " + rootFolder);
        l.add(beg + "server " + serverName);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("server")) {
            serverName = cmd.word();
            return false;
        }
        if (s.equals("dirlist")) {
            dirListing = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/";
            return false;
        }
        if (s.equals("server")) {
            serverName = "";
            return false;
        }
        if (s.equals("dirlist")) {
            dirListing = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  path                         set root folder");
        l.add("2 .    <path>                     name of root folder");
        l.add("1 2  server                       set domain name");
        l.add("2 .    <name>                     domain name of server");
        l.add("1 .  dirlist                      allow directory listing");
    }

    public String srvName() {
        return "gopher";
    }

    public int srvPort() {
        return portNum;
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

}

class servGopherConn implements Runnable {

    private servGopher lower;

    private pipeSide pipe;

    public servGopherConn(servGopher parent, pipeSide stream) {
        lower = parent;
        pipe = stream;
        new Thread(this).start();
    }

    public void sendSeparator() {
        byte[] buf = new byte[1];
        buf[0] = 9;
        pipe.blockingPut(buf, 0, buf.length);
    }

    public void sendLine(String desc, String path, String serv, int port) {
        pipe.strPut(desc);
        sendSeparator();
        pipe.strPut(path);
        sendSeparator();
        pipe.strPut(serv);
        sendSeparator();
        pipe.linePut("" + port);
    }

    private boolean sendOneFile(String s) {
        RandomAccessFile fr;
        long siz;
        try {
            fr = new RandomAccessFile(s, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        for (long pos = 0; pos < siz;) {
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
                return false;
            }
            if (pipe.morePut(buf, 0, rndi) < rndi) {
                try {
                    fr.close();
                } catch (Exception e) {
                }
                return false;
            }
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
        return false;
    }

    private boolean sendOneDir(String s, String p) {
        if (s.lastIndexOf("/") + 1 < s.length()) {
            s += "/";
            p += "/";
        }
        if (!sendOneFile(s + "index.gopher")) {
            return false;
        }
        if (!lower.dirListing) {
            sendLine("inot found", "", "", 0);
            return true;
        }
        File[] fl = userFlash.dirList(s);
        if (fl == null) {
            return true;
        }
        sendLine("idirectory listing of " + p, "", "", 0);
        sendLine("1/", "/", lower.serverName, lower.srvPort);
        sendLine("1..", p + "../", lower.serverName, lower.srvPort);
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
            String t = "9";
            if (f.isDirectory()) {
                a += "/";
                t = "1";
            }
            sendLine(t + a, p + a, lower.serverName, lower.srvPort);
        }
        sendLine("igenerated by " + version.namVer, "", "", 0);
        return false;
    }

    public void run() {
        try {
            String s = pipe.lineGet(1);
            s = uniResLoc.normalizePath(s);
            if (debugger.servGopherTraf) {
                logger.debug("req " + s);
            }
            uniResLoc u = uniResLoc.parseOne(s);
            String p = lower.rootFolder + s;
            boolean b = true;
            if (u.toFileName().length() > 0) {
                b = sendOneFile(p);
            } else {
                b = sendOneDir(p, s);
            }
            if (b) {
                sendLine("inot found", "", "", 0);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
