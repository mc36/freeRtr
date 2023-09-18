package net.freertr.serv;

import java.io.File;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgProxy;
import net.freertr.enc.encXml;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.sec.secWebsock;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * http utilities
 *
 * @author matecsaba
 */
public class servHttpUtil {

    private servHttpUtil() {
    }

    /**
     * allow nothing
     */
    public final static int apiBitsNothing = 0;

    /**
     * allow something
     */
    public final static int apiBitsSomething = 0x01;

    /**
     * allow exec commands
     */
    public final static int apiBitsExec = 0x02;

    /**
     * allow config commands
     */
    public final static int apiBitsConfig = 0x04;

    /**
     * allow ip info commands
     */
    public final static int apiBitsIpinfo = 0x08;

    protected final static void dumpXml(String s) {
        if (!debugger.servHttpXml) {
            return;
        }
        dumpXml(encXml.parseOne(s.replaceAll("\r", "").replaceAll("\n", "")));
    }

    protected final static void dumpXml(encXml xml) {
        if (!debugger.servHttpXml) {
            return;
        }
        List<String> l = xml.show();
        for (int i = 0; i < l.size(); i++) {
            logger.debug("xml " + l.get(i));
        }
    }

    /**
     * convert string to api bits
     *
     * @param cmd commands
     * @return api bits
     */
    public final static int string2apiBits(cmds cmd) {
        int i = apiBitsNothing;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("exec")) {
                i |= apiBitsExec;
                continue;
            }
            if (a.equals("config")) {
                i |= 4;
                continue;
            }
            if (a.equals("ipinfo")) {
                i |= 8;
                continue;
            }
        }
        if (i == apiBitsSomething) {
            return apiBitsNothing;
        }
        return i | apiBitsSomething;
    }

    /**
     * convert api bits to string
     *
     * @param i bits to convert
     * @return api bits
     */
    public final static String apiBits2string(int i) {
        if ((i & apiBitsSomething) == 0) {
            return "bug=" + i;
        }
        String s = "";
        if ((i & apiBitsExec) != 0) {
            s += " exec";
        }
        if ((i & apiBitsConfig) != 0) {
            s += " config";
        }
        if ((i & apiBitsIpinfo) != 0) {
            s += " ipinfo";
        }
        return s;
    }

    /**
     * subconnect to string
     *
     * @param subconn subconnect mode
     * @return config string
     */
    protected final static String subconn2string(int subconn) {
        String s = "";
        if ((subconn & 0x1) != 0) {
            s += " strip-path";
        }
        if ((subconn & 0x2) != 0) {
            s += " strip-name";
        }
        if ((subconn & 0x4) != 0) {
            s += " strip-ext";
        }
        if ((subconn & 0x8) != 0) {
            s += " strip-param";
        }
        if ((subconn & 0x10) != 0) {
            s += " keep-cred";
        }
        if ((subconn & 0x20) != 0) {
            s += " keep-host";
        }
        if ((subconn & 0x40) != 0) {
            s += " keep-path";
        }
        return s;
    }

    protected final static boolean checkNoHeaders(String s) {
        return new File(s + ".noheaders").exists();
    }

    /**
     * read up subconnect modes
     *
     * @param neg negated
     * @param cmd commands to read
     * @return subconnect mode
     */
    protected final static int string2subconn(boolean neg, cmds cmd) {
        if (neg) {
            return 0;
        }
        int res = 0;
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("strip-path")) {
                res |= 0x1;
                continue;
            }
            if (a.equals("strip-name")) {
                res |= 0x2;
                continue;
            }
            if (a.equals("strip-ext")) {
                res |= 0x4;
                continue;
            }
            if (a.equals("strip-param")) {
                res |= 0x8;
                continue;
            }
            if (a.equals("keep-cred")) {
                res |= 0x10;
                continue;
            }
            if (a.equals("keep-host")) {
                res |= 0x20;
                continue;
            }
            if (a.equals("keep-path")) {
                res |= 0x40;
                continue;
            }
        }
        return res;
    }

    private final static String semi2comma(String a) {
        return a.replaceAll(";", ",");
    }

    protected final static void updateVisitors(servHttpConn cn, String pn) {
        pn = cn.gotHost.path + pn + ".visitors";
        if (!new File(pn).exists()) {
            return;
        }
        String a = cn.peer + ";" + logger.getTimestamp() + ";" + semi2comma(cn.gotAgent) + ";" + semi2comma(cn.gotReferer) + "\n";
        bits.byteSave(false, a.getBytes(), pn);
    }

    protected final static boolean sendOneWebSck(servHttpConn cn, String pn) {
        pn = cn.gotHost.path + pn + ".websock";
        if (!new File(pn).exists()) {
            return true;
        }
        List<String> l = bits.txt2buf(pn);
        if (l == null) {
            return true;
        }
        if (l.size() < 5) {
            return true;
        }
        cfgProxy prx = cfgAll.proxyFind(l.get(0), false);
        if (prx == null) {
            cn.sendRespError(502, "bad proxy profile");
            return false;
        }
        addrIP adr = userTerminal.justResolv(l.get(1), prx.proxy.prefer);
        if (adr == null) {
            cn.sendRespError(502, "bad target hostname");
            return false;
        }
        pipeSide pip = prx.proxy.doConnect(servGeneric.protoTcp, adr, bits.str2num(l.get(2)), "websock");
        if (pip == null) {
            cn.sendRespError(502, "failed to connect");
            return false;
        }
        cn.sendLn("HTTP/1.1 101 switching protocol");
        cn.sendLn("Upgrade: websocket");
        cn.sendLn("Connection: Upgrade");
        cn.sendLn("Sec-WebSocket-Accept: " + secWebsock.calcHash(cn.gotWebsock));
        cn.sendLn("Sec-WebSocket-Protocol: " + l.get(3));
        cn.sendLn("");
        secWebsock wsk = new secWebsock(cn.pipe, new pipeLine(cn.lower.bufSiz, false));
        wsk.binary = l.get(4).equals("bin");
        wsk.startServer();
        pipeConnect.connect(pip, wsk.getPipe(), true);
        cn.gotKeep = false;
        cn.pipe = null;
        return false;
    }

    /**
     * get style of host
     *
     * @return style
     */
    protected final static String getStyle(servHttpConn cn) {
        if (cn.gotHost == null) {
            return "";
        }
        if (cn.gotHost.style == null) {
            return "";
        }
        String s = "<style>\n";
        for (int o = 0; o < cn.gotHost.style.size(); o++) {
            String a = cn.gotHost.style.get(o);
            if (!a.startsWith("@import ")) {
                s += " " + a + "\n";
                continue;
            }
            List<String> l = bits.txt2buf(cn.gotHost.path + a.substring(8, a.length()));
            if (l == null) {
                continue;
            }
            for (int i = 0; i < l.size(); i++) {
                s += " " + l.get(i) + "\n";
            }
        }
        return s + "</style>\n";
    }

}
