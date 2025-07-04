package org.freertr.serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.enc.encUrl;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFlash;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * file transfer protocol (rfc959) server
 *
 * @author matecsaba
 */
public class servFtp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servFtp() {
    }

    /**
     * port number
     */
    public final static int portNum = 21;

    /**
     * root folder
     */
    public String rootFolder = "/data/";

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * read only server
     */
    public boolean readOnly = true;

    /**
     * passive only server
     */
    public boolean passiveOnly = true;

    /**
     * list of connections
     */
    public tabGen<servFtpConn> conns = new tabGen<servFtpConn>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server ftp .*", cmds.tabulator + "port " + portNum, null),
        new userFilter("server ftp .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server ftp .*", cmds.tabulator + "readonly", null),
        new userFilter("server ftp .*", cmds.tabulator + "passiveonly", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * remove one connection
     *
     * @param ntry connection to remove
     * @return false on success, true on error
     */
    protected boolean removeConn(servFtpConn ntry) {
        if (ntry == null) {
            return true;
        }
        return conns.del(ntry) == null;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        servFtpConn ntry = new servFtpConn(id);
        servFtpConn old = conns.add(ntry);
        if (old != null) {
            ntry = old;
        }
        return ntry.adder(this, id, pipe);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (authenticList == null) {
            l.add(beg + "no authentication");
        } else {
            l.add(beg + "authentication " + authenticList.autName);
        }
        cmds.cfgLine(l, !readOnly, beg, "readonly", "");
        cmds.cfgLine(l, !passiveOnly, beg, "passiveonly", "");
        l.add(beg + "path " + rootFolder);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("authentication")) {
            cfgAuther lst = cfgAll.autherFind(cmd.word(), null);
            if (lst == null) {
                cmd.error("no such auth list");
                return false;
            }
            authenticList = lst.getAuther();
            return false;
        }
        if (s.equals("path")) {
            rootFolder = "/" + encUrl.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = true;
            return false;
        }
        if (s.equals("passiveonly")) {
            passiveOnly = true;
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("authentication")) {
            authenticList = null;
            return false;
        }
        if (s.equals("path")) {
            rootFolder = "/data/";
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = false;
            return false;
        }
        if (s.equals("passiveonly")) {
            passiveOnly = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set authentication");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of authentication list");
        l.add(null, false, 1, new int[]{2}, "path", "set root folder");
        l.add(null, false, 2, new int[]{-1}, "<path>", "name of root folder");
        l.add(null, false, 1, new int[]{-1}, "readonly", "set write protection");
        l.add(null, false, 1, new int[]{-1}, "passiveonly", "set connection protection");
    }

    public String srvName() {
        return "ftp";
    }

    public int srvPort() {
        return portNum;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        if (genStrmStart(this, new pipeLine(32768, false), srvPort - 1)) {
            return true;
        }
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        if (genericStop(srvPort - 1)) {
            return true;
        }
        return genericStop(0);
    }

}

class servFtpConn implements Comparable<servFtpConn> {

    public addrIP addr;

    public prtGenConn connCmnd;

    public prtGenConn connData;

    public pipeSide pipeCmnd;

    public pipeSide pipeData;

    public servFtpDoer doer;

    public int compareTo(servFtpConn o) {
        return addr.compareTo(o.addr);
    }

    public servFtpConn(prtGenConn id) {
        addr = id.peerAddr;
    }

    public boolean adder(servFtp parent, prtGenConn id, pipeSide stream) {
        int i = 0;
        if ((connData == null) && (doer != null)) {
            i = 2;
        }
        if ((connCmnd == null) && (id.portLoc == parent.srvPort)) {
            i = 1;
        }
        if (stream == null) {
            i = 0;
        }
        switch (i) {
            case 1:
                connCmnd = id;
                pipeCmnd = stream;
                doer = new servFtpDoer(parent, stream, this);
                break;
            case 2:
                connData = id;
                pipeData = stream;
                doer.addData(stream);
                break;
            default:
                id.setClosing();
                stream.setClose();
                return true;
        }
        return false;
    }

    public void closer(int side) {
        if (((side & 0x1) != 0) && (pipeCmnd != null)) {
            pipeCmnd.setClose();
        }
        if (((side & 0x2) != 0) && (pipeData != null)) {
            pipeData.setClose();
            pipeData = null;
            doer.addData(null);
        }
        if (((side & 0x10) != 0) && (connCmnd != null)) {
            connCmnd.setClosing();
        }
        if (((side & 0x20) != 0) && (connData != null)) {
            connData.setClosing();
            connData = null;
        }
    }

    public String toString() {
        return "cmd=" + pipeCmnd + " data=" + pipeData + " cmd=" + connCmnd + " data=" + connData;
    }

}

class servFtpDoer implements Runnable {

    private servFtp lower;

    private pipeSide pipe;

    private pipeSide data = null;

    private servFtpConn conn;

    private String userN = "";

    private boolean authed = false;

    private String path = "/";

    private String renameFrom = "";

    private long restartFrom = 0;

    public servFtpDoer(servFtp parent, pipeSide stream, servFtpConn id) {
        lower = parent;
        pipe = stream;
        conn = id;
        new Thread(this).start();
    }

    public String getRelPath(String s, boolean real) {
        if (s.startsWith("/")) {
            s = "/" + encUrl.normalizePath(s);
        } else {
            s = "/" + encUrl.normalizePath(path + s);
        }
        if (real) {
            s = lower.rootFolder + s.substring(1, s.length());
        }
        return s;
    }

    public String dumpConn(String s, pipeSide p, prtGenConn c) {
        if (c == null) {
            if (p != null) {
                return s + " pipeline opened";
            }
            return "no " + s + " connection";
        }
        return s + " connected to " + c.peerAddr + " " + c.portRem;
    }

    public void addData(pipeSide stream) {
        if (data != null) {
            data.setClose();
        }
        data = stream;
    }

    public boolean wait4data() {
        for (int i = 0; i < 32; i++) {
            if (data == null) {
                bits.sleep(1000);
                continue;
            }
            if (data.wait4ready(120000)) {
                data.setClose();
                continue;
            }
            if (data.isClosed() != 0) {
                data = null;
                continue;
            }
            return false;
        }
        return true;
    }

    public void sendResult(boolean b) {
        if (!b) {
            doLine("226 successful");
        } else {
            doLine("550 error sending file");
        }
    }

    public boolean sendOneFile(File f, long pos) {
        if (!f.exists()) {
            doLine("550 not exists");
            return false;
        }
        if (!f.isFile()) {
            doLine("550 not file");
            return false;
        }
        if (wait4data()) {
            doLine("550 data pipe error");
            return false;
        }
        doLine("150 data pipe ready, transferring");
        RandomAccessFile fr;
        try {
            fr = new RandomAccessFile(f, "r");
        } catch (Exception e) {
            return true;
        }
        long siz;
        try {
            fr.seek(pos);
            siz = f.length();
        } catch (Exception e) {
            siz = -1;
        }
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
                siz = -1;
                break;
            }
            if (data.morePut(buf, 0, rndi) < rndi) {
                try {
                    fr.close();
                } catch (Exception e) {
                }
                return true;
            }
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
        return siz < 0;
    }

    public boolean recvOneFile(File f, long pos) {
        if (!f.exists()) {
            doLine("550 not exists");
            return false;
        }
        if (!f.isFile()) {
            doLine("550 not file");
            return false;
        }
        if (wait4data()) {
            doLine("550 data pipe error");
            return false;
        }
        doLine("150 data pipe ready, transferring");
        RandomAccessFile fr;
        try {
            fr = new RandomAccessFile(f, "rw");
        } catch (Exception e) {
            return true;
        }
        boolean res = false;
        try {
            fr.setLength(pos);
            fr.seek(pos);
        } catch (Exception e) {
            res = true;
        }
        for (;;) {
            final int max = 8192;
            byte[] buf = new byte[max];
            int siz = data.moreGet(buf, 0, max);
            if (siz < 1) {
                break;
            }
            try {
                fr.write(buf, 0, siz);
            } catch (Exception ex) {
                res = true;
                break;
            }
        }
        try {
            fr.close();
        } catch (Exception e) {
        }
        return res;
    }

    public String getOldInfo(File f) {
        String a;
        if (f.isDirectory()) {
            a = "d";
        } else {
            a = "-";
        }
        a = a + "rwxrwxr-x 1 2 3 " + f.length();
        a = bits.padEnd(a, 55, " ") + " " + f.getName();
        return a;
    }

    public String getNameInfo(File f) {
        return f.getName();
    }

    public String getNewInfo(File f) {
        String a = "type=";
        if (f.isDirectory()) {
            a = a + "dir;";
        } else {
            a = a + "file;";
        }
        a = a + "size=" + f.length() + ";";
        a = a + "modify=" + f.lastModified();
        a = a + " " + f.getName();
        return a;
    }

    public void doLine(String s) {
        if (debugger.servFtpTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    public boolean doOne() {
        String s = pipe.lineGet(1).trim();
        if (debugger.servFtpTraf) {
            logger.debug("rx: " + s);
        }
        cmds cmd = new cmds("", s);
        String a = cmd.word().toLowerCase();
        if (a.length() < 1) {
            return pipe.isClosed() != 0;
        }
        if (a.equals("quit")) {
            doLine("221 goodbye");
            return true;
        }
        if (a.equals("noop")) {
            doLine("250 no operation done");
            return false;
        }
        if (a.equals("help")) {
            doLine("214-the following commands are recognized");
            doLine("214-user   port   type   dele   syst   rmd");
            doLine("214-pass   stru   allo   cwd    feat   cdup");
            doLine("214-stou   eprt   mode   rest   stat   pwd");
            doLine("214-size   pasv   retr   rnfr   list   help");
            doLine("214-mdtm   stor   rnto   nlst   noop   quit");
            doLine("214-epsv   appe   abor   mkd    mlst   mlsd");
            doLine("214 direct comments to ftp@" + cfgAll.hostName);
            return false;
        }
        if (a.equals("stat")) {
            doLine("211-" + cfgAll.hostName + " server status");
            doLine("211-" + dumpConn("command", conn.pipeCmnd, conn.connCmnd));
            doLine("211-" + dumpConn("data", conn.pipeData, conn.connData));
            doLine("211-logged in " + userN);
            doLine("211-TYPE: image, FORM: nonprint; STRUcture: file; transfer MODE: stream");
            doLine("211 end of status");
            return false;
        }
        if (a.equals("syst")) {
            doLine("215 UNIX type: L8");
            return false;
        }
        if (a.equals("auth")) {
            a = cmd.word().toLowerCase();
            if (!a.equals("tls")) {
                doLine("500 bad subcommand");
                return false;
            }
            if (lower.noneSecKeys()) {
                doLine("454 not allowed");
                return false;
            }
            doLine("234 do it");
            pipeSide res = lower.negoSecSess(pipe, servGeneric.protoTls, new pipeLine(65536, false), null);
            if (res == null) {
                return true;
            }
            res.lineRx = pipeSide.modTyp.modeCRtryLF;
            res.lineTx = pipeSide.modTyp.modeCRLF;
            pipe = res;
            return false;
        }
        if (a.equals("feat")) {
            doLine("211-extensions supported");
            if (!lower.noneSecKeys()) {
                doLine("211-auth tls");
            }
            doLine("211-size");
            doLine("211 end");
            return false;
        }
        if (a.equals("user")) {
            authed = false;
            userN = cmd.word();
            doLine("331 password please");
            return false;
        }
        if (a.equals("pass")) {
            authed = false;
            authResult res = lower.authenticList.authUserPass(userN, cmd.word());
            if (res == null) {
                doLine("530 error");
                return true;
            }
            if (res.result != authResult.authSuccessful) {
                doLine("530 failed");
                return true;
            }
            doLine("230 login successful");
            authed = true;
            return false;
        }
        if (!authed) {
            doLine("530 not logged in");
            return false;
        }
        if (a.equals("pwd")) {
            doLine("257 \"" + path + "\" is the current directory");
            return false;
        }
        if (a.equals("cdup")) {
            path = getRelPath("../", false);
            doLine("250 moved up one level");
            return false;
        }
        if (a.equals("cwd")) {
            a = getRelPath(cmd.getRemaining() + "/", false);
            File f = new File(getRelPath(a + "/", true));
            if (!f.isDirectory()) {
                doLine("550 no such file or directory");
                return false;
            }
            path = a;
            doLine("250 moved to folder");
            return false;
        }
        if (a.equals("type")) {
            s = "";
            a = cmd.word().toLowerCase();
            if (a.equals("a")) {
                s = "ascii";
            }
            if (a.equals("i")) {
                s = "image";
            }
            if (s.length() < 1) {
                doLine("504 only the ascii and image types are supported");
                return false;
            }
            doLine("200 type is " + s);
            return false;
        }
        if (a.equals("mode")) {
            a = cmd.word().toLowerCase();
            if (a.equals("s")) {
                doLine("504 only the stream mode is supported");
                return false;
            }
            doLine("200 mode is stream");
            return false;
        }
        if (a.equals("stru")) {
            a = cmd.word().toLowerCase();
            if (a.equals("f")) {
                doLine("504 only the file structure is supported");
                return false;
            }
            doLine("200 structure is file");
            return false;
        }
        if (a.equals("dele")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            if (userFlash.delete(getRelPath(a, true))) {
                doLine("550 failed");
                return false;
            }
            doLine("250 successful");
            return false;
        }
        if (a.equals("rmd")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            if (userFlash.delete(getRelPath(a, true))) {
                doLine("550 failed");
                return false;
            }
            doLine("250 successful");
            return false;
        }
        if (a.equals("mkd")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            if (userFlash.mkdir(getRelPath(a, true))) {
                doLine("550 failed");
                return false;
            }
            doLine("250 successful");
            return false;
        }
        if (a.equals("rnfr")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            renameFrom = getRelPath(cmd.getRemaining(), false);
            doLine("250 successful");
            return false;
        }
        if (a.equals("rnto")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            if (userFlash.rename(getRelPath(renameFrom, true), getRelPath(a, true), false, false)) {
                doLine("550 failed");
                return false;
            }
            doLine("250 successful");
            return false;
        }
        if (a.equals("size")) {
            a = getRelPath(cmd.getRemaining(), false);
            File f = new File(getRelPath(a, true));
            if (!f.exists()) {
                doLine("550 not exists");
                return false;
            }
            doLine("213 " + f.length());
            return false;
        }
        if (a.equals("mdtm")) {
            a = getRelPath(cmd.getRemaining(), false);
            File f = new File(getRelPath(a, true));
            if (!f.exists()) {
                doLine("550 not exists");
                return false;
            }
            doLine("213 " + f.lastModified());
            return false;
        }
        if (a.equals("allo")) {
            doLine("200 command is meaningless");
            return false;
        }
        if (a.equals("mlst")) {
            a = getRelPath(cmd.getRemaining(), false);
            File f = new File(getRelPath(a, true));
            if (!f.exists()) {
                doLine("550 not exists");
                return false;
            }
            doLine("250-listing " + a);
            doLine("250-" + getNewInfo(f));
            doLine("250 end");
            return false;
        }
        if (a.equals("pasv")) {
            addrIPv4 adr4 = conn.connCmnd.iface.addr.toIPv4();
            a = "";
            byte[] buf = adr4.getBytes();
            for (int i = 0; i < buf.length; i++) {
                a = a + (buf[i] & 0xff) + ",";
            }
            int i = lower.srvPort - 1;
            a = a + (i >>> 8) + "," + (i & 0xff);
            doLine("227 entering passive mode (" + a + ")");
            return false;
        }
        if (a.equals("epsv")) {
            int i = lower.srvPort - 1;
            a = "|||" + i + "|";
            doLine("227 entering passive mode (" + a + ")");
            return false;
        }
        if (a.equals("port")) {
            if (lower.passiveOnly) {
                doLine("550 not allowed");
                return false;
            }
            byte[] buf = new byte[128];
            int i = 0;
            for (;;) {
                a = cmd.word(",");
                buf[i] = (byte) bits.str2num(a);
                if (a.length() < 1) {
                    break;
                }
                i++;
            }
            addrIPv4 adr4 = new addrIPv4();
            adr4.fromBuf(buf, 0);
            addrIP adr = new addrIP();
            adr.fromIPv4addr(adr4);
            i = bits.msbGetW(buf, 4);
            prtTcp t = lower.srvVrf.getTcp(adr);
            pipeSide p = t.streamConnect(new pipeLine(32768, false), null, 0, adr, i, lower.srvName(), -1, null, -1, -1);
            conn.closer(2);
            conn.adder(lower, null, p);
            doLine("220 connecting " + adr + " " + i);
            return false;
        }
        if (a.equals("eprt")) {
            if (lower.passiveOnly) {
                doLine("550 not allowed");
                return false;
            }
            addrIP adr = new addrIP();
            cmd.word("|");
            cmd.word("|");
            adr.fromString(cmd.word("|"));
            int i = bits.str2num(cmd.word("|"));
            prtTcp t = lower.srvVrf.getTcp(adr);
            pipeSide p = t.streamConnect(new pipeLine(32768, false), null, 0, adr, i, lower.srvName(), -1, null, -1, -1);
            conn.closer(2);
            conn.adder(lower, null, p);
            doLine("220 connecting " + adr + " " + i);
            return false;
        }
        if (a.equals("abor")) {
            conn.closer(0x22);
            doLine("226 successful");
            return false;
        }
        if (a.equals("list")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = "./";
            }
            a = getRelPath(a, false);
            File[] fl = userFlash.dirList(getRelPath(a, true));
            if (fl == null) {
                doLine("550 not directory");
                return false;
            }
            if (wait4data()) {
                doLine("550 data pipe error");
                return false;
            }
            doLine("150 data pipe ready, transferring");
            data.lineTx = pipeSide.modTyp.modeCRLF;
            for (int i = 0; i < fl.length; i++) {
                data.linePut(getOldInfo(fl[i]));
            }
            conn.closer(0x22);
            doLine("226 successful");
            return false;
        }
        if (a.equals("nlst")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = "./";
            }
            a = getRelPath(a, false);
            File[] fl = userFlash.dirList(getRelPath(a, true));
            if (fl == null) {
                doLine("550 not directory");
                return false;
            }
            if (wait4data()) {
                doLine("550 data pipe error");
                return false;
            }
            doLine("150 data pipe ready, transferring");
            data.lineTx = pipeSide.modTyp.modeCRLF;
            for (int i = 0; i < fl.length; i++) {
                data.linePut(getNameInfo(fl[i]));
            }
            conn.closer(0x22);
            doLine("226 successful");
            return false;
        }
        if (a.equals("mlsd")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = "./";
            }
            a = getRelPath(a, false);
            File[] fl = userFlash.dirList(getRelPath(a, true));
            if (fl == null) {
                doLine("550 not directory");
                return false;
            }
            if (wait4data()) {
                doLine("550 data pipe error");
                return false;
            }
            doLine("150 data pipe ready, transferring");
            data.lineTx = pipeSide.modTyp.modeCRLF;
            for (int i = 0; i < fl.length; i++) {
                data.linePut(getNewInfo(fl[i]));
            }
            conn.closer(0x22);
            doLine("226 successful");
            return false;
        }
        if (a.equals("rest")) {
            restartFrom = bits.str2long(cmd.word());
            doLine("350 restarting at " + restartFrom);
            return false;
        }
        if (a.equals("retr")) {
            a = getRelPath(cmd.getRemaining(), false);
            a = getRelPath(a, true);
            sendResult(sendOneFile(new File(a), restartFrom));
            restartFrom = 0;
            conn.closer(0x22);
            return false;
        }
        if (a.equals("stor")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            a = getRelPath(a, true);
            userFlash.mkfile(a);
            sendResult(recvOneFile(new File(a), restartFrom));
            restartFrom = 0;
            conn.closer(0x22);
            return false;
        }
        if (a.equals("appe")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(cmd.getRemaining(), false);
            a = getRelPath(a, true);
            File f = new File(a);
            long siz = 0;
            try {
                siz = f.length();
            } catch (Exception e) {
                doLine("550 not found");
                return false;
            }
            sendResult(recvOneFile(f, siz));
            conn.closer(0x22);
            return false;
        }
        if (a.equals("stou")) {
            if (lower.readOnly) {
                doLine("550 not allowed");
                return false;
            }
            a = getRelPath(bits.getTime() + ".bin", false);
            a = getRelPath(a, true);
            sendResult(recvOneFile(new File(a), 0));
            conn.closer(0x22);
            return false;
        }
        doLine("500 bad command");
        return false;
    }

    public void run() {
        try {
            doLine("220 server ready");
            for (;;) {
                if (doOne()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.removeConn(conn);
        conn.closer(3);
    }

}
