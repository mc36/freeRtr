package org.freertr.serv;

import java.io.File;
import java.util.List;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
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
 * internet message access protocol 4 (rfc1730) server
 *
 * @author matecsaba
 */
public class servImap4 extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servImap4() {
    }

    /**
     * port number
     */
    public final static int port = 143;

    /**
     * mail folders
     */
    public String mailFolders = "/data/";

    /**
     * authentication list
     */
    public authGeneric authenticList;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server imap4 .*", cmds.tabulator + "port " + port, null),
        new userFilter("server imap4 .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servImap4doer(this, pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (authenticList == null) {
            l.add(beg + "no authentication");
        } else {
            l.add(beg + "authentication " + authenticList.autName);
        }
        l.add(beg + "path " + mailFolders);
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
            mailFolders = "/" + encUrl.normalizePath(cmd.word() + "/");
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
            mailFolders = "/data/";
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "authentication", "set authentication");
        l.add(null, false, 2, new int[]{-1}, "<name:aaa>", "name of authentication list");
        l.add(null, false, 1, new int[]{2}, "path", "set root folder");
        l.add(null, false, 2, new int[]{-1}, "<path>", "name of root folder");
    }

    public String srvName() {
        return "imap4";
    }

    public int srvPort() {
        return port;
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

class servImap4msg implements Comparable<servImap4msg> {

    public String name;

    public int idx;

    public long size;

    public long date;

    public int compareTo(servImap4msg o) {
        return name.compareTo(o.name);
    }

}

class servImap4doer implements Runnable {

    private servImap4 lower;

    private pipeSide pipe;

    private String userN = "";

    private boolean authed = false;

    private tabGen<servImap4msg> deled = null;

    public servImap4doer(servImap4 parent, pipeSide stream) {
        lower = parent;
        pipe = stream;
        logger.startThread(this);
    }

    public tabGen<servImap4msg> getMsgList() {
        File f;
        File[] fl = userFlash.dirList(lower.mailFolders + userN);
        if (fl == null) {
            return null;
        }
        tabGen<servImap4msg> lst = new tabGen<servImap4msg>();
        for (int i = 0; i < fl.length; i++) {
            f = fl[i];
            servImap4msg n = new servImap4msg();
            n.name = f.getName();
            n.size = f.length();
            n.date = f.lastModified();
            lst.put(n);
        }
        for (int i = 0; i < lst.size(); i++) {
            servImap4msg cur = lst.get(i);
            cur.idx = i + 1;
        }
        return lst;
    }

    public int findMsgUid(tabGen<servImap4msg> lst, String uid) {
        if (uid.equals("1")) {
            return 0;
        }
        if (uid.equals("*")) {
            return lst.size() - 1;
        }
        uid += ".";
        for (int i = 0; i < lst.size(); i++) {
            servImap4msg cur = lst.get(i);
            if (cur.name.startsWith(uid)) {
                return i;
            }
        }
        return -1;
    }

    public tabGen<servImap4msg> getMsgSet(String set, boolean uid) {
        tabGen<servImap4msg> lst = getMsgList();
        if (lst == null) {
            return new tabGen<servImap4msg>();
        }
        int i = set.indexOf(":");
        if (i < 0) {
            if (uid) {
                i = findMsgUid(lst, set);
            } else {
                i = bits.str2num(set) - 1;
            }
            if (i < 0) {
                return new tabGen<servImap4msg>();
            }
            if (i >= lst.size()) {
                return new tabGen<servImap4msg>();
            }
            servImap4msg cur = lst.get(i);
            lst.clear();
            lst.add(cur);
            return lst;
        }
        int min;
        int max;
        if (uid) {
            min = findMsgUid(lst, set.substring(0, i));
            max = findMsgUid(lst, set.substring(i + 1, set.length()));
        } else {
            min = bits.str2num(set.substring(0, i)) - 1;
            max = bits.str2num(set.substring(i + 1, set.length())) - 1;
        }
        if (min < 0) {
            min = 0;
        }
        if (max >= lst.size()) {
            max = lst.size() - 1;
        }
        tabGen<servImap4msg> res = new tabGen<servImap4msg>();
        for (i = min; i <= max; i++) {
            res.add(lst.get(i));
        }
        return res;
    }

    public String getMsgName(servImap4msg msg) {
        return lower.mailFolders + userN + "/" + msg.name;
    }

    public String getMsgUid(servImap4msg msg) {
        int i = msg.name.indexOf(".");
        if (i < 0) {
            return msg.name;
        }
        return msg.name.substring(0, i);
    }

    public void doLine(String s) {
        if (debugger.servImap4traf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    public boolean doOne() {
        String s = pipe.lineGet(1).trim();
        if (debugger.servImap4traf) {
            logger.debug("rx: " + s);
        }
        cmds cmd = new cmds("", s);
        String tag = cmd.word();
        String a = cmd.word().toLowerCase();
        boolean uids = a.equals("uid");
        if (uids) {
            a = cmd.word().toLowerCase();
        }
        if (a.length() < 1) {
            return pipe.isClosed() != 0;
        }
        if (a.equals("capability")) {
            doLine("* CAPABILITY IMAP4");
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("noop")) {
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("logout")) {
            doLine("* BYE imap4 server logging out");
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("authenticate")) {
            doLine(tag + " NO unsupported method");
            return false;
        }
        if (a.equals("login")) {
            authed = false;
            userN = cmd.word().replaceAll("\"", "");
            authResult res = lower.authenticList.authUserPass(userN, cmd.word().replaceAll("\"", ""));
            if (res == null) {
                doLine(tag + " NO error");
                return true;
            }
            if (res.result != authResult.authSuccessful) {
                doLine(tag + " NO failed");
                return true;
            }
            doLine(tag + " OK completed");
            deled = new tabGen<servImap4msg>();
            authed = true;
            return false;
        }
        if (!authed) {
            doLine(tag + " BAD please authenticate");
            return false;
        }
        if (getMsgList() == null) {
            doLine(tag + " BAD invalid credentinals");
            authed = false;
            return true;
        }
        if (a.equals("select") || a.equals("examine")) {
            tabGen<servImap4msg> lst = getMsgList();
            doLine("* " + lst.size() + " exists");
            doLine("* " + lst.size() + " recent");
            doLine("* OK [UNSEEN 1] message 1 is the first");
            if (lst.size() > 0) {
                doLine("* OK [UIDVALIDITY " + getMsgUid(lst.get(0)) + "] UIDs are valid");
            }
            doLine("* FLAGS (\\Deleted)");
            doLine("* OK [PERMANENTFLAGS (\\Deleted \\*)] limited");
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("create") || a.equals("delete") || a.equals("rename") || a.equals("copy")) {
            doLine(tag + " NO you have only inbox");
            return false;
        }
        if (a.equals("subscribe") || a.equals("unsubscribe")) {
            doLine(tag + " NO no such list");
            return false;
        }
        if (a.equals("list")) {
            doLine("* OK LIST () \"\" INBOX");
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("lsub")) {
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("append")) {
            doLine(tag + " NO upload forbidden");
            return false;
        }
        if (a.equals("check") || a.equals("close")) {
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("expunge")) {
            if (deled != null) {
                tabGen<servImap4msg> lst = getMsgList();
                for (int i = 0; i < deled.size(); i++) {
                    doLine("* " + (lst.index(deled.get(i)) + 1) + " EXPUNGE");
                    userFlash.delete(getMsgName(deled.get(i)));
                }
                deled.clear();
            }
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("search")) {
            tabGen<servImap4msg> lst = getMsgList();
            a = "";
            if (uids) {
                for (int i = 0; i < lst.size(); i++) {
                    a += " " + getMsgUid(lst.get(i));
                }
            } else {
                for (int i = 0; i < lst.size(); i++) {
                    a += " " + (i + 1);
                }
            }
            doLine("* SEARCH" + a);
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("fetch")) {
            tabGen<servImap4msg> lst = getMsgSet(cmd.word(), uids);
            a = cmd.getRemaining();
            a = a.replaceAll("\\(", " ");
            a = a.replaceAll("\\)", " ");
            a = a.toLowerCase() + " ";
            boolean flg = a.indexOf("flags") >= 0;
            boolean dat = a.indexOf("internaldate") >= 0;
            boolean uid = a.indexOf("uid") >= 0;
            boolean env = a.indexOf("envelope") >= 0;
            boolean siz = a.indexOf("rfc822.size") >= 0;
            boolean hdr = a.indexOf("rfc822.header") >= 0;
            boolean txt = a.indexOf("rfc822.text") >= 0;
            if (a.indexOf("rfc822 ") >= 0) {
                hdr = true;
                txt = true;
            }
            if (a.indexOf("all") >= 0) {
                flg = true;
                dat = true;
                siz = true;
                env = true;
            }
            if (a.indexOf("fast") >= 0) {
                flg = true;
                dat = true;
                siz = true;
            }
            if (a.indexOf("full") >= 0) {
                flg = true;
                dat = true;
                siz = true;
                env = true;
            }
            for (int i = 0; i < lst.size(); i++) {
                servImap4msg cur = lst.get(i);
                a = "";
                if (flg) {
                    a += " FLAGS (";
                    if (deled.find(cur) != null) {
                        a += "\\Deleted";
                    }
                    a += ")";
                }
                if (dat) {
                    a += " INTERNALDATE \"" + bits.time2str(cfgAll.timeZoneName, cur.date, 4) + "\"";
                }
                if (uid) {
                    a += " UID \"" + getMsgUid(cur) + "\"";
                }
                if (env) {
                    a += " ENVELOPE ()";
                }
                if (siz) {
                    a += " RFC822.SIZE " + cur.size;
                }
                if (a.length() > 0) {
                    a = a.substring(1, a.length());
                }
                if (!hdr && !txt) {
                    doLine("* " + cur.idx + " FETCH (" + a + ")");
                    continue;
                }
                List<String> msg = bits.txt2buf(getMsgName(cur));
                if (msg == null) {
                    doLine("* " + cur.idx + " FETCH (" + a + ")");
                    continue;
                }
                int p = msg.size();
                for (int o = 0; o < msg.size(); o++) {
                    if (msg.get(o).length() > 0) {
                        continue;
                    }
                    p = o;
                    break;
                }
                a += " RFC822";
                if (hdr && !txt) {
                    for (int o = msg.size() - 1; o >= p; o--) {
                        msg.remove(o);
                    }
                    a += ".HEADER";
                }
                if (!hdr && txt) {
                    for (int o = p; o >= 0; o--) {
                        msg.remove(o);
                    }
                    a += ".TEXT";
                }
                p = 0;
                for (int o = 0; o < msg.size(); o++) {
                    p += msg.get(o).length() + 2;
                }
                doLine("* " + cur.idx + " FETCH (" + a + " {" + p + "}");
                for (int o = 0; o < msg.size(); o++) {
                    doLine(msg.get(o));
                }
                doLine(")");
            }
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("store")) {
            tabGen<servImap4msg> lst = getMsgSet(cmd.word(), uids);
            a = cmd.getRemaining().toLowerCase();
            if (a.indexOf("deleted") < 0) {
                doLine(tag + " NO no other flags");
                return false;
            }
            for (int i = 0; i < lst.size(); i++) {
                servImap4msg cur = lst.get(i);
                deled.add(cur);
            }
            doLine(tag + " OK completed");
            return false;
        }
        if (a.equals("partial")) {
            doLine(tag + " BAD unimplemented");
            return false;
        }
        doLine(tag + " BAD bad command");
        return false;
    }

    public void run() {
        try {
            doLine("* OK imap4 service ready");
            for (;;) {
                if (doOne()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
