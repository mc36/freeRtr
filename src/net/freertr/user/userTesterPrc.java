package net.freertr.user;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.cfg.cfgInit;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeShell;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * one tester process
 *
 * @author matecaba
 */
public class userTesterPrc implements Comparator<userTesterPrc> {

    /**
     * reader to use
     */
    protected final pipeProgress rdr;

    /**
     * test name
     */
    protected final String name;

    /**
     * slot to use
     */
    protected final int slot;

    /**
     * prefix to use
     */
    protected final String prefix;

    /**
     * shell to use
     */
    protected final pipeShell shell;

    /**
     * pipe to use
     */
    protected pipeSide pipe;

    /**
     * persistent process
     */
    protected boolean persistent;

    /**
     * syncer string
     */
    protected String syncr = "!!!hello there!!!";

    /**
     * connections
     */
    protected tabGen<userTesterCon> conns;

    /**
     * create instance
     *
     * @param reader reader to use
     * @param pfx test prefix
     * @param slt tester slot
     * @param nam test name
     * @param command command, null if none
     */
    protected userTesterPrc(pipeProgress reader, String pfx, int slt, String nam, String command) {
        slot = slt;
        name = nam;
        rdr = reader;
        prefix = pfx;
        if (command == null) {
            shell = null;
            return;
        }
        pipeLine pl = new pipeLine(32768, false);
        final int tim = 600 * 1000;
        pipe = pl.getSide();
        pipe.setTime(tim);
        shell = pipeShell.exec(pipe, command, null, true, false, false, true);
        pipe = pl.getSide();
        pipe.setTime(tim);
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        rdr.debugStat(slot + "/" + name + ": starting process");
    }

    public int compare(userTesterPrc o1, userTesterPrc o2) {
        return o1.name.compareTo(o2.name);
    }

    protected void waitFor() {
        if (shell == null) {
            return;
        }
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.waitFor();
        pipe.setClose();
    }

    protected void stopNow() {
        if (shell == null) {
            return;
        }
        if (persistent) {
            return;
        }
        rdr.debugStat(slot + "/" + name + ": stopping process");
        shell.kill();
        shell.waitFor();
        pipe.setClose();
    }

    protected static String getLogName(String pfx, int slt, String nam, int mod) {
        String s;
        switch (mod) {
            case 1:
                s = "run";
                break;
            case 2:
                s = "all";
                break;
            case 3:
                s = "err";
                break;
            case 4:
                s = "con";
                break;
            default:
                s = "log";
                break;
        }
        return pfx + slt + nam + "-log." + s;
    }

    protected String getLogName(int mod) {
        return getLogName(prefix, slot, name, mod);
    }

    protected void putChar(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) i;
        pipe.blockingPut(buf, 0, buf.length);
    }

    protected String getLine() {
        String s = pipe.lineGet(0x11);
        if (s == null) {
            return null;
        }
        if (s.length() < 1) {
            if (pipe.isClosed() != 0) {
                return null;
            }
            return "";
        }
        bits.buf2txt(false, bits.str2lst("rx:" + s), getLogName(4));
        return s;
    }

    protected void putLine(String s) {
        bits.buf2txt(false, bits.str2lst("tx:" + s), getLogName(4));
        pipe.linePut(s);
    }

    protected int doPing(String s) {
        putLine("ping " + s);
        for (;;) {
            s = getLine();
            if (s == null) {
                return -100000000;
            }
            s = s.toLowerCase();
            if (s.startsWith("result=")) {
                break;
            }
        }
        int i = s.indexOf("=");
        if (i < 0) {
            return -100000000;
        }
        s = s.substring(i + 1, s.length());
        i = s.indexOf("%");
        s = s.substring(0, i);
        i = s.indexOf(".");
        if (i >= 0) {
            s = s.substring(0, i);
        }
        return bits.str2num(s);
    }

    protected boolean morePings(String s, tabIntMatcher ned, int round) {
        rdr.debugStat(slot + "/" + name + ": pinging " + s + ".");
        rdr.setMax(round);
        int i = -1;
        for (int rnd = 0; rnd <= round; rnd++) {
            rdr.setCurr(rnd);
            i = doPing(s);
            bits.buf2txt(false, bits.str2lst("res:" + i + " percent"), getLogName(4));
            if (ned.matches(i)) {
                return false;
            }
            bits.sleep(1000);
        }
        rdr.debugStat(slot + "/" + name + ": test failed: got " + i + ", expected " + ned);
        return true;
    }

    protected List<String> getOutput(String s) {
        String beg = "!begin-command-" + s;
        String end = "!end-command-" + s;
        putLine(beg);
        putLine(s);
        putLine(end);
        List<String> res = new ArrayList<String>();
        for (;;) {
            String a = getLine();
            if (a == null) {
                return null;
            }
            a = bits.trimE(a);
            if (a.length() < 1) {
                continue;
            }
            if (a.endsWith(beg)) {
                res.clear();
                continue;
            }
            if (a.endsWith(end)) {
                return res;
            }
            res.add(a);
        }
    }

    protected int getSummary(String inc, String exc) {
        List<String> buf = getOutput("show interface swsummary");
        if (buf == null) {
            return -1;
        }
        int tot = 0;
        int usd = 0;
        for (int i = 0; i < buf.size(); i++) {
            String s = buf.get(i);
            if (s.indexOf(inc) < 0) {
                continue;
            }
            if (s.indexOf(exc) >= 0) {
                continue;
            }
            bits.buf2txt(false, bits.str2lst("hit:" + s), getLogName(4));
            cmds cmd = new cmds("res", s);
            int col = 0;
            int sum = 0;
            for (;;) {
                s = cmd.word(";");
                if (s.length() < 1) {
                    break;
                }
                col++;
                sum += bits.str2num(s);
            }
            if (col < 3) {
                continue;
            }
            tot += sum;
            usd++;
        }
        bits.buf2txt(false, bits.str2lst("res:" + tot + " bytes"), getLogName(4));
        if (usd < 1) {
            return -1;
        }
        return tot;
    }

    protected void doSync() {
        if (syncr.length() < 1) {
            return;
        }
        String s = syncr + bits.randomD();
        putLine(s);
        for (;;) {
            String a = getLine();
            if (a == null) {
                return;
            }
            a = bits.trimE(a);
            if (a.indexOf(s) >= 0) {
                break;
            }
        }
    }

    protected void applyCfg(List<String> cfg) {
        doSync();
        rdr.setMax(cfg.size());
        for (int i = 0; i < cfg.size(); i++) {
            if ((i % 5) == 4) {
                doSync();
            }
            putLine(cfg.get(i));
            bits.sleep(100);
            rdr.setCurr(i);
        }
        doSync();
    }

    protected void readConns() {
        conns = new tabGen<userTesterCon>();
        List<String> l = bits.txt2buf(prefix + slot + name + "-" + cfgInit.hwCfgEnd);
        if (l == null) {
            return;
        }
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i);
            if (!a.startsWith("int ")) {
                continue;
            }
            userTesterCon c = new userTesterCon();
            a = a.substring(4, a.length());
            c.ifc = a.substring(0, a.indexOf(" "));
            int p = a.indexOf("127.0.0.1");
            if (p < 0) {
                continue;
            }
            c.locP = bits.str2num(a.substring(p + 10, p + 15));
            p = a.lastIndexOf("127.0.0.1");
            if (p < 0) {
                continue;
            }
            c.remP = bits.str2num(a.substring(p + 10, p + 15));
            conns.add(c);
        }
    }

}
