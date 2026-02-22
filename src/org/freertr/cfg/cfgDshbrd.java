package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one dashboard
 *
 * @author matecsaba
 */
public class cfgDshbrd implements Comparable<cfgDshbrd>, cfgGeneric {

    /**
     * name of dashboard
     */
    public String name;

    /**
     * last reported
     */
    public long last;

    /**
     * time elapsed
     */
    public int time;

    /**
     * reports generated
     */
    public int cnt;

    /**
     * description of this dashboard
     */
    public String description = null;

    /**
     * actions
     */
    protected tabGen<cfgDshbrdNtry> actions = new tabGen<cfgDshbrdNtry>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {};

    /**
     * create new sensor
     *
     * @param n name
     */
    public cfgDshbrd(String n) {
        name = n;
    }

    public int compareTo(cfgDshbrd o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "set sequence number");
        l.add(null, false, 2, new int[]{1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "text", "text to show");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "char", "char to show");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "ascii code");
        l.add(null, false, 1, new int[]{-1}, "newline", "new line to show");
        l.add(null, false, 1, new int[]{2, -1}, "sensor", "sensor information");
        l.add(null, false, 2, new int[]{3}, "<name:sns>", "sensor name");
        l.add(null, false, 3, new int[]{-1}, "<num>", "column to show");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex time map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this chat map");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("dashboard " + name);
        for (int i = 0; i < actions.size(); i++) {
            l.add(cmds.tabulator + actions.get(i).getCfg(filter));
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals(cmds.negated);
        if (!neg) {
            cmd = cmd.copyBytes(true);
        }
        if (s.equals("rename")) {
            s = cmd.word();
            cfgDshbrd v = cfgAll.dshbrdFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
            return;
        }
        if (s.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            reindex(i, bits.str2num(cmd.word()));
            return;
        }
        cfgDshbrdNtry ntry = new cfgDshbrdNtry();
        if (ntry.fromString(cmd)) {
            cmd.badCmd();
            return;
        }
        if (neg) {
            actions.del(ntry);
            return;
        }
        if (ntry.seq < 1) {
            ntry.seq = 0;
            cfgDshbrdNtry old = actions.get(actions.size() - 1);
            if (old == null) {
                old = ntry;
            }
            ntry.seq = old.seq + 10;
        }
        actions.put(ntry);
        return;
    }

    public String getPrompt() {
        return "dash";
    }

    /**
     * reindex dashboard
     *
     * @param beg first number
     * @param inc increment
     */
    public void reindex(int beg, int inc) {
        if (beg < 1) {
            beg = 10;
        }
        if (inc < 1) {
            inc = 10;
        }
        for (int i = 0; i < actions.size(); i++) {
            cfgDshbrdNtry t = actions.get(i);
            t.seq = (inc * i) + beg;
        }
    }

    /**
     * get result
     *
     * @return result
     */
    public List<String> getResult() {
        last = bits.getTime();
        cnt++;
        List<String> lst = new ArrayList<String>();
        String s = "";
        for (int i = 0; i < actions.size(); i++) {
            cfgDshbrdNtry t = actions.get(i);
            switch (t.act) {
                case nwln:
                    lst.add(s);
                    s = "";
                    break;
                case str:
                    s += t.str;
                    break;
                case chr:
                    s += (char) t.num;
                    break;
                case snsr:
                    s += t.val.getDashValue(t.num);
                    break;
                default:
                    break;
            }
        }
        lst.add(s);
        time = (int) (bits.getTime() - last);
        return lst;
    }

}

class cfgDshbrdNtry implements Comparable<cfgDshbrdNtry> {

    public int seq;

    public command act;

    public int num;

    public String str;

    public cfgSensor val;

    public enum command {
        descr,
        str,
        chr,
        nwln,
        snsr
    }

    public int compareTo(cfgDshbrdNtry o) {
        if (seq < o.seq) {
            return -1;
        }
        if (seq > o.seq) {
            return +1;
        }
        return 0;
    }

    public String getCfg(int filter) {
        String s;
        switch (act) {
            case descr:
                s = "description " + str;
                break;
            case str:
                s = "text " + str;
                break;
            case chr:
                s = "char " + num;
                break;
            case snsr:
                s = "sensor " + val.name + " " + num;
                break;
            case nwln:
                s = "newline";
                break;
            default:
                s = "unknown";
                break;
        }
        return "sequence " + seq + " " + s;
    }

    public boolean fromString(cmds cmd) {
        String s = cmd.word();
        if (s.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            s = cmd.word();
        }
        if (s.equals("description")) {
            act = command.descr;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("text")) {
            act = command.str;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("char")) {
            act = command.chr;
            num = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("newline")) {
            act = command.nwln;
            return false;
        }
        if (s.equals("sensor")) {
            act = command.snsr;
            val = cfgAll.sensorFind(cmd.word(), false);
            num = bits.str2num(cmd.word());
            return val == null;
        }
        return true;
    }

}
