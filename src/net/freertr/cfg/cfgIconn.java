package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.ifc.ifcConnect;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcUp;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * iconnect configuration
 *
 * @author matecsaba
 */
public class cfgIconn implements Comparator<cfgIconn>, cfgGeneric {

    /**
     * create instance
     */
    public cfgIconn() {
    }

    /**
     * name of connect
     */
    public String name;

    /**
     * description
     */
    public String description;

    /**
     * side 1
     */
    public ifcEthTyp side1 = null;

    /**
     * side 2
     */
    public ifcEthTyp side2 = null;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "connect .*! no description",
        "connect .*! no side1",
        "connect .*! no side2"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgIconn o1, cfgIconn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "iconnect " + name;
    }

    private void getSideHelp(userHelping l) {
        l.add(null, "2 .     <name>                      name of interface");
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 3,. description                   specify description");
        l.add(null, "3 3,.   <str>                       text");
        l.add(null, "1 2  side1                          specify first side of connection");
        getSideHelp(l);
        l.add(null, "1 2  side2                          specify seconds side of connection");
        getSideHelp(l);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("connect " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, side1 == null, cmds.tabulator, "side1", "" + side1);
        cmds.cfgLine(l, side2 == null, cmds.tabulator, "side2", "" + side2);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        stop2run();
        String s = cmd.word();
        if (s.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (s.equals("side1")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            side1 = ifc.ethtyp;
            start2run();
            return;
        }
        if (s.equals("side2")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            side2 = ifc.ethtyp;
            start2run();
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("description")) {
            description = null;
            return;
        }
        if (s.equals("side1")) {
            side1 = null;
            return;
        }
        if (s.equals("side2")) {
            side2 = null;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "conn";
    }

    /**
     * stop running
     */
    public void stop2run() {
        if (side1 != null) {
            side1.delET(-1);
        }
        if (side2 != null) {
            side2.delET(-1);
        }
    }

    /**
     * start running
     */
    public void start2run() {
        if (side1 == null) {
            return;
        }
        if (side2 == null) {
            return;
        }
        ifcConnect con = new ifcConnect();
        ifcUp side = con.getSide1();
        side1.addET(-1, "connect", side);
        side1.updateET(-1, side);
        side = con.getSide2();
        side2.addET(-1, "connect", side);
        side2.updateET(-1, side);
        con.setPromiscous(true);
    }

}
