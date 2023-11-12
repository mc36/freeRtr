package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userHwdet;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * virtual ethernet
 *
 * @author matecsaba
 */
public class cfgVnet implements Comparator<cfgVnet>, cfgGeneric {

    /**
     * number of this bridge
     */
    public final int number;

    /**
     * description of this bridge
     */
    public String description;

    /**
     * port number to use
     */
    protected int port;

    /**
     * side one
     */
    public final cfgVnetSide side1 = new cfgVnetSide(1);

    /**
     * side two
     */
    public final cfgVnetSide side2 = new cfgVnetSide(2);

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "vnet .*! no description",
        "vnet .*! no side1 type",
        "vnet .*! no side1 name",
        "vnet .*! no side2 type",
        "vnet .*! no side2 name"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgVnet o1, cfgVnet o2) {
        if (o1.number < o2.number) {
            return -1;
        }
        if (o1.number > o2.number) {
            return +1;
        }
        return 0;
    }

    /**
     * copy bytes
     *
     * @return copy
     */
    public cfgVnet copyBytes() {
        cfgVnet n = new cfgVnet("" + number);
        n.description = description;
        side1.copyBytes(n.side1);
        side2.copyBytes(n.side2);
        return n;
    }

    public String toString() {
        return "vnet " + number;
    }

    /**
     * create new bridge instance
     *
     * @param nam name of bridge
     */
    public cfgVnet(String nam) {
        number = bits.str2num(nam);
    }

    private void getHelp(userHelping l, int s, String n) {
        l.add(null, "1 2     side" + s + "                       configure side " + n);
        l.add(null, "2 3       type                      type of process");
        l.add(null, "3 .         socat                   use socat");
        l.add(null, "3 .         pcap                    use pcapint");
        l.add(null, "3 .         raw                     use rawint");
        l.add(null, "3 .         map                     use mapint");
        l.add(null, "2 3       local                     name of local interface");
        l.add(null, "3 .         <str>                   name");
        l.add(null, "2 3       connect                   name of connected interface");
        l.add(null, "3 .         <str>                   name");
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2,.   description                 description of this bridge");
        l.add(null, "2 2,.     [text]                    text describing this bridge");
        getHelp(l, 1, "one");
        getHelp(l, 2, "two");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("vnet " + number);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        side1.getShRun(l, cmds.tabulator);
        side2.getShRun(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("side1")) {
            side1.doCfgStr(cmd);
            return;
        }
        if (a.equals("side2")) {
            side2.doCfgStr(cmd);
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = null;
            return;
        }
        if (a.equals("side1")) {
            side1.doUnCfg(cmd);
            return;
        }
        if (a.equals("side2")) {
            side2.doUnCfg(cmd);
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vnet";
    }

    /**
     * stop work
     */
    public void stopNow() {
        ///////////
    }

    /**
     * start work
     */
    public void startNow(int p) {
        port = p;
        ////////////
    }

}

class cfgVnetSide {

    public final int id;

    public userHwdet.ifcTyp ifcTyp;

    public String locNam;

    public String conNam;

    public cfgVnetSide(int i) {
        id = i;
    }

    public void copyBytes(cfgVnetSide n) {
        n.ifcTyp = ifcTyp;
        n.locNam = locNam;
    }

    public void getShRun(List<String> lst, String beg) {
        cmds.cfgLine(lst, ifcTyp == null, beg, "side" + id + " type", "" + ifcTyp);
        cmds.cfgLine(lst, locNam == null, beg, "side" + id + " local", locNam);
        cmds.cfgLine(lst, locNam == null, beg, "side" + id + " connect", conNam);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("local")) {
            locNam = cmd.word();
            return;
        }
        if (a.equals("connect")) {
            conNam = cmd.word();
            return;
        }
        if (a.equals("type")) {
            a = cmd.word();
            ifcTyp = userHwdet.string2type(a);
            return;
        }
        cmd.badCmd();
    }

    public void doUnCfg(cmds cmd) {
        String a = cmd.word();
        if (a.equals("local")) {
            locNam = null;
            return;
        }
        if (a.equals("connect")) {
            conNam = null;
            return;
        }
        if (a.equals("type")) {
            ifcTyp = null;
            return;
        }
        cmd.badCmd();
    }

}
