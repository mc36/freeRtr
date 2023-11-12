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
     * side one
     */
    public final cfgVnetSide side1 = new cfgVnetSide();

    /**
     * side two
     */
    public final cfgVnetSide side2 = new cfgVnetSide();

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
        l.add(null, "2 3       name                      name of interface");
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
        side1.getShRun(l, cmds.tabulator, 1);
        side2.getShRun(l, cmds.tabulator, 2);
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
    public void stop2run() {
        ///////////
    }

}

class cfgVnetSide {

    public userHwdet.ifcTyp ifcTyp;

    public String ifcNam;

    public cfgVnetSide copyBytes() {
        cfgVnetSide n = new cfgVnetSide();
        n.ifcTyp = ifcTyp;
        n.ifcNam = ifcNam;
        return n;
    }

    public void getShRun(List<String> lst, String beg, int num) {
        cmds.cfgLine(lst, ifcTyp == null, beg, "side" + num + " type", "" + ifcTyp);
        cmds.cfgLine(lst, ifcNam == null, beg, "side" + num + " name", ifcNam);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("name")) {
            ifcNam = cmd.word();
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
        if (a.equals("name")) {
            ifcNam = null;
            return;
        }
        if (a.equals("type")) {
            ifcTyp = null;
            return;
        }
        cmd.badCmd();
    }

}
