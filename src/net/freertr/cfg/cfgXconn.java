package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.ifc.ifcConnect;
import net.freertr.pack.packLdpPwe;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * xconnect configuration
 *
 * @author matecsaba
 */
public class cfgXconn implements Comparator<cfgXconn>, cfgGeneric {

    /**
     * create instance
     */
    public cfgXconn() {
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
     * pw encapsulation
     */
    public int pwtype = packLdpPwe.pwtEthPort;

    /**
     * pw mtu
     */
    public int pwmtu = 1500;

    /**
     * side 1
     */
    public cfgXconnSide side1 = new cfgXconnSide();

    /**
     * side 2
     */
    public cfgXconnSide side2 = new cfgXconnSide();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "xconnect .*! no description",
        "xconnect .*! mtu 1500",
        "xconnect .*! type ethernet",
        "xconnect .*! no side1",
        "xconnect .*! no side2"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgXconn o1, cfgXconn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "xconnect " + name;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 3,. description                   specify description");
        l.add(null, "3 3,.   <str>                       text");
        l.add(null, "1 2  rename                         rename this xconnect");
        l.add(null, "2 .    <str>                        set new name");
        l.add(null, "1 2  mtu                            specify vc mtu");
        l.add(null, "2 .    <num>                        mtu");
        l.add(null, "1 2  type                           type of pseudowire");
        l.add(null, "2 .    ethernet                     ethernet mode");
        l.add(null, "2 .    ip                           ip mode");
        l.add(null, "2 .    vlan                         vlan mode");
        l.add(null, "2 .    hdlc                         hdlc mode");
        l.add(null, "2 .    ppp                          ppp mode");
        l.add(null, "2 .    fr-dlci                      fr dlci mode");
        l.add(null, "2 .    atm-aal5                     atm aal5 mode");
        l.add(null, "1 2  side1                          specify first side of connection");
        cfgXconnSide.getHelp(l, 2);
        l.add(null, "1 2  side2                          specify seconds side of connection");
        cfgXconnSide.getHelp(l, 2);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("xconnect " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        cmds.cfgLine(l, pwmtu == 0, cmds.tabulator, "mtu", "" + pwmtu);
        cmds.cfgLine(l, pwtype < 1, cmds.tabulator, "type", packLdpPwe.type2string(pwtype));
        cmds.cfgLine(l, !side1.ready2run(), cmds.tabulator, "side1", side1.getCfg());
        cmds.cfgLine(l, !side2.ready2run(), cmds.tabulator, "side2", side2.getCfg());
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
        if (s.equals("rename")) {
            s = cmd.word();
            cfgXconn v = cfgAll.xconFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
            return;
        }
        if (s.equals("type")) {
            pwtype = packLdpPwe.string2type(cmd.word());
            start2run();
            return;
        }
        if (s.equals("mtu")) {
            pwmtu = bits.str2num(cmd.word());
            start2run();
            return;
        }
        if (s.equals("side1")) {
            side1.doCfg(cmd);
            start2run();
            return;
        }
        if (s.equals("side2")) {
            side2.doCfg(cmd);
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
            side1 = new cfgXconnSide();
            return;
        }
        if (s.equals("side2")) {
            side2 = new cfgXconnSide();
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "xconn";
    }

    /**
     * stop running
     */
    public void stop2run() {
        side1.stop2run();
        side2.stop2run();
    }

    /**
     * start running
     */
    public void start2run() {
        side1.stop2run();
        side2.stop2run();
        if (!side1.ready2run()) {
            return;
        }
        if (!side2.ready2run()) {
            return;
        }
        ifcConnect con = new ifcConnect();
        side1.upper = con.getSide1();
        side2.upper = con.getSide2();
        side1.name = name;
        side2.name = name;
        side1.pwtype = pwtype;
        side2.pwtype = pwtype;
        side1.pwmtu = pwmtu;
        side2.pwmtu = pwmtu;
        side1.start2run();
        side2.start2run();
    }

}
