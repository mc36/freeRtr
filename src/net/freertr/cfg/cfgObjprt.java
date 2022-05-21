package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabObjprtN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * port object group configuration
 *
 * @author matecsaba
 */
public class cfgObjprt implements Comparator<cfgObjprt>, cfgGeneric {

    /**
     * name of access list
     */
    public String name;

    /**
     * description of access list
     */
    public String description;

    /**
     * list of statements
     */
    public tabListing<tabObjprtN<addrIP>, addrIP> objgrp;

    /**
     * create object group
     */
    public cfgObjprt() {
        objgrp = new tabListing<tabObjprtN<addrIP>, addrIP>();
    }

    /**
     * defaults text
     */
    public final static String[] defaultL = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgObjprt o1, cfgObjprt o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public void getHelp(userHelping l) {
        l.add(null, "1  2   sequence              sequence number of an entry");
        l.add(null, "2  1     <num>               sequence number");
        l.add(null, "1  3,. description           specify description");
        l.add(null, "3  3,.   <str>               text");
        l.add(null, "1  2   rename                rename this object group");
        l.add(null, "2  .     <str>               set new name");
        l.add(null, "1  .   <port>                port");
        l.add(null, "1  2,. reindex               reindex access list");
        l.add(null, "2  3,.   [num]               initial number to start with");
        l.add(null, "3  4,.     [num]             increment number");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("object-group port " + name);
        if (description != null) {
            l.add(cmds.tabulator + "description " + description);
        }
        l.addAll(objgrp.dump(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("description")) {
                description = null;
                return;
            }
            if (a.equals("sequence")) {
                tabObjprtN<addrIP> ntry = new tabObjprtN<addrIP>(new addrIP());
                ntry.sequence = bits.str2num(cmd.word());
                if (objgrp.del(ntry)) {
                    cmd.error("invalid sequence");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgObjprt v = cfgAll.objprtFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            objgrp.listName = a;
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            objgrp.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        int seq = objgrp.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        tabObjprtN<addrIP> ntry = new tabObjprtN<addrIP>(new addrIP());
        if (ntry.fromString(a + " " + cmd.getRemaining())) {
            cmd.error("invalid port");
            return;
        }
        ntry.sequence = seq;
        objgrp.add(ntry);
    }

    public String getPrompt() {
        return "objgrp";
    }

}
