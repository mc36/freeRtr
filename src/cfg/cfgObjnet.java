package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabListing;
import tab.tabObjnetN;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * network object group configuration
 *
 * @author matecsaba
 */
public class cfgObjnet implements Comparator<cfgObjnet>, cfgGeneric {

    /**
     * name of access list
     */
    public String name;

    /**
     * list of statements
     */
    public tabListing<tabObjnetN<addrIP>, addrIP> objgrp;

    /**
     * create object group
     */
    public cfgObjnet() {
        objgrp = new tabListing<tabObjnetN<addrIP>, addrIP>();
    }

    public int compare(cfgObjnet o1, cfgObjnet o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2   sequence              sequence number of an entry");
        l.add("2  1     <num>               sequence number");
        l.add("1  3       <addr>            address of network");
        l.add("3  .         <mask>          mask of network");
        l.add("1  2,. reindex               reindex access list");
        l.add("2  3,.   [num]               initial number to start with");
        l.add("3  4,.     [num]             increment number");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("object-group network " + name);
        l.addAll(objgrp.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("sequence")) {
                tabObjnetN<addrIP> ntry = new tabObjnetN<addrIP>(new addrIP());
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
        tabObjnetN<addrIP> ntry = new tabObjnetN<addrIP>(new addrIP());
        if (tabObjnetN.fromString(ntry, a + " " + cmd.getRemaining())) {
            cmd.error("invalid network");
            return;
        }
        ntry.sequence = seq;
        objgrp.add(ntry);
    }

    public String getPrompt() {
        return "objgrp";
    }

}
