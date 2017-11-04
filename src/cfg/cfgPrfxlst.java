package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabListing;
import tab.tabListingEntry;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * prefix list configuration
 *
 * @author matecsaba
 */
public class cfgPrfxlst implements Comparator<cfgPrfxlst>, cfgGeneric {

    /**
     * name of prefixlist
     */
    public String name;

    /**
     * list of prefixes
     */
    public tabListing<tabPrfxlstN, addrIP> prflst;

    /**
     * create new prefix list
     */
    public cfgPrfxlst() {
        prflst = new tabListing<tabPrfxlstN, addrIP>();
    }

    /**
     * add one table entry with preset values
     *
     * @param seq sequence number
     * @param act prefix allower, true to permit, false to deny
     * @param s string represeting network/mask ge X le Y
     * @return true if error happened, false if not
     */
    public synchronized boolean add(int seq, tabPlcmapN.actionType act, String s) {
        tabPrfxlstN ntry = new tabPrfxlstN();
        if (ntry.fromString(s)) {
            return true;
        }
        ntry.sequence = seq;
        ntry.action = act;
        prflst.add(ntry);
        return false;
    }

    /**
     * delete one entry
     *
     * @param seq sequence number
     * @return true if deleted, false if not
     */
    public boolean del(int seq) {
        tabPrfxlstN ntry = new tabPrfxlstN();
        ntry.sequence = seq;
        return prflst.del(ntry);
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("prefix-list " + name);
        l.addAll(prflst.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2   sequence              sequence number of an entry");
        l.add("2 1     <num>               sequence number");
        l.add("1 3   permit                specify networks to allow");
        l.add("1 3   deny                  specify networks to forbid");
        l.add("3 4,.   <net/mask>          network in perfix/mask format");
        l.add("4 5       ge                minimum prefix length to be matched");
        l.add("5 4,.       <num>           minimum prefix length");
        l.add("4 5       le                maximum prefix length to be matched");
        l.add("5 4,.       <num>           maximum prefix length");
        l.add("4 4,.     log               set logging on match");
        l.add("1 2,. reindex               reindex prefix list");
        l.add("2 3,.   [num]               initial number to start with");
        l.add("3 4,.     [num]             increment number");
        return l;
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("sequence")) {
                if (del(bits.str2num(cmd.word()))) {
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
            prflst.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        int seq = prflst.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        if (add(seq, tabListingEntry.string2action(a), cmd.getRemaining())) {
            cmd.error("invalid network");
            return;
        }
    }

    public int compare(cfgPrfxlst o1, cfgPrfxlst o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "prfx";
    }

    public String toString() {
        return name;
    }

}
