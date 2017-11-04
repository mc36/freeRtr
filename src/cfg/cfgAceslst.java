package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabAceslstN;
import tab.tabListing;
import tab.tabListingEntry;
import tab.tabPlcmapN;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * access list configuration
 *
 * @author matecsaba
 */
public class cfgAceslst implements Comparator<cfgAceslst>, cfgGeneric {

    /**
     * name of access list
     */
    public String name;

    /**
     * list of statements
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> aceslst;

    /**
     * create new access list
     */
    public cfgAceslst() {
        aceslst = new tabListing<tabAceslstN<addrIP>, addrIP>();
    }

    /**
     * add one table entry with preset values
     *
     * @param seq sequence number
     * @param act statement allower, true to permit, false to deny
     * @param s string represeting proto src/mask/port trg/mask/port
     * @return true if error happened, false if not
     */
    public synchronized boolean add(int seq, tabPlcmapN.actionType act, String s) {
        tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
        if (ntry.fromString(s)) {
            return true;
        }
        ntry.sequence = seq;
        ntry.action = act;
        aceslst.add(ntry);
        return false;
    }

    /**
     * delete one entry
     *
     * @param seq sequence number
     * @return true if deleted, false if not
     */
    public boolean del(int seq) {
        tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
        ntry.sequence = seq;
        return aceslst.del(ntry);
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("access-list " + name);
        l.addAll(aceslst.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2   sequence              sequence number of an entry");
        l.add("2  1     <num>               sequence number");
        l.add("1  3   permit                specify networks to allow");
        l.add("1  3   deny                  specify networks to forbid");
        l.add("3  4     all                 no protocol matching");
        l.add("3  4     <proto>             protocol number");
        l.add("4  6       any               no source address matching");
        l.add("4  5       <addr>            address of source network");
        l.add("5  6         <mask>          mask of source network");
        l.add("6  7           all           no source port matching");
        l.add("6  7           <port>        source port");
        l.add("7  9             any         no target address matching");
        l.add("7  8             <addr>      address of target network");
        l.add("8  9               <mask>    mask of target network");
        l.add("9  10,.              all     no target port matching");
        l.add("9  10,.              <port>  target port");
        l.add("10 11                flag    tcp flags");
        l.add("11 10,.                all   no flag matching");
        l.add("11 10,.                <num> flag value");
        l.add("10 11                tos     type of service matching");
        l.add("11 10,.                all   no tos matching");
        l.add("11 10,.                <num> tos value");
        l.add("10 11                dscp    dscp matching");
        l.add("11 10,.                all   no dscp matching");
        l.add("11 10,.                <num> tos value");
        l.add("10 11                prec    precedence matching");
        l.add("11 10,.                all   no precedence matching");
        l.add("11 10,.                <num> tos value");
        l.add("10 11                len     length matching");
        l.add("11 10,.                all   no length matching");
        l.add("11 10,.                <num> length value");
        l.add("10 11                ttl     time to live matching");
        l.add("11 10,.                all   no ttl matching");
        l.add("11 10,.                <num> ttl value");
        l.add("10 10,.              log     log on matching");
        l.add("1  2,. reindex               reindex access list");
        l.add("2  3,.   [num]               initial number to start with");
        l.add("3  4,.     [num]             increment number");
        return l;
    }

    public void doCfgStr(cmds cmd) {
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
            aceslst.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        int seq = aceslst.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        if (add(seq, tabListingEntry.string2action(a), cmd.getRemaining())) {
            cmd.error("invalid network");
            return;
        }
    }

    public int compare(cfgAceslst o1, cfgAceslst o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "aces";
    }

}
