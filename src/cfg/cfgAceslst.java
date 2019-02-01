package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabAceslstN;
import tab.tabIntMatcher;
import tab.tabListing;
import tab.tabListingEntry;
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

    public String toString() {
        return name;
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
        l.add("1  2   sequence                      sequence number of an entry");
        l.add("2  1     <num>                       sequence number");
        l.add("1  3   permit                        specify networks to allow");
        l.add("1  3   deny                          specify networks to forbid");
        l.add("3  4     all                         no protocol matching");
        l.add("3  4     <proto>                     protocol number");
        l.add("4  6       any                       no source address matching");
        l.add("4  5       obj                       object group source address matching");
        l.add("4  5       <addr>                    address of source network");
        l.add("5  6         <mask>                  mask of source network");
        l.add("6  8           all                   no source port matching");
        l.add("6  8           <port>                source port");
        l.add("6  7           obj                   object group source port matching");
        l.add("7  8             <name>              name of object group");
        l.add("8  10            any                 no target address matching");
        l.add("8  9             obj                 object group target address matching");
        l.add("8  9             <addr>              address of target network");
        l.add("9  10              <mask>            mask of target network");
        l.add("10 12,.              all             no target port matching");
        l.add("10 12,.              <port>          target port");
        l.add("10 11                obj             object group target port matching");
        l.add("11 12,.                <name>        name of object group");
        l.add("12 13                  flag          tcp flags");
        l.add("13 12,.                  all         no flag matching");
        l.add("13 12,.                  <num>       flag value");
        l.add("12 13                  tos           type of service matching");
        l.add("13 12,.                  all         no tos matching");
        l.add("13 12,.                  <num>       tos value");
        l.add("12 13                  dscp          dscp matching");
        l.add("13 12,.                  all         no dscp matching");
        l.add("13 12,.                  <num>       tos value");
        l.add("12 13                  prec          precedence matching");
        l.add("13 12,.                  all         no precedence matching");
        l.add("13 12,.                  <num>       tos value");
        l.add("12 13                  len           length matching");
        l.add("13 12,.                  all         no length matching");
        l.add("13 12,.                  <num>       length value");
        l.add("12 13                  ttl           time to live matching");
        l.add("13 12,.                  all         no ttl matching");
        l.add("13 12,.                  <num>       ttl value");
        l.add("12 12,.                log           log on matching");
        l.add("1  2,. reindex                       reindex access list");
        l.add("2  3,.   [num]                       initial number to start with");
        l.add("3  4,.     [num]                     increment number");
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("sequence")) {
                tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
                ntry.sequence = bits.str2num(cmd.word());
                if (aceslst.del(ntry)) {
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
        tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
        ntry.action = tabListingEntry.string2action(a);
        if (tabAceslstN.fromString(ntry, cmd)) {
            cmd.error("invalid network");
            return;
        }
        ntry.sequence = seq;
        aceslst.add(ntry);
    }

    public int compare(cfgAceslst o1, cfgAceslst o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "acl";
    }

}
