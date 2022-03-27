package net.freertr.auth;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * authenticate one user
 *
 * @author matecsaba
 */
public abstract class authGeneric implements Comparator<authGeneric> {

    /**
     * create instance
     */
    public authGeneric() {
    }

    /**
     * name of list
     */
    public String autName = "";

    /**
     * log on failure
     */
    public boolean logFail = false;

    /**
     * log on error
     */
    public boolean logErr = false;

    /**
     * log on success
     */
    public boolean logOk = false;

    /**
     * log password
     */
    public boolean logPass = false;

    /**
     * number of failure responses
     */
    public int sawFail;

    /**
     * number of error responses
     */
    public int sawErr;

    /**
     * number of ok responses
     */
    public int sawOk;

    /**
     * last response
     */
    public long sawLast;

    /**
     * authenticate user by username/password
     *
     * @param user username
     * @param pass password
     * @return authentication value
     */
    public abstract authResult authUserPass(String user, String pass);

    /**
     * authorize user command
     *
     * @param user username
     * @param cmd command
     * @return authentication value
     */
    public abstract authResult authUserCommand(String user, String cmd);

    /**
     * authenticate user by username/chap
     *
     * @param user username
     * @param id id received
     * @param chal challenge requested
     * @param resp response received
     * @return authentication value
     */
    public abstract authResult authUserChap(String user, int id, byte[] chal, byte[] resp);

    /**
     * authenticate user by username/apop
     *
     * @param cookie cookie
     * @param user username
     * @param resp response received
     * @return authentication value
     */
    public abstract authResult authUserApop(String cookie, String user, String resp);

    /**
     * authenticate user by username/pubkey
     *
     * @param key public key
     * @param algo hash algorithm
     * @param algn sign algorithm
     * @param chal challenge
     * @param user username
     * @param resp response received
     * @return authentication value
     */
    public abstract authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, String algn, byte[] chal, String user, byte[] resp);

    /**
     * check user by username/pubkey
     *
     * @param key public key
     * @param user username
     * @return authentication value
     */
    public abstract authResult authUserPkey(cryKeyGeneric key, String user);

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @param filter filter defaults
     * @return string list
     */
    public abstract List<String> getShRun(String beg, int filter);

    /**
     * get help text
     *
     * @param l where to store
     */
    public abstract void getHelp(userHelping l);

    /**
     * get config text
     *
     * @return text
     */
    public abstract String getCfgName();

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public abstract boolean fromString(cmds cmd);

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return autName;
    }

    /**
     * compare two instances
     *
     * @param o1 first
     * @param o2 second
     * @return as usual
     */
    public int compare(authGeneric o1, authGeneric o2) {
        return o1.autName.toLowerCase().compareTo(o2.autName.toLowerCase());
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "category|value");
        res.add("ok|" + sawOk);
        res.add("fail|" + sawFail);
        res.add("error|" + sawErr);
        res.add("last|" + bits.time2str(cfgAll.timeZoneName, sawLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(sawLast) + " ago)");
        return res;
    }

    /**
     * convert route to prefix
     *
     * @param trg target
     * @param src route
     */
    public static void route2prefix(tabListing<tabPrfxlstN, addrIP> trg, String src) {
        src = src.trim();
        int i = src.indexOf(" ");
        if (i >= 0) {
            src = src.substring(0, i);
        }
        tabPrfxlstN ntry = new tabPrfxlstN();
        if (ntry.fromString(src)) {
            return;
        }
        ntry.sequence = trg.nextseq();
        ntry.action = tabListingEntry.actionType.actPermit;
        trg.add(ntry);
    }

    /**
     * convert route list to prefixes
     *
     * @param src route
     * @return converted
     */
    public static tabListing<tabPrfxlstN, addrIP> route2prefixes(String src) {
        tabListing<tabPrfxlstN, addrIP> res = new tabListing<tabPrfxlstN, addrIP>();
        res.listName = "converted";
        route2prefix(res, src);
        if (res.size() < 1) {
            return null;
        }
        return res;
    }

    /**
     * convert route list to prefixes
     *
     * @param src list
     * @return converted
     */
    public static tabListing<tabPrfxlstN, addrIP> routes2prefixes(List<String> src) {
        tabListing<tabPrfxlstN, addrIP> res = new tabListing<tabPrfxlstN, addrIP>();
        res.listName = "converted";
        for (int i = 0; i < src.size(); i++) {
            route2prefix(res, src.get(i));
        }
        if (res.size() < 1) {
            return null;
        }
        return res;
    }

}
