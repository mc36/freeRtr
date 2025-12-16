package org.freertr.auth;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

/**
 * authenticate one user
 *
 * @author matecsaba
 */
public abstract class authGeneric implements Comparable<authGeneric> {

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
     * last failure response
     */
    public long lastFail;

    /**
     * last error response
     */
    public long lastErr;

    /**
     * last ok response
     */
    public long lastOk;

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
     * account user session
     *
     * @param user username
     * @param addr address
     * @param sess session
     * @param cntr counter
     * @param stat status, 1=start, 2=stop, 3=update
     * @return accounting value
     */
    public abstract authResult acntUserSession(String user, String addr, int sess, counter cntr, int stat);

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
     * authenticate user by username
     *
     * @param user username
     * @return authentication value
     */
    public abstract authResult authUserNone(String user);

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
    public abstract void getHelp(userHelp l);

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
     * get show
     *
     * @return show
     */
    public abstract userFormat getShowSpec();

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return autName;
    }

    public int compareTo(authGeneric o) {
        return autName.toLowerCase().compareTo(o.autName.toLowerCase());
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShowGlob() {
        userFormat res = new userFormat("|", "reply|times|ago|last");
        res.add("ok|" + sawOk + "|" + bits.timePast(lastOk) + "|" + bits.time2str(cfgAll.timeZoneName, lastOk + cfgAll.timeServerOffset, 3));
        res.add("fail|" + sawFail + "|" + bits.timePast(lastFail) + "|" + bits.time2str(cfgAll.timeZoneName, lastFail + cfgAll.timeServerOffset, 3));
        res.add("error|" + sawErr + "|" + bits.timePast(lastErr) + "|" + bits.time2str(cfgAll.timeZoneName, lastErr + cfgAll.timeServerOffset, 3));
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
