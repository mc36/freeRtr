package org.freertr.auth;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.tab.tabAuthlstN;
import org.freertr.tab.tabListing;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

/**
 * list of methods
 *
 * @author matecsaba
 */
public class authList extends authGeneric {

    /**
     * create instance
     */
    public authList() {
    }

    private tabListing<tabAuthlstN, addrIP> methods = new tabListing<tabAuthlstN, addrIP>();

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "select sequence number");
        l.add(null, false, 2, new int[]{3}, "<num>", "number of entry");
        l.add(null, false, 3, new int[]{-1}, "<name:aaa>", "name of authenticator");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex prefix list");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
    }

    public List<String> getShRun(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        l.addAll(methods.dump(cmds.tabulator, filter));
        return l;
    }

    public String getCfgName() {
        return "list";
    }

    public boolean fromString(cmds cmd) {
        String a = cmd.word();
        if (a.equals(cmds.negated)) {
            a = cmd.word();
            if (a.equals("sequence")) {
                tabAuthlstN ntry = new tabAuthlstN();
                ntry.sequence = bits.str2num(cmd.word());
                if (methods.del(ntry)) {
                    cmd.error("invalid sequence");
                    return false;
                }
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            methods.reindex(i, bits.str2num(cmd.word()));
            return false;
        }
        int seq = methods.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        tabAuthlstN ntry = new tabAuthlstN();
        ntry.sequence = seq;
        cfgAuther auth = cfgAll.autherFind(a, null);
        if (auth == null) {
            cmd.error("no such method");
            return false;
        }
        if (auth.name.equals(autName)) {
            cmd.error("loop detected");
            return false;
        }
        ntry.auth = auth.getAuther();
        methods.add(ntry);
        return false;
    }

    public userFormat getShowSpec() {
        userFormat res = new userFormat("|", "aaa|times|ago|last");
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.auth.autName + "|" + ntry.timeout + "|" + bits.timePast(ntry.lastMatch) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.lastMatch + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

    public authResult authUserPass(String user, String pass) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserPass(user, pass);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, pass);
    }

    public authResult authUserCommand(String user, String cmd) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserCommand(user, cmd);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, cmd);
    }

    public authResult acntUserSession(String user, String addr, int sess, counter cntr, int stat) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.acntUserSession(user, addr, sess, cntr, stat);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserChap(user, id, chal, resp);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserApop(String cookie, String user, String resp) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserApop(cookie, user, resp);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, String user) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserPkey(key, user);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserPkey(cryKeyGeneric key, cryHashGeneric algo, String algn, byte[] chal, String user, byte[] resp) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            authResult res = ntry.auth.authUserPkey(key, algo, algn, chal, user, resp);
            if (res == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

    public authResult authUserNone(String user) {
        for (int i = 0; i < methods.size(); i++) {
            tabAuthlstN ntry = methods.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.timeout++;
            ntry.lastMatch = bits.getTime();
            authResult res = ntry.auth.authUserNone(user);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user, "");
    }

}
