package auth;

import cfg.cfgAll;
import cfg.cfgAuther;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * list of methods
 *
 * @author matecsaba
 */
public class authList extends authGeneric {

    private tabGen<authListEntry> lst = new tabGen<authListEntry>();

    public void getHelp(userHelping l) {
        l.add("1 2  sequence            select sequence number");
        l.add("2 3    <num>             number of entry");
        l.add("3 .      <name>          name of authenticator");
    }

    public List<String> getShRun(String beg) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < lst.size(); i++) {
            authListEntry ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(beg + ntry);
        }
        return l;
    }

    public String getCfgName() {
        return "list";
    }

    public boolean fromString(cmds cmd) {
        String s = cmd.word();
        if (s.equals("sequence")) {
            int seq = bits.str2num(cmd.word());
            cfgAuther auth = cfgAll.autherFind(cmd.word(), null);
            if (auth == null) {
                return false;
            }
            if (auth.name.equals(autName)) {
                return false;
            }
            authListEntry ntry = new authListEntry();
            ntry.seq = seq;
            ntry.auth = auth.getAuther();
            lst.put(ntry);
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("sequence")) {
            int seq = bits.str2num(cmd.word());
            authListEntry ntry = new authListEntry();
            ntry.seq = seq;
            lst.del(ntry);
            return false;
        }
        return true;
    }

    public authResult authUserPass(String user, String pass) {
        for (int i = 0; i < lst.size(); i++) {
            authListEntry ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            authResult res = ntry.auth.authUserPass(user, pass);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user);
    }

    public authResult authUserCommand(String user, String cmd) {
        for (int i = 0; i < lst.size(); i++) {
            authListEntry ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            authResult res = ntry.auth.authUserCommand(user, cmd);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user);
    }

    public authResult authUserChap(String user, int id, byte[] chal, byte[] resp) {
        for (int i = 0; i < lst.size(); i++) {
            authListEntry ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            authResult res = ntry.auth.authUserChap(user, id, chal, resp);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user);
    }

    public authResult authUserApop(String cookie, String user, String resp) {
        for (int i = 0; i < lst.size(); i++) {
            authListEntry ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            authResult res = ntry.auth.authUserApop(cookie, user, resp);
            if (res == null) {
                continue;
            }
            if (res.result == authResult.authServerError) {
                continue;
            }
            return res;
        }
        return new authResult(this, authResult.authServerError, user);
    }

}

class authListEntry implements Comparator<authListEntry> {

    public int seq;

    public authGeneric auth;

    public String toString() {
        return "sequence " + seq + " " + auth.autName;
    }

    public int compare(authListEntry o1, authListEntry o2) {
        if (o1.seq < o2.seq) {
            return -1;
        }
        if (o1.seq > o2.seq) {
            return +1;
        }
        return 0;
    }

}
