package user;

import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.cmds;

/**
 * process hw population
 *
 * @author matecsaba
 */
public class userHwpop {

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        cmds orig = cmd;
        orig.error("populating forwarding ports");
        String fn = null;
        String srv = null;
        boolean ask = true;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("input")) {
                fn = cmd.word();
                continue;
            }
            if (s.equals("server")) {
                srv = cmd.word();
                srv += " " + cmd.word();
                continue;
            }
            if (s.equals("ask")) {
                ask = true;
                continue;
            }
            if (s.equals("noask")) {
                ask = false;
                continue;
            }
        }
        if (fn == null) {
            orig.error("no input specified");
            return;
        }
        if (srv == null) {
            orig.error("no server specified");
            return;
        }
        List<String> txt = bits.txt2buf(fn);
        if (txt == null) {
            orig.error("error reading input");
            return;
        }
        List<String> ned = new ArrayList<String>();
        for (int i = 0; i < txt.size(); i++) {
            cmd = new cmds("hwp", txt.get(i));
            String prt = cmd.word("|");
            cmd.word("|"); // mac
            int dpp = bits.str2num(cmd.word("|"));
            int pip = bits.str2num(cmd.word("/"));
            int ppt = bits.str2num(cmd.word("|"));
            String spd = cmd.word("|").trim().toLowerCase();
            if (!spd.endsWith("g")) {
                continue;
            }
            spd = spd.substring(0, spd.length() - 1);
            util.logger.debug("here " + prt + " " + dpp + " " + pip + " " + ppt + " " + spd);/////
        }
    }

}
