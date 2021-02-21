package user;

import cfg.cfgAll;
import cfg.cfgInit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import util.bits;
import util.cmds;
import util.logger;

/**
 * process hw population
 *
 * @author matecsaba
 */
public class userHwpop {

    private cmds orig;

    private boolean apply = false;

    private void doResult(String head, List<String> res) {
        orig.error("apply these for " + head + ":");
        for (int i = 0; i < res.size(); i++) {
            orig.pipe.linePut(res.get(i));
        }
        if (!apply) {
            return;
        }
        orig.error("applying");
        int rep = cfgInit.executeSWcommands(res, false);
        orig.error("errors=" + rep);
    }

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        orig = cmd;
        orig.error("populating forwarding ports");
        String inf = null;
        String mpf = null;
        String srv = null;
        int aut = 0;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("input")) {
                inf = cmd.word();
                continue;
            }
            if (s.equals("map")) {
                mpf = cmd.word();
                continue;
            }
            if (s.equals("auto")) {
                aut = 0;
                s = cmd.word();
                if (s.equals("normal")) {
                    aut = 1;
                }
                if (s.equals("split")) {
                    aut = 2;
                }
                continue;
            }
            if (s.equals("server")) {
                srv = cmd.word();
                srv += " " + cmd.word();
                continue;
            }
            if (s.equals("apply")) {
                apply = true;
                continue;
            }
            if (s.equals("show")) {
                apply = false;
                continue;
            }
        }
        if (inf == null) {
            orig.error("no input specified");
            return;
        }
        if (srv == null) {
            orig.error("no server specified");
            return;
        }
        List<String> txt = bits.txt2buf(inf);
        if (txt == null) {
            orig.error("error reading input");
            return;
        }
        List<String> txt2 = null;
        tabGen<userHwpopPrt> map = new tabGen<userHwpopPrt>();
        if (mpf != null) {
            txt2 = bits.txt2buf(mpf);
        }
        if (txt2 != null) {
            for (int i = 0; i < txt2.size(); i++) {
                cmd = new cmds("hwp", txt2.get(i));
                userHwpopPrt res = new userHwpopPrt();
                res.port = bits.str2num(cmd.word());
                res.pipe = bits.str2num(cmd.word());
                map.add(res);
            }
        }
        orig.error("found " + map.size() + " custom mappings");
        orig.error("will use automap mode " + aut + " otherwise");
        tabGen<userHwpopPrt> ned = new tabGen<userHwpopPrt>();
        for (int i = 0; i < txt.size(); i++) {
            cmd = new cmds("hwp", txt.get(i));
            userHwpopPrt res = new userHwpopPrt();
            res.desc = cmd.word("|");
            cmd.word("|"); // mac
            res.port = bits.str2num(cmd.word("|"));
            res.pipe = bits.str2num(cmd.word("/"));
            res.piPort = bits.str2num(cmd.word("|"));
            String a = cmd.word("|").trim().toLowerCase();
            if (!a.endsWith("g")) {
                continue;
            }
            res.speed = bits.str2num(a.substring(0, a.length() - 1));
            ned.add(res);
        }
        orig.error("found " + ned.size() + " dataplane interfaces");
        if (ned.size() < 1) {
            orig.error("no interfaces found");
            return;
        }
        txt = cfgAll.getShRun(true);
        txt = userFilter.getSection(txt, userReader.filter2reg(srv));
        if (txt.size() < 1) {
            orig.error("no server found");
            return;
        }
        tabGen<userHwpopPrt> fnd = new tabGen<userHwpopPrt>();
        for (int i = 0; i < txt.size(); i++) {
            cmd = new cmds("srv", txt.get(i).trim());
            if (!cmd.word().equals("export-port")) {
                continue;
            }
            userHwpopPrt res = new userHwpopPrt();
            res.desc = cmd.word();
            res.port = bits.str2num(cmd.word());
            res.speed = bits.str2num(cmd.word());
            fnd.add(res);
        }
        orig.error("found " + fnd.size() + " exported interfaces");
        txt2 = new ArrayList<String>();
        txt.clear();
        txt.add("server " + srv);
        for (int i = 0; i < ned.size(); i++) {
            userHwpopPrt ntry = ned.get(i);
            if (fnd.find(ntry) != null) {
                continue;
            }
            int sdn = ntry.port;
            switch (aut) {
                case 1: // normal
                    sdn = bits.str2num(ntry.desc.substring(0, ntry.desc.indexOf("/")));
                    break;
                case 2: // split
                    sdn = bits.str2num(ntry.desc.replaceAll("/", ""));
                    break;
            }
            userHwpopPrt mpd = map.find(ntry);
            if (mpd != null) {
                sdn = mpd.pipe;
            }
            txt2.add("interface sdn" + sdn);
            txt2.add(cmds.tabulator + "description front port " + ntry.desc);
            txt2.add(cmds.tabulator + cmds.finish);
            txt2.add(cmds.comment);
            txt.add(cmds.tabulator + "export-port sdn" + sdn + " " + ntry.port + " " + ntry.speed);
        }
        txt.add(cmds.tabulator + cmds.finish);
        txt2.addAll(txt);
        doResult("new ports to appear", txt2);
        txt2.clear();
        txt.clear();
        txt.add("server " + srv);
        for (int i = 0; i < fnd.size(); i++) {
            userHwpopPrt ntry = fnd.get(i);
            if (ned.find(ntry) != null) {
                continue;
            }
            txt2.add("no interface " + ntry.desc);
            txt.add(cmds.tabulator + "no export-port " + ntry.desc + " " + ntry.port);
        }
        txt.add(cmds.tabulator + cmds.finish);
        txt.addAll(txt2);
        doResult("old ports to disappear", txt);
        txt2.clear();
        txt.clear();
        txt.add("server " + srv);
        for (int i = 0; i < ned.size(); i++) {
            userHwpopPrt ntry = ned.get(i);
            userHwpopPrt old = fnd.find(ntry);
            if (old == null) {
                continue;
            }
            if (ntry.speed == old.speed) {
                continue;
            }
            txt.add(cmds.tabulator + "export-port " + old.desc + " " + ntry.port + " " + ntry.speed);
        }
        txt.add(cmds.tabulator + cmds.finish);
        doResult("updated port speeds", txt);
    }

}

class userHwpopPrt implements Comparator<userHwpopPrt> {

    public int port;

    public int pipe;

    public int piPort;

    public int speed;

    public String desc;

    public int compare(userHwpopPrt o1, userHwpopPrt o2) {
        if (o1.port < o2.port) {
            return -1;
        }
        if (o1.port > o2.port) {
            return +1;
        }
        return 0;
    }

}
