package org.freertr.user;

import java.util.List;
import org.freertr.cfg.cfgInit;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * process hw redetection
 *
 * @author matecsaba
 */
public class userHwred {

    /**
     * create instance
     */
    public userHwred() {
    }

    private String path = "./";

    private String pref = "./rtr-";

    private String hwdn = "hwdet-all.sh";

    private String macs = "hwdet.eth";

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        cmd.error("redetecting hardware");
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("path")) {
                path = cmd.word();
                continue;
            }
            if (s.equals("eth")) {
                macs = cmd.word();
                continue;
            }
        }
        List<String> hwd = bits.txt2buf(path + hwdn);
        if (hwd == null) {
            cmd.error("error reading " + hwdn);
            return;
        }
        List<String> det = bits.txt2buf(path + macs);
        if (det == null) {
            cmd.error("error reading " + macs);
            return;
        }
        List<String> hwc = bits.txt2buf(path + pref + cfgInit.hwCfgEnd);
        if (hwc == null) {
            cmd.error("error reading hw config");
            return;
        }
        tabGen<userHwifc> cur = new tabGen<userHwifc>();
        for (int i = 0; i < det.size(); i++) {
            userHwifc ntry = userHwifc.fromRaw(det, i);
            if (ntry == null) {
                continue;
            }
            cur.add(ntry);
        }
        int o = hwd.indexOf("### macs ###");
        if (o < 0) {
            cmd.error("error splitting " + hwdn);
            return;
        }
        tabGen<userHwifc> old = new tabGen<userHwifc>();
        for (int i = o; i < hwd.size(); i++) {
            String a = hwd.get(i);
            if (a.length() < 1) {
                break;
            }
            userHwifc ntry = userHwifc.fromOwn(a);
            if (ntry == null) {
                continue;
            }
            old.add(ntry);
        }
        tabGen<userHwifc> rep1 = new tabGen<userHwifc>();
        tabGen<userHwifc> rep2 = new tabGen<userHwifc>();
        tabGen<userHwifc> rep3 = new tabGen<userHwifc>();
        for (int i = 0; i < cur.size(); i++) {
            userHwifc ntry = cur.get(i);
            userHwifc prev = findMac(old, ntry);
            if (prev == null) {
                continue;
            }
            if (ntry.name.equals(prev.name)) {
                continue;
            }
            userHwifc res1 = new userHwifc();
            userHwifc res2 = new userHwifc();
            userHwifc res3 = new userHwifc();
            res1.name = " " + prev.name + " ";
            res1.mac = " " + ntry.name + " ";
            rep1.add(res1);
            res2.name = res1.name;
            res2.mac = " placeholder-" + i + "-interface ";
            rep2.add(res2);
            res3.name = res2.mac;
            res3.mac = res1.mac;
            rep3.add(res3);
        }
        cmd.error("detected=" + cur.size() + " needed=" + old.size() + " changed=" + rep1.size());
        if (rep1.size() < 1) {
            return;
        }
        for (int i = 0; i < rep1.size(); i++) {
            cmd.error(rep1.get(i) + " - " + rep2.get(i) + " - " + rep3.get(i));
        }
        replaceText(hwd, rep2);
        replaceText(hwc, rep2);
        replaceText(hwd, rep3);
        replaceText(hwc, rep3);
        if (bits.buf2txt(true, hwc, path + pref + cfgInit.hwCfgEnd)) {
            cmd.error("error saving hw config");
            return;
        }
        if (bits.buf2txt(true, hwd, path + hwdn)) {
            cmd.error("error saving " + hwdn);
            return;
        }
        cfgInit.stopRouter(true, 20, "hardware changed");
    }

    private userHwifc findMac(tabGen<userHwifc> lst, userHwifc old) {
        for (int i = 0; i < lst.size(); i++) {
            userHwifc ntry = lst.get(i);
            if (!old.mac.equals(ntry.mac)) {
                continue;
            }
            return ntry;
        }
        return null;
    }

    private void replaceText(List<String> txt, tabGen<userHwifc> rep) {
        for (int i = 0; i < txt.size(); i++) {
            String s = txt.get(i);
            s = replaceText(s, rep);
            txt.set(i, s);
        }
    }

    private String replaceText(String txt, tabGen<userHwifc> rep) {
        txt = " " + txt + " ";
        for (int o = 0; o < rep.size(); o++) {
            userHwifc ntry = rep.get(o);
            int i = txt.indexOf(ntry.name);
            if (i < 0) {
                continue;
            }
            txt = txt.substring(0, i) + ntry.mac + txt.substring(i + ntry.name.length(), txt.length());
        }
        return txt.substring(1, txt.length() - 1);
    }

}
