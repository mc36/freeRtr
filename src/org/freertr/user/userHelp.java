package org.freertr.user;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgCert;
import org.freertr.cfg.cfgChat;
import org.freertr.cfg.cfgCheck;
import org.freertr.cfg.cfgEvntmgr;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgIpsec;
import org.freertr.cfg.cfgKey;
import org.freertr.cfg.cfgLin;
import org.freertr.cfg.cfgMenuK;
import org.freertr.cfg.cfgMenuT;
import org.freertr.cfg.cfgMtrack;
import org.freertr.cfg.cfgObjnet;
import org.freertr.cfg.cfgObjprt;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgPool;
import org.freertr.cfg.cfgPrcss;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgSched;
import org.freertr.cfg.cfgScrpt;
import org.freertr.cfg.cfgSensor;
import org.freertr.cfg.cfgSessn;
import org.freertr.cfg.cfgTime;
import org.freertr.cfg.cfgTlmtry;
import org.freertr.cfg.cfgTrack;
import org.freertr.cfg.cfgTrnsltn;
import org.freertr.cfg.cfgVdc;
import org.freertr.cfg.cfgVpdn;
import org.freertr.cfg.cfgVrf;
import org.freertr.cfg.cfgXconn;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.enc.encXml;
import org.freertr.util.version;

/**
 * help system
 *
 * @author matecsaba
 */
public class userHelp {

    /**
     * create instance
     */
    public userHelp() {
    }

    private final static int maxVal = 0x100000;

    /**
     * lines of data
     */
    protected List<userHelpData> lines = new ArrayList<userHelpData>();

    /**
     * need to expand lists
     */
    public boolean expand;

    /**
     * get generic config help
     *
     * @param l help text
     */
    protected static void getCfgGen(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, cmds.finish, "go back to previous mode");
        l.add(null, false, 1, new int[]{2, -1}, "end", "close this config session");
        l.add(null, false, 2, new int[]{2, -1}, "<cmd>", "parameters");
        l.add(null, false, 1, new int[]{2}, "do", "execute one exec command");
        l.add(null, false, 2, new int[]{2, -1}, "<cmd>", "exec command");
        l.add(null, false, 1, new int[]{1}, cmds.negated, "negate a command");
    }

    /**
     * collect lines starts with
     *
     * @param l list to read
     * @param s starting string
     * @return list of lines starting with s
     */
    public static List<String> startsWith(List<String> l, String s) {
        s = s.toLowerCase();
        List<String> r = new ArrayList<String>();
        for (int i = 0; i < l.size(); i++) {
            String a = l.get(i);
            if (a.toLowerCase().startsWith(s)) {
                r.add(a);
            }
        }
        return r;
    }

    /**
     * get size of menu lines
     *
     * @return number of lines
     */
    public int size() {
        return lines.size();
    }

    /**
     * get one menu line
     *
     * @param i sequence to get
     * @return value of line
     */
    public userHelpData get(int i) {
        if (i < 0) {
            return null;
        }
        if (i >= lines.size()) {
            return null;
        }
        return lines.get(i);
    }

    /**
     * add one menu line
     *
     * @param loc local list
     * @param exp experimental command
     * @param cur current level
     * @param nxt next level, -1=end, -2=hidden
     * @param cmd command word
     * @param dsc description
     */
    public void add(List<String> loc, boolean exp, int cur, int[] nxt, String cmd, String dsc) {
        userHelpData d = new userHelpData(cur, nxt, cmd, dsc);
        d.variable = (cmd.indexOf("<") == 0) || (cmd.indexOf("[") == 0);
        if (exp) {
            d.description = "!!!EXPERiMENTAL!!! " + dsc;
            if (!cfgAll.buggy) {
                d.level = 666;
            }
            if (cfgAll.evalVdcPrivs()) {
                d.level = 666;
            }
        }
        lines.add(d);
        if (!expand) {
            return;
        }
        int i = d.command.indexOf(":");
        if (i < 0) {
            return;
        }
        String a = d.command.substring(i + 1, d.command.length());
        String s = d.command.substring(0, i);
        if (d.variable) {
            a = a.substring(0, a.length() - 1);
            i = d.command.length();
            s += d.command.subSequence(i - 1, i);
        }
        d.command = s;
        d = d.copyBytes();
        d.complete = true;
        d.variable = false;
        if (a.equals("ifc")) {
            for (i = 0; i < cfgAll.ifaces.size(); i++) {
                cfgIfc ntry = cfgAll.ifaces.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("lin")) {
            for (i = 0; i < cfgAll.lines.size(); i++) {
                cfgLin ntry = cfgAll.lines.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("vrf")) {
            for (i = 0; i < cfgAll.vrfs.size(); i++) {
                cfgVrf ntry = cfgAll.vrfs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("rm")) {
            for (i = 0; i < cfgAll.routemaps.size(); i++) {
                cfgRoump ntry = cfgAll.routemaps.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("rpl")) {
            for (i = 0; i < cfgAll.routeplcs.size(); i++) {
                cfgRouplc ntry = cfgAll.routeplcs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("pl")) {
            for (i = 0; i < cfgAll.prefixlsts.size(); i++) {
                cfgPrfxlst ntry = cfgAll.prefixlsts.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("ogn")) {
            for (i = 0; i < cfgAll.objgrpnets.size(); i++) {
                cfgObjnet ntry = cfgAll.objgrpnets.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("ogp")) {
            for (i = 0; i < cfgAll.objgrpprts.size(); i++) {
                cfgObjprt ntry = cfgAll.objgrpprts.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("acl")) {
            for (i = 0; i < cfgAll.accesslsts.size(); i++) {
                cfgAceslst ntry = cfgAll.accesslsts.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("pm")) {
            for (i = 0; i < cfgAll.policymaps.size(); i++) {
                cfgPlymp ntry = cfgAll.policymaps.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("tm")) {
            for (i = 0; i < cfgAll.timemaps.size(); i++) {
                cfgTime ntry = cfgAll.timemaps.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("ips")) {
            for (i = 0; i < cfgAll.ipsecs.size(); i++) {
                cfgIpsec ntry = cfgAll.ipsecs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("rsa")) {
            for (i = 0; i < cfgAll.rsakeys.size(); i++) {
                cfgKey<cryKeyRSA> ntry = cfgAll.rsakeys.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("dsa")) {
            for (i = 0; i < cfgAll.dsakeys.size(); i++) {
                cfgKey<cryKeyDSA> ntry = cfgAll.dsakeys.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("ecd")) {
            for (i = 0; i < cfgAll.ecdsakeys.size(); i++) {
                cfgKey<cryKeyECDSA> ntry = cfgAll.ecdsakeys.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("mld")) {
            for (i = 0; i < cfgAll.mldsakeys.size(); i++) {
                cfgKey<cryKeyMLDSA> ntry = cfgAll.mldsakeys.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("crt")) {
            for (i = 0; i < cfgAll.certs.size(); i++) {
                cfgCert ntry = cfgAll.certs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("ses")) {
            for (i = 0; i < cfgAll.sessns.size(); i++) {
                cfgSessn ntry = cfgAll.sessns.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("chk")) {
            for (i = 0; i < cfgAll.checks.size(); i++) {
                cfgCheck ntry = cfgAll.checks.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("sns")) {
            for (i = 0; i < cfgAll.sensors.size(); i++) {
                cfgSensor ntry = cfgAll.sensors.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("tlm")) {
            for (i = 0; i < cfgAll.tlmtrydst.size(); i++) {
                cfgTlmtry ntry = cfgAll.tlmtrydst.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("eem")) {
            for (i = 0; i < cfgAll.eventmgrs.size(); i++) {
                cfgEvntmgr ntry = cfgAll.eventmgrs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("prx")) {
            for (i = 0; i < cfgAll.proxys.size(); i++) {
                cfgProxy ntry = cfgAll.proxys.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("cht")) {
            for (i = 0; i < cfgAll.chats.size(); i++) {
                cfgChat ntry = cfgAll.chats.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("mnk")) {
            for (i = 0; i < cfgAll.menuk.size(); i++) {
                cfgMenuK ntry = cfgAll.menuk.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("mnt")) {
            for (i = 0; i < cfgAll.menut.size(); i++) {
                cfgMenuT ntry = cfgAll.menut.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("sch")) {
            for (i = 0; i < cfgAll.schedulers.size(); i++) {
                cfgSched ntry = cfgAll.schedulers.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("pl4")) {
            for (i = 0; i < cfgAll.ip4pool.size(); i++) {
                cfgPool<addrIPv4> ntry = cfgAll.ip4pool.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("pl6")) {
            for (i = 0; i < cfgAll.ip4pool.size(); i++) {
                cfgPool<addrIPv6> ntry = cfgAll.ip6pool.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("scr")) {
            for (i = 0; i < cfgAll.scripts.size(); i++) {
                cfgScrpt ntry = cfgAll.scripts.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("vpd")) {
            for (i = 0; i < cfgAll.vpdns.size(); i++) {
                cfgVpdn ntry = cfgAll.vpdns.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("xcn")) {
            for (i = 0; i < cfgAll.xconnects.size(); i++) {
                cfgXconn ntry = cfgAll.xconnects.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("vdc")) {
            for (i = 0; i < cfgAll.vdcs.size(); i++) {
                cfgVdc ntry = cfgAll.vdcs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("prc")) {
            for (i = 0; i < cfgAll.prcs.size(); i++) {
                cfgPrcss ntry = cfgAll.prcs.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("trn")) {
            for (i = 0; i < cfgAll.trnsltns.size(); i++) {
                cfgTrnsltn ntry = cfgAll.trnsltns.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("trk")) {
            for (i = 0; i < cfgAll.trackers.size(); i++) {
                cfgTrack ntry = cfgAll.trackers.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("mtr")) {
            for (i = 0; i < cfgAll.mtrackers.size(); i++) {
                cfgMtrack ntry = cfgAll.mtrackers.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("aaa")) {
            for (i = 0; i < cfgAll.authers.size(); i++) {
                cfgAuther ntry = cfgAll.authers.get(i);
                if (ntry == null) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = ntry.name;
                lines.add(res);
            }
            return;
        }
        if (a.equals("rtr")) {
            List<Integer> nums = new ArrayList<Integer>();
            for (i = 0; i < cfgAll.routers.size(); i++) {
                cfgRtr ntry = cfgAll.routers.get(i);
                if (ntry == null) {
                    continue;
                }
                nums.add(ntry.number);
            }
            Collections.sort(nums);
            int o = -1;
            for (i = 0; i < nums.size(); i++) {
                int p = nums.get(i);
                if (o == p) {
                    continue;
                }
                userHelpData res = d.copyBytes();
                res.command = "" + p;
                lines.add(res);
                o = p;
            }
            return;
        }
        if (a.equals("loc")) {
            if (loc == null) {
                return;
            }
            for (i = 0; i < loc.size(); i++) {
                userHelpData res = d.copyBytes();
                res.command = loc.get(i);
                lines.add(res);
            }
            return;
        }
    }

    /**
     * get lines from an other help
     *
     * @param src source
     */
    public void addOther(userHelp src) {
        lines.addAll(src.lines);
    }

    /**
     * collect menu lines in one direction
     *
     * @param d list to update
     * @param lin starting line number
     * @param skip skip current level
     * @param dir direction, + or - 1
     * @param levMin minimum level to scan
     * @param levMax maximum level to scan
     * @param levReq required level to collect
     */
    private void collectDirection(userHelpList d, int lin, boolean skip, int dir, int levMin, int levMax, int levReq) {
        if (skip) {
            int cur = lines.get(lin).level;
            for (;; lin += dir) {
                if (lin >= lines.size()) {
                    return;
                }
                if (lin < 0) {
                    return;
                }
                if (lines.get(lin).level != cur) {
                    break;
                }
            }
        }
        for (;; lin += dir) {
            if (lin >= lines.size()) {
                return;
            }
            if (lin < 0) {
                return;
            }
            int lv = lines.get(lin).level;
            if (lv == levReq) {
                d.add(lin);
                continue;
            }
            if (lv < levMin) {
                return;
            }
            if (lv > levMax) {
                return;
            }
        }
    }

    /**
     * find possible next words for a line
     *
     * @param lin line number to process
     * @return list of possible next lines
     */
    private userHelpList nextWords(int lin) {
        userHelpList d = new userHelpList();
        if (lin < 0) {
            collectDirection(d, 0, false, 1, -maxVal, maxVal, 1);
            return d;
        }
        userHelpData curNxt = lines.get(lin);
        for (int i = 0; i < curNxt.num(); i++) {
            int req = curNxt.val(i);
            if (req < 0) {
                d.add(req);
                continue;
            }
            if (req > 100) {
                collectDirection(d, lin, false, -1, 0, maxVal, req);
                collectDirection(d, lin, false, 1, 0, maxVal, req);
                continue;
            }
            if (req > curNxt.level) {
                collectDirection(d, lin, true, 1, curNxt.level + 1, maxVal, req);
                continue;
            }
            collectDirection(d, lin, false, -1, req, maxVal, req);
            collectDirection(d, lin + 1, false, 1, req, maxVal, req);
        }
        return d;
    }

    /**
     * match one string to given lines
     *
     * @param lns list of lines
     * @param s string to match
     * @param ncm consider completes
     * @return match probability for lines level set to best one or a negative
     * error code of the following: -2 more best matches, have to type more
     * chars -3 string does not match to any of lines -4 generic error
     */
    private userHelpList matchStr(userHelpList lns, String s, boolean ncm) {
        byte[] b1 = s.trim().toLowerCase().getBytes();
        userHelpList d = new userHelpList();
        for (int o = 0; o < lns.num(); o++) {
            int i = lns.val(o);
            if (i < 0) {
                d.add(0);
                continue;
            }
            userHelpData dat = lines.get(i);
            if (ncm && dat.complete) {
                d.add(-2);
                continue;
            }
            if (dat.variable) {
                d.add(-1);
                continue;
            }
            byte[] b2 = dat.command.toLowerCase().getBytes();
            for (i = 0;; i++) {
                if (i >= b1.length) {
                    break;
                }
                if (i >= b2.length) {
                    break;
                }
                if (b1[i] != b2[i]) {
                    break;
                }
            }
            d.add(i);
        }
        int num = -4;
        int max = -1;
        int vld = -1;
        for (int i = 0; i < d.num(); i++) {
            int cur = d.val(i);
            if (cur == -1) {
                vld = i;
                continue;
            }
            if (cur < max) {
                continue;
            }
            if (cur == max) {
                num = -2;
                continue;
            }
            max = cur;
            num = i;
        }
        if (max != b1.length) {
            num = -3;
        }
        if ((num < 0) && (vld >= 0)) {
            num = vld;
        }
        d.level = num;
        return d;
    }

    /**
     * find longest matching string
     *
     * @param lns list of lines
     * @param s string to match
     * @return string
     */
    private String matchLong(userHelpList lns, String s) {
        s = s.trim().toLowerCase();
        String m = null;
        for (int o = 0; o < lns.num(); o++) {
            int i = lns.val(o);
            if (i < 0) {
                continue;
            }
            userHelpData dat = lines.get(i);
            if (dat.variable) {
                continue;
            }
            if (!dat.command.toLowerCase().startsWith(s)) {
                continue;
            }
            if (m == null) {
                m = dat.command;
                continue;
            }
            String a = dat.command;
            for (i = 0;; i++) {
                if (i >= m.length()) {
                    break;
                }
                if (i >= a.length()) {
                    break;
                }
                if (a.substring(i, i + 1).toLowerCase().equals(m.substring(i, i + 1).toLowerCase())) {
                    continue;
                }
                m = a.substring(0, i);
                break;
            }
        }
        return m;
    }

    /**
     * create a list of lines to a command string
     *
     * @param s command string to process
     * @param ext consider extensions
     * @return line numbers over wich i get to the command level set to a last
     * one, or error from matchStr
     */
    private userHelpList whereAm(String s, boolean ext) {
        s = s.trim();
        userHelpList res = new userHelpList();
        res.level = -1;
        String b = "";
        for (; s.length() > 0;) {
            userHelpList lns = nextWords(res.level);
            int i = s.indexOf(" ");
            String a;
            if (i < 0) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i).trim();
                s = s.substring(i, s.length()).trim();
            }
            userHelpList sel = matchStr(lns, a, !ext);
            if (sel.level < 0) {
                res.level = sel.level;
                if (s.length() > 0) {
                    return res;
                }
                s = matchLong(lns, a);
                if (s == null) {
                    return res;
                }
                res.str = b + s;
                return res;
            }
            i = lns.val(sel.level);
            res.add(i);
            if (lines.get(i).variable) {
                String c = null;
                if (ext) {
                    c = matchLong(lns, a);
                }
                if (c == null) {
                    b += a + " ";
                } else {
                    if ((c.length() >= a.length()) && (s.length() < 1)) {
                        b += c;
                    } else {
                        b += a + " ";
                    }
                }
            } else {
                b += lines.get(i).command + " ";
            }
            res.level = i;
        }
        res.str = b;
        return res;
    }

    /**
     * repair commands in one line
     *
     * @param a string to process
     * @return repaired line, variables preserved, empty if no guess
     */
    public String repairLine(String a) {
        if (a == null) {
            return "";
        }
        userHelpList d = whereAm(a, false);
        if (d.level < 0) {
            return "";
        }
        return d.str;
    }

    /**
     * guess commands in one line
     *
     * @param a string to process
     * @return guessed line, variables preserved, null if no guess
     */
    public String guessLine(String a) {
        userHelpList d = whereAm(a, true);
        return d.str;
    }

    /**
     * test if at end of a valid command
     *
     * @param s command to test
     * @return false if valid command, true if an invalid command
     */
    public boolean endOfCmd(String s) {
        userHelpList d = whereAm(s, false);
        if (d.level < 0) {
            return true;
        }
        d = nextWords(d.level);
        for (int i = 0; i < d.num(); i++) {
            if (d.val(i) == -1) {
                return false;
            }
        }
        return true;
    }

    /**
     * format help text to user displayable format for a line
     *
     * @param lin line number to generate help for
     * @return array of strings that user should read
     */
    private List<String> formatHelp(int lin) {
        final String begin = "  ";
        final String enter = "<cr>";
        userHelpList d = nextWords(lin);
        List<String> s = new ArrayList<String>();
        int max = -1;
        for (int i = 0; i < d.num(); i++) {
            int o = d.val(i);
            if (o < 0) {
                o = enter.length();
            } else {
                o = lines.get(o).command.length();
            }
            if (o > max) {
                max = o;
            }
        }
        for (int i = 0; i < d.num(); i++) {
            int o = d.val(i);
            if (o < 0) {
                s.add(begin + enter);
                continue;
            }
            userHelpData cur = lines.get(o);
            s.add(begin + bits.padEnd(cur.command, max, " ") + " - " + cur.description);
        }
        return s;
    }

    /**
     * find usage of a line
     *
     * @param dat data to append
     * @param beg beginning
     * @param hlp help text
     * @param lin line to get
     * @param lev current level
     * @return found entries
     */
    protected int formatUsage(userHelp dat, String beg, String hlp, int lin, int lev) {
        int fnd = 0;
        for (int o = lin; o < lines.size(); o++) {
            userHelpData ntry = lines.get(o);
            if ((fnd > 0) && (ntry.level < lev)) {
                break;
            }
            if (ntry.level != lev) {
                continue;
            }
            fnd++;
            int q = 0;
            boolean b = false;
            for (int i = 0; i < ntry.num(); i++) {
                int p = ntry.val(i);
                if (p < 0) {
                    b = true;
                    continue;
                }
                if (p <= lev) {
                    continue;
                }
                q += formatUsage(dat, beg + " " + ntry.command, hlp + ", " + ntry.description, o + 1, p);
            }
            if ((!b) && (q > 0)) {
                continue;
            }
            String d = hlp + ", " + ntry.description;
            d = d.substring(2, d.length());
            String c = beg + " " + ntry.command;
            c = c.substring(1, c.length());
            dat.lines.add(new userHelpData(1, new int[]{}, c, d));
        }
        return fnd;
    }

    /**
     * find usage of a line
     *
     * @param dat data to append
     * @param id indent string
     * @param lin line to get
     * @param lev current level
     */
    protected void formatYang(List<String> dat, String id, int lin, int lev) {
        int fnd = 0;
        for (int o = lin; o < lines.size(); o++) {
            userHelpData ntry = lines.get(o);
            if ((fnd > 0) && (ntry.level < lev)) {
                break;
            }
            if (ntry.level != lev) {
                continue;
            }
            fnd++;
            List<Integer> nxt = new ArrayList<Integer>();
            boolean b = false;
            for (int i = 0; i < ntry.num(); i++) {
                int p = ntry.val(i);
                if (p < 0) {
                    b = true;
                    continue;
                }
                if (p <= lev) {
                    continue;
                }
                nxt.add(p);
            }
            String vn = ntry.command;
            if (ntry.variable) {
                vn = vn.substring(1, vn.length() - 1);
            }
            if (nxt.size() < 1) {
                if (ntry.variable) {
                    dat.add(id + "leaf " + encXml.content + vn + " {");
                    dat.add(id + "  description \"" + ntry.description + "\";");
                    dat.add(id + "  type string;");
                    dat.add(id + "}");
                    continue;
                }
                dat.add(id + "leaf " + vn + " {");
                dat.add(id + "  description \"" + ntry.description + "\";");
                dat.add(id + "  type empty;");
                dat.add(id + "}");
                continue;
            }
            if (!ntry.variable) {
                dat.add(id + "container " + vn + " {");
                dat.add(id + "  description \"" + ntry.description + "\";");
                if (b) {
                    dat.add(id + "  leaf " + encXml.ignore + " {");
                    dat.add(id + "    description \"" + ntry.description + "\";");
                    dat.add(id + "    type empty;");
                    dat.add(id + "  }");
                }
                for (int i = 0; i < nxt.size(); i++) {
                    formatYang(dat, id + "  ", o + 1, nxt.get(i));
                }
                dat.add(id + "}");
                continue;
            }
            dat.add(id + "list " + encXml.ignore + o + " {");
            dat.add(id + "  key \"" + encXml.content + o + "\";");
            dat.add(id + "  leaf " + encXml.content + o + " {");
            dat.add(id + "    description \"" + ntry.description + "\";");
            dat.add(id + "    type string;");
            dat.add(id + "  }");
            if (b) {
                dat.add(id + "  leaf " + encXml.ignore + " {");
                dat.add(id + "    description \"" + ntry.description + "\";");
                dat.add(id + "    type empty;");
                dat.add(id + "  }");
            }
            for (int i = 0; i < nxt.size(); i++) {
                formatYang(dat, id + "  ", o + 1, nxt.get(i));
            }
            dat.add(id + "}");
        }
    }

    /**
     * get help text for a given command line
     *
     * @param s the command line
     * @param oneLine set true to return only one line response
     * @return array of strings that user should read
     */
    public List<String> getHelp(String s, boolean oneLine) {
        userHelpList d = whereAm(s, true);
        String cmd = "";
        for (int i = 0; i < d.num(); i++) {
            cmd += " " + lines.get(d.val(i)).command;
        }
        if (cmd.length() > 0) {
            cmd = cmd.substring(1, cmd.length());
        }
        if (d.level == -2) {
            d = whereAm(cmd, true);
            s = s.trim();
            int i = s.lastIndexOf(" ") + 1;
            if (i < 1) {
                i = 0;
            }
            s = s.substring(i, s.length());
            if (oneLine) {
                return getBarkBack("ambigous command, try " + s + "?");
            }
            List<String> lst = formatHelp(d.level);
            lst = startsWith(lst, "  " + s);
            return lst;
        }
        if (d.level >= -1) {
            if (oneLine) {
                return getBarkBack("incomplete command, try " + cmd + " ?");
            }
            if (d.level == -1) {
                return formatHelp(d.level);
            }
            if (s.lastIndexOf(" ") >= s.length() - 1) {
                return formatHelp(d.level);
            }
            userHelpData r = lines.get(d.level);
            if (!r.variable) {
                return getBarkBack("type " + r.command + " to " + r.description);
            }
            s = s.trim();
            int i = s.lastIndexOf(" ") + 1;
            if (i < 1) {
                return getBarkBack("type " + r.command + " to " + r.description);
            }
            d = whereAm(s.substring(0, i - 1), true);
            s = s.substring(i, s.length());
            List<String> lst = formatHelp(d.level);
            lst = startsWith(lst, "  " + s);
            if (lst.size() <= 1) {
                return getBarkBack("type " + r.command + " to " + r.description);
            }
            return lst;
        }
        return getBarkBack("invalid command, try " + cmd + " ?");
    }

    private List<String> getBarkBack(String s) {
        if (cfgAll.clientShamer == null) {
            return bits.str2lst(s);
        }
        return bits.str2lst(cfgAll.clientShamer.getOneLine() + " (" + s + ")");
    }

    /**
     * get usage text for a given command
     *
     * @return usage text
     */
    public List<String> getUsage() {
        userHelp d = new userHelp();
        formatUsage(d, "", "", 0, 1);
        return d.formatHelp(-1);
    }

    /**
     * get yang for a given command
     *
     * @param path path
     * @param prefix prefix
     * @return yang
     */
    public List<String> getYang(String path, String prefix) {
        List<String> res = new ArrayList<String>();
        res.add("module " + prefix + " {");
        res.add("  namespace \"" + version.homeUrl + "yang/" + prefix + "\";");
        res.add("  prefix \"" + prefix + "\";");
        cmds cp = new cmds("ya", path);
        String id = "  ";
        for (;;) {
            if (cp.size() < 1) {
                break;
            }
            String a = cp.word("/");
            a = encXml.escId(a);
            res.add(id + "container " + a + " {");
            id += "  ";
        }
        formatYang(res, id, 0, 1);
        for (; id.length() > 0;) {
            id = id.substring(0, id.length() - 2);
            res.add(id + "}");
        }
        return res;
    }

    /**
     * get list of commands
     *
     * @return list of commands
     */
    public List<String> getList() {
        List<String> s = new ArrayList<String>();
        String[] l = new String[64];
        for (int i = 0; i < l.length; i++) {
            l[i] = "";
        }
        for (int o = 0; o < lines.size(); o++) {
            userHelpData ntry = lines.get(o);
            l[ntry.level] = "" + ntry.command;
            String a = "";
            for (int i = 0; i <= ntry.level; i++) {
                a = a + l[i] + " ";
            }
            s.add(a.trim());
        }
        return s;
    }

    /**
     * add possibility to lines
     *
     * @param lvl source level to add
     * @param nxt next level to add
     */
    public void possible(int lvl, int nxt) {
        for (int i = 0; i < lines.size(); i++) {
            lines.get(i).possible(lvl, nxt);
        }
    }

}

class userHelpList {

    private List<Integer> data = new ArrayList<Integer>(); // values

    protected int level; // which level we are on

    protected String str; // repaired line

    public int num() {
        return data.size();
    }

    public int val(int i) {
        return data.get(i);
    }

    public void add(int i) {
        data.add(i);
    }

}

class userHelpData {

    private int[] data; // values

    protected int level; // which level we are on

    protected String command; // the command

    protected String description; // the description

    protected boolean variable; // variable or fixed

    protected boolean complete; // complete variable

    public userHelpData(int l, int[] n, String c, String d) {
        level = l;
        data = n;
        command = c;
        description = d;
    }

    public userHelpData copyBytes() {
        int[] res = new int[data.length];
        for (int i = 0; i < data.length; i++) {
            res[i] = data[i];
        }
        userHelpData n = new userHelpData(level, res, command, description);
        n.complete = complete;
        n.variable = variable;
        return n;
    }

    public int num() {
        return data.length;
    }

    public int val(int i) {
        return data[i];
    }

    public void possible(int lvl, int nxt) {
        for (int o = 0; o < data.length; o++) {
            if (data[o] != lvl) {
                continue;
            }
            int[] res = new int[data.length + 1];
            for (int i = 0; i < data.length; i++) {
                res[i] = data[i];
            }
            res[data.length] = nxt;
            data = res;
            return;
        }
    }

}
