package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryUtils;
import net.freertr.pipe.pipeShell;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * process image creation
 *
 * @author matecsaba
 */
public class userImage {

    /**
     * create instance
     */
    public userImage() {
    }

    private pipeSide pip;

    private String tempDir = "../binDsk";

    private String downDir = "../binDwn";

    private String imgName = "../binImg/rtr";

    private String found = "";

    private int downMode = 1;

    private long regeTim = bits.getTime() - Integer.MAX_VALUE;

    private String arch = "amd64";

    private tabGen<userImageCat> catalogs = new tabGen<userImageCat>();

    private tabGen<userImagePkg> allPkgs = new tabGen<userImagePkg>();

    private tabGen<userImagePkg> missing = new tabGen<userImagePkg>();

    private tabGen<userImagePkg> selected = new tabGen<userImagePkg>();

    private tabGen<userImagePkg> forbidden = new tabGen<userImagePkg>();

    private tabGen<userImagePkg> discarded = new tabGen<userImagePkg>();

    private userImagePkg matchReg(tabGen<userImagePkg> lst, String a) {
        for (int i = 0; i < lst.size(); i++) {
            userImagePkg pkg = lst.get(i);
            if (a.matches(pkg.name)) {
                return pkg;
            }
        }
        return null;
    }

    private void filterCat(tabGen<userImagePkg> trg, userImageCat cat, tabGen<userImagePkg> src) {
        trg.clear();
        for (int i = 0; i < src.size(); i++) {
            userImagePkg cur = src.get(i);
            if (cur.cat != cat) {
                continue;
            }
            trg.add(cur);
        }
    }

    private String dumpList(tabGen<userImagePkg> lst, boolean detail) {
        String s = "";
        long o = 0;
        for (int i = 0; i < lst.size(); i++) {
            userImagePkg pkg = lst.get(i);
            o += pkg.size;
            if (!detail) {
                continue;
            }
            s += " " + pkg.name;
        }
        if (detail) {
            s += " -";
        }
        s += " " + (o / 1024) + " kb in " + lst.size() + " packages";
        return s;
    }

    private String getPackageName(userImagePkg pkg) {
        return downDir + "/" + arch + "-" + pkg.name + ".deb";
    }

    private int execCmd(String s) {
        pip.linePut("!" + s + ".");
        pipeShell sh = pipeShell.exec(pip, "sh -c", s, false, true, false);
        sh.waitFor();
        return sh.resultNum();
    }

    private boolean delFiles(String s) {
        return execCmd("rm -rf " + s) != 0;
    }

    private boolean installPackage(String name) {
        return execCmd("dpkg-deb -x " + name + " " + tempDir + "/") != 0;
    }

    private boolean downloadFile(String url, String fil, int siz) {
        File f = new File(fil);
        switch (downMode) {
            case 3:
                if (!f.exists()) {
                    break;
                }
                if (siz < 0) {
                    if (f.lastModified() < regeTim) {
                        break;
                    }
                    return false;
                }
                if (f.length() == siz) {
                    return false;
                }
                break;
            case 2:
                if (f.exists()) {
                    return false;
                }
                break;
            case 1:
                break;
            case 0:
                return false;
        }
        userFlash.delete(fil + ".tmp");
        if (execCmd("wget -O " + fil + ".tmp " + url) != 0) {
            pip.linePut("error downloading " + fil);
            return true;
        }
        userFlash.rename(fil, fil + ".bak", true, true);
        userFlash.copy(fil + ".tmp", fil, true);
        userFlash.delete(fil + ".tmp");
        return false;
    }

    private boolean verifyPackage(String fil, String sum) {
        cryHashGeneric h = new cryHashSha2256();
        h.init();
        if (cryUtils.hashFile(h, new File(fil))) {
            pip.linePut("error reading " + fil);
            return true;
        }
        if (!sum.equals(cryUtils.hash2hex(h))) {
            pip.linePut("checksum mismatch on " + fil);
            return true;
        }
        pip.linePut(fil + " verified");
        return false;
    }

    private boolean readUpCatalog(userImageCat cat, List<String> res) {
        if (res == null) {
            return true;
        }
        userImagePkg pkg = new userImagePkg("");
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String a = res.get(cnt).trim();
            int i = a.indexOf(":");
            if (i < 1) {
                continue;
            }
            String b = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("package")) {
                pkg.cat = cat;
                allPkgs.put(pkg);
                pkg = new userImagePkg(b);
                continue;
            }
            if (a.equals("depends")) {
                pkg.addDepends(b);
                continue;
            }
            if (a.equals("pre-depends")) {
                pkg.addDepends(b);
                continue;
            }
            if (a.equals("sha256")) {
                pkg.sum = b;
                continue;
            }
            if (a.equals("filename")) {
                pkg.file = b;
                continue;
            }
            if (a.equals("version")) {
                pkg.vers = b;
                continue;
            }
            if (a.equals("size")) {
                pkg.size = bits.str2num(b);
                continue;
            }
        }
        return false;
    }

    private boolean readUpCatalog(cmds cmd) {
        String name = cmd.word();
        String comp = cmd.word();
        String mirr = cmd.word();
        String dist = cmd.word();
        for (;;) {
            String pool = cmd.word();
            if (pool.length() < 1) {
                break;
            }
            userImageCat cat = new userImageCat(name + "-" + pool);
            cat.url = mirr;
            catalogs.add(cat);
            cmd.error("reading " + name + " " + pool + " list");
            String cat1 = tempDir + "/" + name + "-" + pool + ".txt";
            String cat2 = downDir + "/" + arch + "--" + name + "-" + pool + "." + comp;
            userFlash.delete(cat1);
            if (downloadFile(mirr + "dists/" + dist + "/" + pool + "/binary-" + arch + "/Packages." + comp, cat2, -1)) {
                return true;
            }
            if (comp.equals("xz")) {
                execCmd("cp " + cat2 + " " + cat1 + ".xz");
                execCmd("xz -d " + cat1 + ".xz");
            }
            if (comp.equals("gz")) {
                execCmd("cp " + cat2 + " " + cat1 + ".gz");
                execCmd("gzip -d " + cat1 + ".gz");
            }
            List<String> res = bits.txt2buf(cat1);
            userFlash.delete(cat1);
            if (readUpCatalog(cat, res)) {
                cmd.error("parse failed");
                return true;
            }
        }
        return false;
    }

    private void selectOnePackage(int level, String nam, String by) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return;
        }
        userImagePkg pkt = new userImagePkg(nam);
        pkt = allPkgs.find(pkt);
        if (pkt == null) {
            missing.add(new userImagePkg(nam));
            return;
        }
        pkt.added = by;
        if (matchReg(forbidden, nam) != null) {
            discarded.add(pkt);
            return;
        }
        if (selected.add(pkt) != null) {
            return;
        }
        for (int i = 0; i < pkt.depend.size(); i++) {
            selectOnePackage(level + 1, pkt.depend.get(i), nam);
        }
    }

    private boolean downAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImagePkg pkg = selected.get(i);
            if (downloadFile(pkg.cat.url + pkg.file, getPackageName(pkg), pkg.size)) {
                return true;
            }
            if (verifyPackage(getPackageName(pkg), pkg.sum)) {
                return true;
            }
        }
        return false;
    }

    private boolean instAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImagePkg pkg = selected.get(i);
            if (installPackage(getPackageName(pkg))) {
                return true;
            }
        }
        return false;
    }

    /**
     * do the work
     *
     * @param cmd command to do
     * @return true on error, false on success
     */
    public boolean doer(cmds cmd) {
        pip = cmd.pipe;
        List<String> res = bits.txt2buf(cmd.word());
        if (res == null) {
            cmd.error("no such file");
            return true;
        }
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            s = s.replaceAll("%tmp%", tempDir);
            s = s.replaceAll("%dwn%", downDir);
            s = s.replaceAll("%img%", imgName);
            s = s.replaceAll("%arch%", arch);
            s = s.replaceAll("%find%", found);
            s = s.replaceAll("%%", "%");
            s += "#";
            int i = s.indexOf("#");
            s = s.substring(0, i).trim();
            s += " ";
            i = s.indexOf(" ");
            String a = s.substring(0, i).trim().toLowerCase();
            s = s.substring(i, s.length()).trim();
            if (a.length() < 1) {
                continue;
            }
            if (a.equals("exec")) {
                execCmd(s);
                continue;
            }
            cmd.error("--> " + a + " " + s + " <--");
            if (a.equals("include")) {
                cmds c = new cmds("", s);
                c.pipe = pip;
                if (doer(c)) {
                    return true;
                }
                continue;
            }
            if (a.equals("reget-time")) {
                regeTim = bits.getTime() - (bits.str2num(s) * 3600 * 1000);
                continue;
            }
            if (a.equals("download")) {
                downMode = bits.str2num(s);
                continue;
            }
            if (a.equals("arch")) {
                arch = s;
                continue;
            }
            if (a.equals("temp")) {
                tempDir = s;
                continue;
            }
            if (a.equals("exit")) {
                break;
            }
            if (a.equals("find-clear")) {
                found = "";
                continue;
            }
            if (a.equals("find-result")) {
                cmd.error("result='" + found + "'");
                continue;
            }
            if (a.equals("find-file")) {
                i = s.indexOf(" ");
                a = s.substring(0, i);
                s = s.substring(i + 1, s.length());
                File[] fl = userFlash.dirList(a);
                if (fl == null) {
                    cmd.error("error getting list");
                    continue;
                }
                for (i = 0; i < fl.length; i++) {
                    a = fl[i].getName();
                    if (!a.matches(s)) {
                        continue;
                    }
                    found = a;
                    break;
                }
                continue;
            }
            if (a.equals("catalog-read")) {
                cmds c = new cmds("", s);
                c.pipe = pip;
                if (readUpCatalog(c)) {
                    return true;
                }
                continue;
            }
            if (a.equals("select-one")) {
                selectOnePackage(0, s, s);
                continue;
            }
            if (a.equals("select-dis")) {
                forbidden.add(new userImagePkg(s));
                continue;
            }
            if (a.equals("select-del")) {
                selected.del(new userImagePkg(s));
                continue;
            }
            if (a.equals("select-lst")) {
                for (i = 0; i < selected.size(); i++) {
                    cmd.error("" + selected.get(i));
                }
                continue;
            }
            if (a.equals("select-sum")) {
                cmd.error("");
                cmd.error("available:" + dumpList(allPkgs, false));
                cmd.error("");
                cmd.error("forbidden:" + dumpList(forbidden, true));
                cmd.error("");
                cmd.error("discarded:" + dumpList(discarded, true));
                cmd.error("");
                cmd.error("selected:" + dumpList(selected, true));
                cmd.error("");
                cmd.error("missing:" + dumpList(missing, true));
                tabGen<userImagePkg> lst = new tabGen<userImagePkg>();
                for (i = 0; i < catalogs.size(); i++) {
                    userImageCat cat = catalogs.get(i);
                    filterCat(lst, cat, selected);
                    cmd.error("");
                    cmd.error("from " + cat + ":" + dumpList(lst, true));
                }
                cmd.error("");
                continue;
            }
            if (a.equals("package-down")) {
                if (downAllFiles()) {
                    return true;
                }
                continue;
            }
            if (a.equals("package-inst")) {
                if (instAllFiles()) {
                    return true;
                }
                continue;
            }
            if (a.equals("del-ifdn")) {
                if (downMode == 1) {
                    delFiles(s);
                }
                continue;
            }
            if (a.equals("del-alw")) {
                delFiles(s);
                continue;
            }
            cmd.error("unknown command: " + a + " " + s);
            return true;
        }
        return false;
    }

}

class userImageCat implements Comparator<userImageCat> {

    public final String name;

    public String url;

    public userImageCat(String n) {
        name = n.trim();
    }

    public int compare(userImageCat o1, userImageCat o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        return name;
    }

}

class userImagePkg implements Comparator<userImagePkg> {

    public final String name;

    public userImageCat cat;

    public String added = "";

    public String sum = "";

    public String file = "";

    public String vers = "";

    public int size = 0;

    public List<String> depend = new ArrayList<String>();

    public int level;

    public userImagePkg(String n) {
        name = n.trim();
    }

    public int compare(userImagePkg o1, userImagePkg o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        String s = name + " " + cat + " " + vers + " " + added + " " + file + " " + size;
        for (int i = 0; i < depend.size(); i++) {
            s += " " + depend.get(i);
        }
        return s;
    }

    public void addDepends(String s) {
        s += ",";
        for (;;) {
            int i = s.indexOf(",");
            int o = s.indexOf("|");
            if (i < 0) {
                break;
            }
            if ((o >= 0) && (o < i)) {
                i = o;
            }
            String a = s.substring(0, i).trim();
            s = s.substring(i + 1, s.length());
            i = a.indexOf("(");
            if (i >= 0) {
                a = a.substring(0, i);
            }
            depend.add(a.trim());
        }
    }

}
