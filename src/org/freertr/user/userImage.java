package org.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashSha2256;
import org.freertr.cry.cryUtils;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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

    private int hashMode = 3;

    private long regeTim = bits.getTime() - Integer.MAX_VALUE;

    private String miro = "http://deb.debian.org/debian/";

    private String qemu = "x86_64";

    private String cmpr = "clang";

    private String arch = "amd64";

    private String boot = "x86_64-efi";

    private String kern = "cloud-amd64";

    private String unam = "x86_64";

    private String grub = "efi-amd64-bin";

    private String uefi = "bootx64.efi";

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

    private String dumpList(tabGen<userImagePkg> lst, boolean done, boolean detail) {
        String s = "";
        long o = 0;
        for (int i = 0; i < lst.size(); i++) {
            userImagePkg pkg = lst.get(i);
            o += pkg.size;
            if (!detail) {
                continue;
            }
            if (done != pkg.done) {
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
        pipeShell sh = pipeShell.exec(pip, "sh -c", s, false, true, false, true);
        sh.waitFor();
        return sh.resultNum();
    }

    private boolean delFiles(String s) {
        return execCmd("rm -rf " + s) != 0;
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
        userFlash.rename(fil + ".tmp", fil, true, true);
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
            cat.arch = arch;
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

    private void selectOnePackage(int level, String nam, String by, boolean nod) {
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
        if (nod) {
            return;
        }
        for (int i = 0; i < pkt.depend.size(); i++) {
            selectOnePackage(level + 1, pkt.depend.get(i), nam, nod);
        }
    }

    /**
     * install one file
     *
     * @param pkg package to download
     * @return true on error, false on success
     */
    private boolean instOneFile(userImagePkg pkg) {
        String name = getPackageName(pkg);
        if (pkg.done) {
            pip.linePut("skipping " + name);
            return false;
        }
        pkg.done = true;
        return execCmd("dpkg-deb -x " + name + " " + tempDir + "/") != 0;
    }

    /**
     * download one image
     *
     * @param pkg package to download
     * @return true on error, false on success
     */
    private boolean downOneFile(userImagePkg pkg) {
        String name = getPackageName(pkg);
        if (pkg.done) {
            pip.linePut("skipping " + name);
            return false;
        }
        for (int i = 0; i < hashMode; i++) {
            if (downloadFile(pkg.cat.url + pkg.file, name, pkg.size)) {
                return true;
            }
            boolean vrfy = verifyPackage(name, pkg.sum);
            if (!vrfy) {
                return false;
            }
            userFlash.rename(name, name + ".bak", true, true);
        }
        return true;
    }

    private boolean downAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImagePkg pkg = selected.get(i);
            if (downOneFile(pkg)) {
                return true;
            }
        }
        return false;
    }

    private boolean instAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImagePkg pkg = selected.get(i);
            if (instOneFile(pkg)) {
                return true;
            }
        }
        return false;
    }

    private boolean doIncludeAll(cmds c) {
        boolean res = false;
        for (;;) {
            String s = c.word();
            if (s.length() < 1) {
                break;
            }
            cmds cmd = getCmd(s);
            res |= doIncludeOne(cmd);
        }
        return res;
    }

    private boolean doIncludeOne(cmds c) {
        String s = c.getRemaining();
        c.error("including " + s);
        List<String> lst = bits.txt2buf(s);
        if (lst == null) {
            c.error("no such file " + s);
            return true;
        }
        return doOneFile(lst);
    }

    private cmds getCmd(String s) {
        cmds c = new cmds("img", s);
        c.pipe = pip;
        return c;
    }

    private boolean doOneFile(List<String> res) {
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            s = s.replaceAll("%tmp%", tempDir);
            s = s.replaceAll("%dwn%", downDir);
            s = s.replaceAll("%img%", imgName);
            s = s.replaceAll("%mirr%", miro);
            s = s.replaceAll("%qemu%", qemu);
            s = s.replaceAll("%comp%", cmpr);
            s = s.replaceAll("%arch%", arch);
            s = s.replaceAll("%boot%", boot);
            s = s.replaceAll("%kern%", kern);
            s = s.replaceAll("%unam%", unam);
            s = s.replaceAll("%grub%", grub);
            s = s.replaceAll("%uefi%", uefi);
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
            pip.linePut("--> " + a + " " + s + " <--");
            if (a.equals("exec")) {
                execCmd(s);
                continue;
            }
            cmds cmd = getCmd(a + " " + s);
            cmd.word();
            if (a.equals("include")) {
                if (doIncludeOne(cmd)) {
                    return true;
                }
                continue;
            }
            if (a.equals("inclall")) {
                if (doIncludeAll(cmd)) {
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
            if (a.equals("hashdown")) {
                hashMode = bits.str2num(s);
                continue;
            }
            if (a.equals("mirr")) {
                miro = s;
                continue;
            }
            if (a.equals("qemu")) {
                qemu = s;
                continue;
            }
            if (a.equals("comp")) {
                cmpr = s;
                continue;
            }
            if (a.equals("arch")) {
                arch = s;
                continue;
            }
            if (a.equals("boot")) {
                boot = s;
                continue;
            }
            if (a.equals("kern")) {
                kern = s;
                continue;
            }
            if (a.equals("unam")) {
                unam = s;
                continue;
            }
            if (a.equals("grub")) {
                grub = s;
                continue;
            }
            if (a.equals("uefi")) {
                uefi = s;
                continue;
            }
            if (a.equals("temp")) {
                tempDir = s;
                continue;
            }
            if (a.equals("down")) {
                downDir = s;
                continue;
            }
            if (a.equals("image")) {
                imgName = s;
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
            if (a.equals("catalog-sum")) {
                tabGen<userImagePkg> lst = new tabGen<userImagePkg>();
                for (i = 0; i < catalogs.size(); i++) {
                    userImageCat cat = catalogs.get(i);
                    filterCat(lst, cat, selected);
                    cmd.error("");
                    cmd.error("catalog " + cat + ":" + dumpList(lst, false, true));
                }
                cmd.error("");
                continue;
            }
            if (a.equals("catalog-read")) {
                cmds c = getCmd(s);
                if (readUpCatalog(c)) {
                    return true;
                }
                continue;
            }
            if (a.equals("select-one")) {
                selectOnePackage(0, s, s, false);
                continue;
            }
            if (a.equals("select-nod")) {
                selectOnePackage(0, s, s, true);
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
                    userImagePkg ntry = selected.get(i);
                    if (ntry.done) {
                        continue;
                    }
                    cmd.error("" + ntry);
                }
                continue;
            }
            if (a.equals("select-clr")) {
                selected.clear();
                forbidden.clear();
                discarded.clear();
                missing.clear();
                continue;
            }
            if (a.equals("select-sum")) {
                cmd.error("");
                cmd.error("available:" + dumpList(allPkgs, false, false));
                cmd.error("");
                cmd.error("forbidden:" + dumpList(forbidden, false, true));
                cmd.error("");
                cmd.error("discarded:" + dumpList(discarded, false, true));
                cmd.error("");
                cmd.error("selected:" + dumpList(selected, false, true));
                cmd.error("");
                cmd.error("already:" + dumpList(selected, true, true));
                cmd.error("");
                cmd.error("missing:" + dumpList(missing, false, true));
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

    /**
     * do the work
     *
     * @param cmd command to do
     * @return true on error, false on success
     */
    public boolean doer(cmds cmd) {
        pip = cmd.pipe;
        return doIncludeAll(cmd);
    }

}

class userImageCat implements Comparator<userImageCat> {

    public final String name;

    public String url;

    public String arch;

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

    public boolean done;

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
