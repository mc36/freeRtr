package user;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import pipe.pipeShell;
import pipe.pipeSide;
import util.bits;
import util.cmds;

/**
 * process image creation
 *
 * @author matecsaba
 */
public class userImage {

    private pipeSide pip;

    private String tempDir = "/tmp/rtr.tmp";

    private String downDir = "../binDown";

    private String imgName = "../binImg/rtr";

    private int downMode = 1;

    private String arch = "i386";

    private String dist = "stable";

    private String mirror = "";

    private userImageList allPkgs = new userImageList();

    private userImageList missing = new userImageList();

    private userImageList selected = new userImageList();

    private userImageList forbidden = new userImageList();

    private int exec(String s) {
        pip.linePut("!" + s + ".");
        pipeShell sh = pipeShell.exec(pip, "sh -c", s, false, true);
        sh.waitFor();
        return sh.resultNum();
    }

    private boolean delete(String s) {
        return exec("rm -rf " + s) != 0;
    }

    private boolean download(String url, String fil) {
        switch (downMode) {
            case 2:
                if (new File(fil).exists()) {
                    return false;
                }
                break;
            case 1:
                break;
            case 0:
                return false;
        }
        delete(fil);
        return exec("wget -O " + fil + " " + url) != 0;
    }

    private boolean readUpCatalog(String pool) {
        String cat1 = tempDir + "/" + pool + ".txt";
        String cat2 = downDir + "/" + arch + "--" + pool + ".gz";
        delete(cat1);
        download(mirror + "dists/" + dist + "/" + pool + "/binary-" + arch + "/Packages.gz", cat2);
        exec("cp " + cat2 + " " + cat1 + ".gz");
        exec("gunzip -d " + cat1 + ".gz");
        List<String> res = bits.txt2buf(cat1);
        if (res == null) {
            return true;
        }
        userImageNtry pkg = new userImageNtry();
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String a = res.get(cnt).trim();
            int i = a.indexOf(":");
            if (i < 1) {
                continue;
            }
            String b = a.substring(i + 1, a.length()).trim();
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("package")) {
                allPkgs.update(pkg);
                pkg = new userImageNtry();
                pkg.name = b;
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
        delete(cat1);
        return false;
    }

    private void selectOnePackage(int level, String nam) {
        nam = nam.trim();
        if (nam.length() < 1) {
            return;
        }
        if (forbidden.find(nam) != null) {
            return;
        }
        userImageNtry pkt = allPkgs.find(nam);
        if (pkt == null) {
            missing.add(nam);
            return;
        }
        if (selected.update(pkt)) {
            return;
        }
        for (int i = 0; i < pkt.depend.size(); i++) {
            selectOnePackage(level + 1, pkt.depend.get(i));
        }
        return;
    }

    private void downAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImageNtry pkg = selected.get(i);
            download(mirror + pkg.file, downDir + "/" + arch + "-" + pkg.name + ".deb");
        }
    }

    private void install(String name) {
        exec("dpkg-deb -x " + name + " " + tempDir + "/");
    }

    private void instAllFiles() {
        for (int i = 0; i < selected.size(); i++) {
            userImageNtry pkg = selected.get(i);
            install(downDir + "/" + arch + "-" + pkg.name + ".deb");
        }
    }

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        pip = cmd.pipe;
        List<String> res = bits.txt2buf(cmd.word());
        if (res == null) {
            cmd.error("no such file");
            return;
        }
        missing.setSorting(true);
        selected.setSorting(true);
        forbidden.setSorting(true);
        for (int cnt = 0; cnt < res.size(); cnt++) {
            String s = res.get(cnt);
            s = s.replaceAll("%tmp%", tempDir);
            s = s.replaceAll("%dwn%", downDir);
            s = s.replaceAll("%img%", imgName);
            s = s.replaceAll("%arch%", arch);
            s = s.replaceAll("%dist%", dist);
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
                exec(s);
                continue;
            }
            cmd.error("--> " + a + " " + s + " <--");
            if (a.equals("include")) {
                cmds c = new cmds("", s);
                c.pipe = pip;
                doer(c);
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
            if (a.equals("distro")) {
                dist = s;
                continue;
            }
            if (a.equals("exit")) {
                break;
            }
            if (a.equals("mirror")) {
                mirror = s;
                continue;
            }
            if (a.equals("catalog-read")) {
                cmd.error("reading " + s + " list");
                if (readUpCatalog(s)) {
                    cmd.error("failed");
                }
                continue;
            }
            if (a.equals("catalog-sort")) {
                cmd.error("sorting " + allPkgs.size() + " entries");
                allPkgs.setSorting(true);
                continue;
            }
            if (a.equals("select-one")) {
                selectOnePackage(0, s);
                continue;
            }
            if (a.equals("select-dis")) {
                forbidden.add(s);
                continue;
            }
            if (a.equals("select-del")) {
                selected.del(s);
                continue;
            }
            if (a.equals("select-sum")) {
                cmd.error("");
                cmd.error("forbidden: " + forbidden);
                cmd.error("");
                cmd.error("selected: " + selected);
                cmd.error("");
                cmd.error("missing: " + missing);
                cmd.error("");
                continue;
            }
            if (a.equals("package-down")) {
                downAllFiles();
                continue;
            }
            if (a.equals("package-inst")) {
                instAllFiles();
                continue;
            }
            if (a.equals("del-ifdn")) {
                if (downMode == 1) {
                    delete(s);
                }
                continue;
            }
            if (a.equals("del-alw")) {
                delete(s);
                continue;
            }
            cmd.error("unknown command: " + a + " " + s);
        }
    }

}

class userImageList {

    private final List<userImageNtry> lst = new ArrayList<userImageNtry>();

    private boolean needSorting = false;

    public void setSorting(boolean sorted) {
        needSorting = sorted;
        if (!needSorting) {
            return;
        }
        synchronized (lst) {
            userImageNtry pkg = new userImageNtry();
            Collections.sort(lst, pkg);
        }
    }

    public userImageNtry find(String a) {
        synchronized (lst) {
            userImageNtry pkg = new userImageNtry();
            pkg.name = a;
            int i = Collections.binarySearch(lst, pkg, pkg);
            if (i < 0) {
                return null;
            }
            return lst.get(i);
        }
    }

    public userImageNtry del(String a) {
        synchronized (lst) {
            userImageNtry pkg = new userImageNtry();
            pkg.name = a;
            int i = Collections.binarySearch(lst, pkg, pkg);
            if (i < 0) {
                return null;
            }
            return lst.remove(i);
        }
    }

    public userImageNtry add(String a) {
        userImageNtry pkg = new userImageNtry();
        pkg.name = a;
        update(pkg);
        return pkg;
    }

    public boolean update(userImageNtry pkg) {
        pkg.name = pkg.name.trim();
        synchronized (lst) {
            if (!needSorting) {
                lst.add(pkg);
                return false;
            }
            int i = Collections.binarySearch(lst, pkg, pkg);
            Boolean b;
            if (i < 0) {
                lst.add(pkg);
                b = false;
            } else {
                lst.set(i, pkg);
                b = true;
            }
            Collections.sort(lst, pkg);
            return b;
        }
    }

    public userImageNtry get(int i) {
        synchronized (lst) {
            if (i < 0) {
                return null;
            }
            if (i >= lst.size()) {
                return null;
            }
            return lst.get(i);
        }
    }

    public int size() {
        synchronized (lst) {
            return lst.size();
        }
    }

    public String toString() {
        synchronized (lst) {
            String s = "";
            long o = 0;
            for (int i = 0; i < lst.size(); i++) {
                userImageNtry pkg = lst.get(i);
                o += pkg.size;
                s += " " + pkg.name;
            }
            s += " (" + o / 1024 + " kb)";
            return s.substring(1, s.length());
        }
    }

}

class userImageNtry implements Comparator<userImageNtry> {

    public String name = "";

    public String file = "";

    public String vers = "";

    public int size = 0;

    public List<String> depend = new ArrayList<String>();

    public int level;

    public int compare(userImageNtry o1, userImageNtry o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        String s = "'" + name + "' '" + file + "'";
        for (int i = 0; i < depend.size(); i++) {
            s += ", " + depend.get(i);
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
