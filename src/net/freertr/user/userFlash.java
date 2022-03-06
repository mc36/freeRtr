package net.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.clnt.clntFtp;
import net.freertr.clnt.clntHttp;
import net.freertr.clnt.clntSmtp;
import net.freertr.clnt.clntTftp;
import net.freertr.clnt.clntXmodem;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashMd5;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryHashSha3256;
import net.freertr.cry.cryHashSha3512;
import net.freertr.cry.cryUtils;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.uniResLoc;

/**
 * flash handler
 *
 * @author matecsaba
 */
public class userFlash {

    /**
     * create instance
     */
    public userFlash() {
    }

    /**
     * command to use
     */
    public cmds cmd;

    /**
     * pipeline to use
     */
    public pipeSide pip;

    /**
     * reader of user
     */
    public userReader rdr;

    private String calcFileHash(cryHashGeneric h, String n) {
        File f = new File(n);
        h.init();
        if (cryUtils.hashFile(h, f)) {
            return null;
        }
        return cryUtils.hash2hex(h);
    }

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.flsh, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("editor")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            if (b == null) {
                b = new ArrayList<String>();
            }
            userEditor e = new userEditor(new userScreen(pip), b, a, false);
            if (e.doEdit()) {
                return null;
            }
            bits.buf2txt(true, b, a);
            return null;
        }
        if (a.equals("viewer")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            userEditor v = new userEditor(new userScreen(pip), b, a, false);
            v.doView();
            return null;
        }
        if (a.equals("commander")) {
            userFilman f = new userFilman(new userScreen(pip));
            f.doWork();
            return null;
        }
        if (a.equals("browser")) {
            userBrowser f = new userBrowser(new userScreen(pip), cmd.getRemaining());
            f.doWork();
            return null;
        }
        if (a.equals("binviewer")) {
            a = cmd.getRemaining();
            List<String> b = binRead(a);
            userEditor v = new userEditor(new userScreen(pip), b, a, false);
            v.doView();
            return null;
        }
        if (a.equals("receive")) {
            a = cmd.word();
            doReceive(pip, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
            return null;
        }
        if (a.equals("transmit")) {
            a = cmd.word();
            doSend(pip, uniResLoc.parseOne(cmd.getRemaining()), new File(a));
            return null;
        }
        if (a.equals("archive")) {
            a = cmd.word();
            cmd.error(userExec.doneFail(archive(a, cmd.word(), false)));
            return null;
        }
        if (a.equals("extract")) {
            a = cmd.word();
            cmd.error(userExec.doneFail(extract(a, cmd.word())));
            return null;
        }
        if (a.equals("hash")) {
            a = cmd.getRemaining();
            cmd.error("file=" + a);
            cmd.error("md5=" + calcFileHash(new cryHashMd5(), a));
            cmd.error("sha1=" + calcFileHash(new cryHashSha1(), a));
            cmd.error("sha2256=" + calcFileHash(new cryHashSha2256(), a));
            cmd.error("sha2512=" + calcFileHash(new cryHashSha2512(), a));
            cmd.error("sha3256=" + calcFileHash(new cryHashSha3256(), a));
            cmd.error("sha3512=" + calcFileHash(new cryHashSha3512(), a));
            return null;
        }
        if (a.equals("disk")) {
            a = cmd.getRemaining();
            rdr.putStrTab(diskInfo(a));
            return null;
        }
        if (a.equals("info")) {
            a = cmd.getRemaining();
            File f = new File(a);
            userFormat l = new userFormat("|", "category|value");
            try {
                l.add("file|" + f.getCanonicalPath());
                l.add("size|" + f.length());
                l.add("modify|" + bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3));
            } catch (Exception e) {
            }
            rdr.putStrTab(l);
            return null;
        }
        if (a.equals("upgrade")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doUpgrade();
            return null;
        }
        if (a.equals("simulate")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doSimulate();
            return null;
        }
        if (a.equals("backup")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doBackup();
            return null;
        }
        if (a.equals("revert")) {
            if (userUpgrade.doRevert()) {
                return null;
            }
            cfgInit.stopRouter(true, 12, "revert finished");
            return null;
        }
        if (a.equals("verify")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doVerify(null);
            return null;
        }
        if (a.equals("type")) {
            rdr.putStrArr(bits.txt2buf(cmd.getRemaining()));
            return null;
        }
        if (a.equals("bintype")) {
            rdr.putStrArr(binRead(cmd.getRemaining()));
            return null;
        }
        if (a.equals("copy")) {
            a = cmd.word();
            cmd.error(userExec.doneFail(copy(a, cmd.word(), false)));
            return null;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cmd.error(userExec.doneFail(rename(a, cmd.word(), false, false)));
            return null;
        }
        if (a.equals("delete")) {
            cmd.error(userExec.doneFail(delete(cmd.getRemaining())));
            return null;
        }
        if (a.equals("mkdir")) {
            cmd.error(userExec.doneFail(mkdir(cmd.word())));
            return null;
        }
        if (a.equals("list")) {
            rdr.putStrTab(dir2txt(dirList(cmd.getRemaining())));
            return null;
        }
        cmd.badCmd();
        return null;
    }

    private static int archiveChecksum(byte[] buf, boolean pad) {
        if (pad) {
            byte[] res = new byte[buf.length];
            bits.byteCopy(buf, 0, res, 0, res.length);
            for (int i = 0; i < 8; i++) {
                res[148 + i] = 32;
            }
            buf = res;
        }
        int o = 0;
        for (int i = 0; i < buf.length; i++) {
            o += buf[i] & 0xff;
        }
        return o;
    }

    private static byte[] toOctal(long l, int s) {
        return bits.padBeg(Long.toString(l, 8), s, "0").getBytes();
    }

    private static long fromOctal(byte[] buf, int pos) {
        int o = 0;
        for (;;) {
            if (buf[pos + o] == 0) {
                break;
            }
            o++;
        }
        try {
            return Long.parseLong(new String(buf, pos, o), 8);
        } catch (Exception e) {
            return -1;
        }
    }

    private static byte[] archiveHeader(String nam, File fil) {
        boolean dir = fil.isDirectory();
        long siz = fil.length();
        byte[] buf = new byte[512];
        byte[] tmp = nam.getBytes();
        bits.byteCopy(tmp, 0, buf, 0, tmp.length); // name
        int i = 0;
        if (fil.canExecute()) {
            i |= 1 << 6;
        }
        if (fil.canWrite()) {
            i |= 2 << 6;
        }
        if (fil.canRead()) {
            i |= 4 << 6;
        }
        if (dir) {
            siz = 0;
        }
        tmp = toOctal(i, 7);
        bits.byteCopy(tmp, 0, buf, 100, tmp.length); // mode
        tmp = toOctal(1000, 7);
        bits.byteCopy(tmp, 0, buf, 108, tmp.length); // uid
        bits.byteCopy(tmp, 0, buf, 116, tmp.length); // gid
        tmp = toOctal(siz, 11);
        bits.byteCopy(tmp, 0, buf, 124, tmp.length); // size
        tmp = toOctal(fil.lastModified() / 1000, 11);
        bits.byteCopy(tmp, 0, buf, 136, tmp.length); // mtime
        if (dir) {
            tmp = " 5".getBytes();
        } else {
            tmp = " 0".getBytes();
        }
        bits.byteCopy(tmp, 0, buf, 155, tmp.length); // type
        tmp = "ustar  ".getBytes();
        bits.byteCopy(tmp, 0, buf, 257, tmp.length); // magic
        tmp = "rtr".getBytes();
        bits.byteCopy(tmp, 0, buf, 265, tmp.length); // uname
        bits.byteCopy(tmp, 0, buf, 297, tmp.length); // gname
        i = archiveChecksum(buf, true);
        tmp = toOctal(i, 6);
        bits.byteCopy(tmp, 0, buf, 148, tmp.length); // checksum
        return buf;
    }

    private boolean doArchiveFile(RandomAccessFile ft, String dir, String beg) {
        pip.linePut("adding " + beg);
        RandomAccessFile fs;
        long siz;
        try {
            File f = new File(dir);
            fs = new RandomAccessFile(f, "r");
            siz = fs.length();
            ft.write(archiveHeader(beg, f));
        } catch (Exception e) {
            return true;
        }
        boolean res = doCopy(fs, ft, siz);
        try {
            int i = (int) (siz % 512);
            if (i > 0) {
                ft.write(new byte[512 - i]);
            }
            fs.close();
        } catch (Exception e) {
            return true;
        }
        return res;
    }

    private boolean doArchiveDir(RandomAccessFile ft, String dir, String beg) {
        pip.linePut("adding " + beg);
        File[] fl;
        try {
            File f = new File(dir);
            fl = f.listFiles();
            if (beg.length() > 0) {
                ft.write(archiveHeader(beg, f));
            }
        } catch (Exception e) {
            return true;
        }
        if (fl == null) {
            return true;
        }
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
            if (a.equals(".")) {
                continue;
            }
            if (a.equals("..")) {
                continue;
            }
            boolean r;
            if (f.isDirectory()) {
                r = doArchiveDir(ft, dir + a + "/", beg + a + "/");
            } else {
                r = doArchiveFile(ft, dir + a, beg + a);
            }
            if (r) {
                return true;
            }
        }
        return false;
    }

    /**
     * create archive file
     *
     * @param arc archive file
     * @param dir directory
     * @param overwrite delete target before
     * @return result code
     */
    public boolean archive(String arc, String dir, boolean overwrite) {
        if (!dir.endsWith("/")) {
            dir = dir + "/";
        }
        pip.linePut("creating " + arc + " from " + dir);
        if (overwrite) {
            delete(arc);
        }
        RandomAccessFile ft;
        try {
            new File(arc).createNewFile();
        } catch (Exception e) {
            return true;
        }
        try {
            ft = new RandomAccessFile(new File(arc), "rw");
            long siz = ft.length() - 1024;
            if (siz < 0) {
                siz = 0;
            }
            ft.setLength(siz);
            ft.seek(siz);
        } catch (Exception e) {
            return true;
        }
        boolean res = doArchiveDir(ft, dir, "");
        try {
            ft.write(new byte[1024]);
        } catch (Exception ex) {
            res = true;
        }
        try {
            ft.close();
        } catch (Exception e) {
            res = true;
        }
        return res;
    }

    /**
     * extract archive file
     *
     * @param arc archive file
     * @param dir directory
     * @return result code
     */
    public boolean extract(String arc, String dir) {
        if (!dir.endsWith("/")) {
            dir = dir + "/";
        }
        pip.linePut("extracting " + arc + " to " + dir);
        RandomAccessFile fs;
        try {
            File f = new File(arc);
            fs = new RandomAccessFile(f, "r");
        } catch (Exception e) {
            return true;
        }
        boolean res = true;
        for (;;) {
            byte[] buf = new byte[512];
            int i;
            try {
                i = fs.read(buf);
            } catch (Exception e) {
                return true;
            }
            if (i != buf.length) {
                pip.linePut("error reading header");
                return true;
            }
            i = archiveChecksum(buf, false);
            if (i < 1) {
                res = false;
                break;
            }
            i = archiveChecksum(buf, true);
            if (fromOctal(buf, 148) != i) {
                pip.linePut("invalid header checksum");
                break;
            }
            int o = 0;
            for (i = 0; i < 100; i++) {
                if (buf[i] != 0) {
                    continue;
                }
                o = i;
                break;
            }
            arc = new String(buf, 0, o);
            pip.linePut("extracting " + arc);
            if (buf[156] == '5') {
                mkdir(dir + arc);
                continue;
            }
            long siz = fromOctal(buf, 124);
            arc = dir + arc;
            delete(arc);
            RandomAccessFile ft;
            try {
                new File(arc).createNewFile();
            } catch (Exception e) {
                pip.linePut("error creating file");
                break;
            }
            File fil = new File(arc);
            try {
                ft = new RandomAccessFile(fil, "rw");
                ft.setLength(0);
            } catch (Exception e) {
                pip.linePut("error opening file");
                break;
            }
            boolean ress = doCopy(fs, ft, siz);
            try {
                ft.close();
            } catch (Exception e) {
                pip.linePut("error closing file");
                break;
            }
            if (ress) {
                pip.linePut("error extracting file");
                break;
            }
            i = (int) fromOctal(buf, 100);
            try {
                i = i >>> 6;
                fil.setExecutable((i & 1) != 0);
                fil.setWritable((i & 2) != 0);
                fil.setReadable((i & 4) != 0);
                fil.setLastModified(1000 * fromOctal(buf, 136));
            } catch (Exception e) {
                pip.linePut("error setting rights");
                break;
            }
            i = (int) (siz % 512);
            if (i < 1) {
                continue;
            }
            i = 512 - i;
            try {
                fs.seek(fs.getFilePointer() + i);
            } catch (Exception e) {
                pip.linePut("error skipping padding");
                break;
            }
        }
        try {
            fs.close();
        } catch (Exception e) {
            return true;
        }
        return res;
    }

    private static boolean doCopy(RandomAccessFile fs, RandomAccessFile ft, long siz) {
        long pos = 0;
        for (; pos < siz;) {
            final int max = 64 * 1024;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            int rndi = (int) rndl;
            byte[] buf = new byte[rndi];
            try {
                rndi = fs.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            try {
                ft.write(buf, 0, rndi);
            } catch (Exception ex) {
                return true;
            }
            pos += rndi;
        }
        return false;
    }

    /**
     * copy one file
     *
     * @param src source file
     * @param trg target file
     * @param overwrite delete target before
     * @return result code
     */
    public static boolean copy(String src, String trg, boolean overwrite) {
        if (overwrite) {
            delete(trg);
        }
        RandomAccessFile fs;
        RandomAccessFile ft;
        try {
            if (!new File(src).exists()) {
                return true;
            }
        } catch (Exception e) {
            return true;
        }
        try {
            new File(trg).createNewFile();
        } catch (Exception e) {
            return true;
        }
        long siz;
        try {
            fs = new RandomAccessFile(new File(src), "r");
            siz = fs.length();
            ft = new RandomAccessFile(new File(trg), "rw");
            ft.setLength(0);
        } catch (Exception e) {
            return true;
        }
        boolean res = doCopy(fs, ft, siz);
        try {
            fs.close();
        } catch (Exception e) {
        }
        try {
            ft.close();
        } catch (Exception e) {
        }
        return res;
    }

    /**
     * rename one file
     *
     * @param src source file
     * @param trg target file
     * @param overwrite delete target before
     * @param secure delete source after
     * @return result code
     */
    public static boolean rename(String src, String trg, boolean overwrite, boolean secure) {
        try {
            if (!new File(src).exists()) {
                return true;
            }
        } catch (Exception e) {
            return true;
        }
        if (overwrite) {
            delete(trg);
        }
        try {
            new File(src).renameTo(new File(trg));
        } catch (Exception e) {
            return true;
        }
        if (secure) {
            delete(src);
        }
        return false;
    }

    /**
     * delete one file
     *
     * @param trg target file
     * @return result code
     */
    public static boolean delete(String trg) {
        try {
            new File(trg).delete();
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * make one directory
     *
     * @param trg target file
     * @return result code
     */
    public static boolean mkdir(String trg) {
        try {
            new File(trg).mkdir();
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * list directory
     *
     * @param trg target file
     * @return result code
     */
    public static File[] dirList(String trg) {
        File[] fl;
        try {
            fl = new File(trg).listFiles();
        } catch (Exception e) {
            return null;
        }
        if (fl == null) {
            return null;
        }
        tabGen<userFlashFile> ld = new tabGen<userFlashFile>();
        tabGen<userFlashFile> lf = new tabGen<userFlashFile>();
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            try {
                if (f.isDirectory()) {
                    ld.put(new userFlashFile(f));
                } else {
                    lf.put(new userFlashFile(f));
                }
            } catch (Exception e) {
                continue;
            }
        }
        fl = new File[ld.size() + lf.size()];
        int o = ld.size();
        for (int i = 0; i < o; i++) {
            fl[i] = ld.get(i).f;
        }
        for (int i = 0; i < lf.size(); i++) {
            fl[o + i] = lf.get(i).f;
        }
        return fl;
    }

    /**
     * get disk information
     *
     * @param a target file
     * @return converted text
     */
    public static userFormat diskInfo(String a) {
        File f = new File(a);
        userFormat l = new userFormat("|", "category|value");
        try {
            l.add("path|" + f.getCanonicalPath());
            l.add("free|" + f.getFreeSpace());
            l.add("total|" + f.getTotalSpace());
            l.add("usable|" + f.getUsableSpace());
        } catch (Exception e) {
        }
        return l;
    }

    /**
     * convert list to text
     *
     * @param fl file list
     * @return converted text
     */
    public static userFormat dir2txt(File[] fl) {
        if (fl == null) {
            return null;
        }
        userFormat l = new userFormat("|", "date|size|name");
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a;
            if (f.isDirectory()) {
                a = "dir";
            } else {
                a = "" + f.length();
            }
            l.add(bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3) + "|" + a + "|" + f.getName());
        }
        return l;
    }

    /**
     * convert to hex
     *
     * @param l list to append
     * @param buf buffer
     * @param beg beginning
     */
    public static void buf2hex(List<String> l, byte[] buf, int beg) {
        String s = bits.toHexD(beg) + ":";
        for (int ps = 0; ps < buf.length;) {
            s += " " + bits.toHexB(buf[ps]);
            ps++;
            beg++;
            if ((ps & 3) == 0) {
                s += " ";
            }
            if ((ps & 15) != 0) {
                continue;
            }
            l.add(s);
            s = bits.toHexD(beg) + ":";
        }
        l.add(s);
    }

    /**
     * read binary file for viewing
     *
     * @param fn file
     * @return converted text
     */
    public static List<String> binRead(String fn) {
        try {
            List<String> l = new ArrayList<String>();
            RandomAccessFile fs = new RandomAccessFile(new File(fn), "r");
            long siz = fs.length();
            long pos = 0;
            for (; pos < siz;) {
                final int max = 8192;
                long red = siz - pos;
                if (red > max) {
                    red = max;
                }
                byte[] buf = new byte[(int) red];
                fs.read(buf, 0, buf.length);
                buf2hex(l, buf, (int) pos);
                pos += buf.length;
            }
            fs.close();
            return l;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * download a file
     *
     * @param pipe pipeline to use
     * @param url url to get
     * @param f file to write to
     * @return result, false on success, true on error
     */
    public static boolean doReceive(pipeSide pipe, uniResLoc url, File f) {
        String a = url.proto.trim().toLowerCase();
        if (a.startsWith("http")) {
            clntHttp c = new clntHttp(pipe, cfgAll.getClntPrx(), false);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.startsWith("ftp")) {
            clntFtp c = new clntFtp(pipe);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.startsWith("tftp")) {
            clntTftp c = new clntTftp(pipe);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.startsWith("xmodem")) {
            clntXmodem c = new clntXmodem(pipe);
            boolean b = c.download(f);
            c.cleanUp();
            return b;
        }
        return true;
    }

    /**
     * download a file
     *
     * @param pipe pipeline to use
     * @param url url to get
     * @param f file to write to
     */
    public static void doSend(pipeSide pipe, uniResLoc url, File f) {
        String a = url.proto.trim().toLowerCase();
        if (a.startsWith("mailto")) {
            clntSmtp c = new clntSmtp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.startsWith("http")) {
            clntHttp c = new clntHttp(pipe, cfgAll.getClntPrx(), false);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.startsWith("ftp")) {
            clntFtp c = new clntFtp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.startsWith("tftp")) {
            clntTftp c = new clntTftp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.startsWith("xmodem")) {
            clntXmodem c = new clntXmodem(pipe);
            c.upload(f);
            c.cleanUp();
            return;
        }
    }

}

class userFlashFile implements Comparator<userFlashFile> {

    public final File f;

    public userFlashFile(File fl) {
        f = fl;
    }

    public int compare(userFlashFile o1, userFlashFile o2) {
        return o1.f.getName().compareTo(o2.f.getName());
    }

}
