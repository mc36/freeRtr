package org.freertr.user;

import org.freertr.pipe.pipeScreen;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.Deflater;
import java.util.zip.Inflater;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.clnt.clntCurl;
import org.freertr.clnt.clntFtp;
import org.freertr.clnt.clntHttp;
import org.freertr.clnt.clntPop3;
import org.freertr.clnt.clntSmtp;
import org.freertr.clnt.clntTftp;
import org.freertr.clnt.clntXmodem;
import org.freertr.cry.cryHashCrc32;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashMd5;
import org.freertr.cry.cryHashSha1;
import org.freertr.cry.cryHashSha2256;
import org.freertr.cry.cryHashSha2512;
import org.freertr.cry.cryHashSha3256;
import org.freertr.cry.cryHashSha3512;
import org.freertr.cry.cryUtils;
import org.freertr.enc.enc7bit;
import org.freertr.pipe.pipeSide;
import org.freertr.enc.encUrl;
import org.freertr.pipe.pipeWindow;
import org.freertr.prt.prtRedun;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

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
    public userRead rdr;

    /**
     * get ascii art from a file
     *
     * @param fn filename
     * @param con console to draw
     * @return converted ascii
     */
    public static List<String> asciiArt(String fn, pipeScreen con) {
        File fil = new File(fn);
        pipeWindow.imageAscii(con, fil);
        return con.getAscii();
    }

    /**
     * get ansi art from a file
     *
     * @param fn filename
     * @param con console to draw
     */
    public static void ansiArt(String fn, pipeScreen con) {
        File fil = new File(fn);
        con.putCls();
        con.putCur(0, 0);
        pipeWindow.imageAnsi(con, fil);
    }

    /**
     * get ansi image from a file
     *
     * @param fn filename
     * @param con console to draw
     */
    public static void ansiPix(String fn, pipeScreen con) {
        File fil = new File(fn);
        con.putCls();
        con.putCur(0, 0);
        pipeWindow.imageSixel(con, fil);
    }

    /**
     * play ansi animation from a file
     *
     * @param fn filename
     * @param con console to draw
     */
    public static void ansiAnim(String fn, pipeScreen con) {
        File fil = new File(fn);
        con.putCls();
        con.putCur(0, 0);
        pipeWindow.imageAnim(con, fil);
    }

    /**
     * get gzip header
     *
     * @return header
     */
    public static byte[] getGzipHdr() {
        byte[] res = new byte[10];
        res[0] = 31; // magic
        res[1] = (byte) 139; // magic
        res[2] = Deflater.DEFLATED; // deflate
        res[3] = 0; // flags
        res[4] = 0; // mtime
        res[5] = 0; // mtime
        res[6] = 0; // mtime
        res[7] = 0; // mtime
        res[8] = 0; // extra flags
        res[9] = (byte) 255; // os
        return res;
    }

    /**
     * get gzip trailer
     *
     * @param unc uncompressed data
     * @return trailer
     */
    public static byte[] getGzipTrl(byte[] unc) {
        byte[] res = new byte[8];
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        crc.update(unc);
        bits.lsbPutD(res, 0, bits.msbGetD(crc.finish(), 0));
        bits.lsbPutD(res, 4, unc.length);
        return res;
    }

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        if (cfgAll.evalVdcPrivs()) {
            cmd.error("not in a vdc");
            return null;
        }
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.flsh, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("edit")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            if (b == null) {
                b = new ArrayList<String>();
            }
            userEditor e = new userEditor(new pipeScreen(pip), b, a, false);
            if (e.doEdit()) {
                return null;
            }
            cmd.error(cmds.doneFail(bits.buf2txt(true, b, a)));
            return null;
        }
        if (a.equals("view")) {
            a = cmd.getRemaining();
            List<String> b = bits.txt2buf(a);
            userEditor v = new userEditor(new pipeScreen(pip), b, a, false);
            v.doView();
            return null;
        }
        if (a.equals("hexview")) {
            a = cmd.getRemaining();
            List<String> b = hexRead(a);
            userEditor v = new userEditor(new pipeScreen(pip), b, a, false);
            v.doView();
            return null;
        }
        if (a.equals("binview")) {
            a = cmd.getRemaining();
            List<String> b = binRead(a);
            userEditor v = new userEditor(new pipeScreen(pip), b, a, false);
            v.doView();
            return null;
        }
        if (a.equals("7bitview")) {
            a = cmd.getRemaining();
            List<String> l = bits.txt2buf(a);
            l = enc7bit.decodeExtLst(l);
            userEditor v = new userEditor(new pipeScreen(pip), l, a, false);
            v.doView();
            return null;
        }
        if (a.equals("hackview")) {
            a = cmd.getRemaining();
            List<String> l = bits.txt2buf(a);
            l = enc7bit.toHackedLst(l);
            userEditor v = new userEditor(new pipeScreen(pip), l, a, false);
            v.doView();
            return null;
        }
        if (a.equals("commander")) {
            userFilman f = new userFilman(new pipeScreen(pip));
            f.doWork();
            return null;
        }
        if (a.equals("browser")) {
            userBrowser f = new userBrowser(new pipeScreen(pip), cmd.getRemaining());
            f.doWork();
            return null;
        }
        if (a.equals("mailer")) {
            userMailer f = new userMailer(new pipeScreen(pip), cmd.getRemaining());
            f.doWork();
            return null;
        }
        if (a.equals("receive")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(doReceive(pip, encUrl.parseOne(cmd.getRemaining()), new File(a))));
            return null;
        }
        if (a.equals("curl")) {
            a = cmd.getRemaining();
            List<String> res = clntCurl.doGetUrl(pip, a);
            cmd.error(cmds.doneFail(res == null));
            rdr.putStrArr(res);
            return null;
        }
        if (a.equals("permission")) {
            a = cmd.word();
            String s = cmd.word();
            setFilePerm(a, s);
            return null;
        }
        if (a.equals("transmit")) {
            a = cmd.word();
            doSend(pip, encUrl.parseOne(cmd.getRemaining()), new File(a));
            return null;
        }
        if (a.equals("compress")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(compress(a, cmd.word())));
            return null;
        }
        if (a.equals("decompress")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(decompress(a, cmd.word())));
            return null;
        }
        if (a.equals("archive")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(archive(a, cmd.word(), false)));
            return null;
        }
        if (a.equals("extract")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(extract(a, cmd.word())));
            return null;
        }
        if (a.equals("hash")) {
            a = cmd.getRemaining();
            List<String> txt = calcFileHashes(a);
            rdr.putStrArr(txt);
            return null;
        }
        if (a.equals("disk")) {
            a = cmd.getRemaining();
            rdr.putStrTab(diskInfo(a));
            return null;
        }
        if (a.equals("info")) {
            a = cmd.getRemaining();
            rdr.putStrArr(getFileInfo(a));
            return null;
        }
        if (a.equals("cleanup")) {
            a = cmd.getRemaining();
            List<String> lst = userUpgrade.cleanBackups(a);
            rdr.putStrArr(lst);
            return null;
        }
        if (a.equals("peer")) {
            prtRedun.doCore();
            return null;
        }
        if (a.equals("cancel")) {
            a = userUpgrade.stopReverter();
            cmd.error(a);
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
        if (a.equals("toggle-boot")) {
            cmd.error("next boot will be " + userUpgrade.toggleBootMode());
            return null;
        }
        if (a.equals("type")) {
            rdr.putStrArr(bits.txt2buf(cmd.getRemaining()));
            return null;
        }
        if (a.equals("hextype")) {
            rdr.putStrArr(hexRead(cmd.getRemaining()));
            return null;
        }
        if (a.equals("bintype")) {
            rdr.putStrArr(binRead(cmd.getRemaining()));
            return null;
        }
        if (a.equals("7bittype")) {
            a = cmd.getRemaining();
            List<String> l = bits.txt2buf(a);
            l = enc7bit.decodeExtLst(l);
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("hacktype")) {
            a = cmd.getRemaining();
            List<String> l = bits.txt2buf(a);
            l = enc7bit.toHackedLst(l);
            rdr.putStrArr(l);
            return null;
        }
        if (a.equals("copy")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(copy(a, cmd.word(), false)));
            return null;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cmd.error(cmds.doneFail(rename(a, cmd.word(), false, false)));
            return null;
        }
        if (a.equals("delete")) {
            cmd.error(cmds.doneFail(delete(cmd.getRemaining())));
            return null;
        }
        if (a.equals("mkdir")) {
            cmd.error(cmds.doneFail(mkdir(cmd.word())));
            return null;
        }
        if (a.equals("list")) {
            rdr.putStrTab(dir2txt(dirList(cmd.getRemaining())));
            return null;
        }
        if (a.equals("count")) {
            rdr.putStrTab(dirUsage(cmd.getRemaining()));
            return null;
        }
        cmd.badCmd();
        return null;
    }

    /**
     * generate hash from file
     *
     * @param h hash to use
     * @param n file to hash
     * @return hash of the file
     */
    public final static String calcFileHash(cryHashGeneric h, String n) {
        File f = new File(n);
        h.init();
        if (cryUtils.hashFile(h, f)) {
            return null;
        }
        return cryUtils.hash2hex(h);
    }

    /**
     * generate eshash from file
     *
     * @param a file to hash
     * @return hashes of the file
     */
    public final static List<String> calcFileHashes(String a) {
        List<String> r = new ArrayList<String>();
        r.add("file=" + a);
        r.add("md5=" + calcFileHash(new cryHashMd5(), a));
        r.add("sha1=" + calcFileHash(new cryHashSha1(), a));
        r.add("sha2256=" + calcFileHash(new cryHashSha2256(), a));
        r.add("sha2512=" + calcFileHash(new cryHashSha2512(), a));
        r.add("sha3256=" + calcFileHash(new cryHashSha3256(), a));
        r.add("sha3512=" + calcFileHash(new cryHashSha3512(), a));
        return r;
    }

    /**
     * get file information
     *
     * @param a file to info
     * @return hashes of the file
     */
    public final static List<String> getFileInfo(String a) {
        List<String> r = new ArrayList<String>();
        File f = new File(a);
        try {
            r.add("file=" + f.getCanonicalPath());
            r.add("size=" + f.length());
            r.add("modify=" + bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3));
            int i = getFilePerm(f);
            r.add("right=" + perm2str(i) + " " + i);
        } catch (Exception e) {
        }
        return r;
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
        File[] fl = dirList(dir);
        if (fl == null) {
            return true;
        }
        try {
            if (beg.length() > 0) {
                ft.write(archiveHeader(beg, new File(dir)));
            }
        } catch (Exception e) {
        }
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
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
     * compress file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean compress(String src, String trg) {
        pip.linePut("compressing " + src + " to " + trg);
        RandomAccessFile fs;
        RandomAccessFile ft;
        long siz;
        try {
            fs = new RandomAccessFile(new File(src), "r");
            siz = fs.length();
        } catch (Exception e) {
            return true;
        }
        try {
            ft = new RandomAccessFile(new File(trg), "rw");
            ft.setLength(0);
        } catch (Exception e) {
            return true;
        }
        byte[] buf1 = getGzipHdr();
        try {
            ft.write(buf1, 0, buf1.length);
        } catch (Exception e) {
            return true;
        }
        Deflater cmp = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        long pos = 0;
        for (; pos < siz;) {
            final int max = 64 * 1024;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            int rndi = (int) rndl;
            pos += rndi;
            buf1 = new byte[rndi];
            try {
                rndi = fs.read(buf1, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            crc.update(buf1);
            cmp.setInput(buf1);
            if (pos >= siz) {
                cmp.finish();
            }
            byte[] buf2 = new byte[max * 2];
            int rndo = cmp.deflate(buf2);
            try {
                ft.write(buf2, 0, rndo);
            } catch (Exception ex) {
                return true;
            }
        }
        buf1 = new byte[8];
        bits.lsbPutD(buf1, 0, bits.msbGetD(crc.finish(), 0));
        bits.lsbPutD(buf1, 4, (int) pos);
        try {
            ft.write(buf1, 0, buf1.length);
        } catch (Exception ex) {
            return true;
        }
        try {
            fs.close();
            ft.close();
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * decompress file
     *
     * @param src source
     * @param trg target
     * @return result code
     */
    public boolean decompress(String src, String trg) {
        pip.linePut("compressing " + src + " to " + trg);
        RandomAccessFile fs;
        RandomAccessFile ft;
        long siz;
        try {
            fs = new RandomAccessFile(new File(src), "r");
            siz = fs.length();
        } catch (Exception e) {
            return true;
        }
        byte[] buf1 = getGzipHdr();
        try {
            int i = bits.lsbGetD(buf1, 0);
            fs.read(buf1, 0, buf1.length);
            if (i != bits.lsbGetD(buf1, 0)) {
                pip.linePut("bad header");
                fs.close();
                return true;
            }
        } catch (Exception e) {
            return true;
        }
        siz -= buf1.length;
        siz -= 8;
        try {
            ft = new RandomAccessFile(new File(trg), "rw");
            ft.setLength(0);
        } catch (Exception e) {
            return true;
        }
        Inflater cmp = new Inflater(true);
        cryHashCrc32 crc = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        crc.init();
        long pos = 0;
        for (; pos < siz;) {
            final int max = 64 * 1024;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            int rndi = (int) rndl;
            pos += rndi;
            buf1 = new byte[rndi];
            try {
                rndi = fs.read(buf1, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            cmp.setInput(buf1);
            for (; cmp.getRemaining() > 0;) {
                byte[] buf2 = new byte[max * 2];
                int rndo;
                try {
                    rndo = cmp.inflate(buf2);
                } catch (Exception e) {
                    pip.linePut("bad compression");
                    return true;
                }
                crc.update(buf2, 0, rndo);
                try {
                    ft.write(buf2, 0, rndo);
                } catch (Exception ex) {
                    return true;
                }
            }
        }
        buf1 = new byte[8];
        try {
            fs.read(buf1, 0, buf1.length);
        } catch (Exception e) {
            return true;
        }
        try {
            fs.close();
            ft.close();
        } catch (Exception e) {
            return true;
        }
        int i = bits.msbGetD(crc.finish(), 0);
        if (i == bits.lsbGetD(buf1, 0)) {
            return false;
        }
        pip.linePut("bad trailer");
        return true;
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
        if (mkfile(arc)) {
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
            if (mkfile(arc)) {
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
        if (mkfile(trg)) {
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
            if (!new File(src).renameTo(new File(trg))) {
                return true;
            }
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
            if (!new File(trg).delete()) {
                return true;
            }
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
            if (!new File(trg).mkdir()) {
                return true;
            }
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * make one file
     *
     * @param trg target file
     * @return result code
     */
    public static boolean mkfile(String trg) {
        try {
            if (!new File(trg).createNewFile()) {
                return true;
            }
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * list directory
     *
     * @param trg target file
     * @return list of files, null if error happened
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
        tabGen<userFlashNtry> ld = new tabGen<userFlashNtry>();
        tabGen<userFlashNtry> lf = new tabGen<userFlashNtry>();
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a = f.getName();
            if (a.equals(".")) {
                continue;
            }
            if (a.equals("..")) {
                continue;
            }
            try {
                if (f.isDirectory()) {
                    ld.put(new userFlashNtry(f));
                } else {
                    lf.put(new userFlashNtry(f));
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
     * direcory usage
     *
     * @param fn name of file to count
     * @return total usage
     */
    public static userFormat dirUsage(String fn) {
        File[] fl = dirList(fn);
        if (fl == null) {
            return null;
        }
        userFormat res = new userFormat("|", "directory|usage");
        long tot = 0;
        long loc = 0;
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            if (f == null) {
                continue;
            }
            String a = f.getName();
            if (!f.isDirectory()) {
                try {
                    loc += f.length();
                } catch (Exception e) {
                }
                continue;
            }
            long cur = countUsage(f.getAbsolutePath());
            tot += cur;
            res.add(a + "|" + cur);
        }
        res.add("directories|" + tot);
        res.add("files|" + loc);
        return res;
    }

    /**
     * count disk usage
     *
     * @param fn name of file to count
     * @return total usage
     */
    public static long countUsage(String fn) {
        File[] fl = dirList(fn);
        if (fl == null) {
            return 0;
        }
        return recursiveUsage(fl);
    }

    /**
     * count disk usage
     *
     * @param fl list of files to count
     * @return total usage
     */
    protected static long recursiveUsage(File[] fl) {
        long res = 0;
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            if (f == null) {
                continue;
            }
            String a = f.getName();
            if (!f.isDirectory()) {
                try {
                    res += f.length();
                } catch (Exception e) {
                }
                continue;
            }
            File[] nl = dirList(f.getAbsolutePath());
            if (nl == null) {
                continue;
            }
            res += recursiveUsage(nl);
        }
        return res;
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
        userFormat l = new userFormat("|", "right|date|size|name");
        for (int i = 0; i < fl.length; i++) {
            File f = fl[i];
            String a;
            if (f.isDirectory()) {
                a = "dir";
            } else {
                a = "" + f.length();
            }
            int o = getFilePerm(f);
            String s = o + " " + perm2str(o);
            l.add(s + "|" + bits.time2str(cfgAll.timeZoneName, f.lastModified(), 3) + "|" + a + "|" + f.getName());
        }
        return l;
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
                enc7bit.buf2bin(l, buf, (int) pos);
                pos += buf.length;
            }
            fs.close();
            return l;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * read binary file for viewing
     *
     * @param fn file
     * @return converted text
     */
    public static List<String> hexRead(String fn) {
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
                enc7bit.buf2hex(l, buf, (int) pos, "");
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
    public static boolean doReceive(pipeSide pipe, encUrl url, File f) {
        String a = url.proto.trim().toLowerCase();
        if (a.startsWith("pop3")) {
            clntPop3 c = new clntPop3(pipe);
            boolean b = c.download(url, f, false);
            c.cleanUp();
            return b;
        }
        if (a.startsWith("http")) {
            clntHttp c = new clntHttp(pipe, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
            boolean b = c.download(url, f, new ArrayList<String>());
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
     * convert permission to string
     *
     * @param i permission to convert
     * @return converted string
     */
    public final static String perm2str(int i) {
        if (i < 0) {
            return "error";
        }
        String a = "";
        if ((i & 0x1) != 0) {
            a += "r";
        } else {
            a += "-";
        }
        if ((i & 0x2) != 0) {
            a += "w";
        } else {
            a += "-";
        }
        if ((i & 0x4) != 0) {
            a += "x";
        } else {
            a += "-";
        }
        return a;
    }

    /**
     * get file permissions
     *
     * @param f file to read
     * @return file permission bits, -1 on error, 1=read, 2=write, 4=exec
     */
    public final static int getFilePerm(File f) {
        try {
            int i = 0;
            if (f.canRead()) {
                i |= 0x1;
            }
            if (f.canWrite()) {
                i |= 0x2;
            }
            if (f.canExecute()) {
                i |= 0x4;
            }
            return i;
        } catch (Exception e) {
            return -1;
        }
    }

    /**
     * set file permissions
     *
     * @param fn file name
     * @param prm file rights
     * @return true on error, false on success
     */
    public final static boolean setFilePerm(String fn, String prm) {
        boolean or = prm.indexOf("r") >= 0;
        boolean ow = prm.indexOf("w") >= 0;
        boolean ox = prm.indexOf("x") >= 0;
        boolean er = prm.indexOf("R") >= 0;
        boolean ew = prm.indexOf("W") >= 0;
        boolean ex = prm.indexOf("X") >= 0;
        return setFilePerm(fn, or, ow, ox, er, ew, ex);
    }

    /**
     * set file permissions
     *
     * @param fn file name
     * @param or owner read
     * @param ow owner write
     * @param ox owner exec
     * @param er everyone read
     * @param ew everyone write
     * @param ex everyone exec
     * @return true on error, false on success
     */
    public final static boolean setFilePerm(String fn, boolean or, boolean ow, boolean ox, boolean er, boolean ew, boolean ex) {
        try {
            File f = new File(fn);
            f.setReadable(er, false);
            f.setWritable(ew, false);
            f.setExecutable(ex, false);
            f.setReadable(or, true);
            f.setWritable(ow, true);
            f.setExecutable(ox, true);
        } catch (Exception e) {
            logger.traceback(e);
            return true;
        }
        return false;
    }

    /**
     * download a file
     *
     * @param pipe pipeline to use
     * @param url url to get
     * @param f file to write to
     */
    public static void doSend(pipeSide pipe, encUrl url, File f) {
        String a = url.proto.trim().toLowerCase();
        if (a.startsWith("mailto")) {
            clntSmtp c = new clntSmtp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.startsWith("http")) {
            clntHttp c = new clntHttp(pipe, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
            c.upload(url, f, new ArrayList<String>());
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

class userFlashNtry implements Comparable<userFlashNtry> {

    /**
     * file entry
     */
    protected final File f;

    /**
     * create instance
     *
     * @param fl file
     */
    protected userFlashNtry(File fl) {
        f = fl;
    }

    public int compareTo(userFlashNtry o) {
        return f.getName().compareTo(o.f.getName());
    }

}
