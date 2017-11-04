package user;

import cfg.cfgAll;
import clnt.clntFtp;
import clnt.clntHttp;
import clnt.clntSmtp;
import clnt.clntTftp;
import clnt.clntXmodem;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeDiscard;
import pipe.pipeProgress;
import pipe.pipeSide;
import tab.tabGen;
import util.bits;
import util.uniResLoc;

/**
 * flash handler
 *
 * @author matecsaba
 */
public class userFlash {

    /**
     * message handler
     */
    public pipeProgress cons;

    /**
     * create new client
     *
     * @param console console to use
     */
    public userFlash(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
    }

    /**
     * copy one file
     *
     * @param src source file
     * @param trg target file
     * @return result code
     */
    public boolean copy(String src, String trg) {
        RandomAccessFile fs;
        RandomAccessFile ft;
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
        cons.setMax(siz);
        cons.debugStat("copying " + cons.getMax() + " bytes");
        long pos = 0;
        for (; pos < siz;) {
            final int max = 8192;
            long rndl = siz - pos;
            if (rndl > max) {
                rndl = max;
            }
            pos += rndl;
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
            try {
                fs.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            try {
                ft.write(buf, 0, rndi);
            } catch (Exception ex) {
                return true;
            }
            cons.setCurr(pos);
        }
        try {
            fs.close();
        } catch (Exception e) {
        }
        try {
            ft.close();
        } catch (Exception e) {
        }
        cons.debugRes(pos + " bytes done");
        return false;
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
                byte buf[] = new byte[(int) red];
                fs.read(buf, 0, buf.length);
                buf2hex(l, buf, (int) pos);
                pos += buf.length;
            }
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
        if (a.equals("http") || a.equals("https")) {
            clntHttp c = new clntHttp(pipe);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.equals("ftp")) {
            clntFtp c = new clntFtp(pipe);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.equals("tftp")) {
            clntTftp c = new clntTftp(pipe);
            boolean b = c.download(url, f);
            c.cleanUp();
            return b;
        }
        if (a.equals("xmodem")) {
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
        if (a.equals("mailto")) {
            clntSmtp c = new clntSmtp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.equals("http") || a.equals("https")) {
            clntHttp c = new clntHttp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.equals("ftp")) {
            clntFtp c = new clntFtp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.equals("tftp")) {
            clntTftp c = new clntTftp(pipe);
            c.upload(url, f);
            c.cleanUp();
            return;
        }
        if (a.equals("xmodem")) {
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
