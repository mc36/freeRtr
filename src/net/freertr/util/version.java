package net.freertr.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;

/**
 * version utils
 *
 * @author matecsaba
 */
public class version {

    private version() {
    }

    /**
     * 9.1.1
     */
    public final static String verNum = verCore.year + "." + verCore.month + "." + verCore.day;

    /**
     * v9.1.1-rel
     */
    public final static String VerNam = "v" + verNum + verCore.state;

    /**
     * ros v9.1.1-rel
     */
    public final static String namVer = verCore.name + " " + VerNam;

    /**
     * ros/9.1.1-rel
     */
    public final static String usrAgnt = verCore.name + "/" + verNum + verCore.state;

    /**
     * ros v9.1.1-rel, done by me.
     */
    public final static String headLine = namVer + ", done by " + verCore.author + ".";

    /**
     * mimetype text
     */
    public final static String[] mimetypes = {
        // text
        "html   text/html",
        "htm    text/html",
        "css    text/css",
        "rtf    text/richtext",
        "text   text/plain",
        "txt    text/plain",
        "md     text/markdown",
        "*      text/plain",
        // image
        "gif    image/gif",
        "jpeg   image/jpeg",
        "jpg    image/jpeg",
        "tiff   image/tiff",
        "tif    image/tiff",
        "png    image/png",
        "svg    image/svg+xml",
        "ico    image/x-icon",
        "pbm    image/x-portable-bitmap",
        "pgm    image/x-portable-graymap",
        "pnm    image/x-portable-anymap",
        "ppm    image/x-portable-pixmap",
        "xbm    image/x-xbitmap",
        "xpm    image/x-xpixmap",
        // video
        "avi    video/msvideo",
        "mov    video/quicktime",
        "qt     video/quicktime",
        "mpeg   video/mpeg",
        "mpg    video/mpeg",
        "mp4    video/mp4",
        "3gp    video/3gpp",
        "ogv    video/ogg",
        // audio
        "aif    audio/x-aiff",
        "aiff   audio/x-aiff",
        "wav    audio/wav",
        "midi   audio/midi",
        "mid    audio/midi",
        "rmi    audio/midi",
        "ram    audio/x-pn-realaudio",
        "rpm    audio/x-pn-realaudio-plugin",
        "ra     audio/x-realaudio",
        "rm     audio/x-pn-realaudio",
        "mp3    audio/mpeg",
        "oga    audio/ogg",
        "flac   audio/flac",
        // application
        "bin    application/octet-stream",
        "doc    application/msword",
        "docx   application/msword",
        "dvi    application/x-dvi",
        "eps    application/postscript",
        "ps     application/postscript",
        "gz     application/x-gzip",
        "js     application/x-javascript",
        "latex  application/x-latex",
        "lzh    application/x-lzh",
        "pdf    application/pdf",
        "swf    application/x-shockwave-flash",
        "tar    application/tar",
        "tcl    application/x-tcl",
        "tex    application/x-tex",
        "tgz    application/x-gzip",
        "zip    application/zip",
        "xml    application/xml",
        "ogg    application/ogg",
        // wireless application
        "wml    text/vnd.wap.wml",
        "wbmp   image/vnd.wap.wbmp"
    };

    /**
     * get show logo text
     *
     * @param head needed extra lines
     * @return list
     */
    public static List<String> shLogo(int head) {
        List<String> sa = new ArrayList<String>();
        if ((head & 0x01) != 0) {
            sa.add("");
        }
        if ((head & 0x02) != 0) {
            sa.add(headLine);
        }
        if ((head & 0x04) != 0) {
            sa.add("");
        }
        if ((head & 0x08) != 0) {
            sa.addAll(Arrays.asList(verCore.logo));
        }
        if ((head & 0x10) != 0) {
            sa.add("");
        }
        if ((head & 0x20) != 0) {
            sa.add(headLine);
        }
        if ((head & 0x40) != 0) {
            sa.add("");
        }
        if ((head & 0x80) != 0) {
            sa.addAll(Arrays.asList(verCore.license));
        }
        if ((head & 0x100) != 0) {
            sa.add("");
        }
        if ((head & 0x200) != 0) {
            sa.add(verNum);
        }
        if ((head & 0x400) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 3));
        }
        if ((head & 0x800) != 0) {
            sa.add(bits.time2str(cfgAll.timeZoneName, version.getFileDate(), 4));
        }
        if ((head & 0x1000) != 0) {
            sa.add(usrAgnt);
        }
        if ((head & 0x2000) != 0) {
            sa.add(verCore.homeUrl);
        }
        return sa;
    }

    /**
     * get show secret text
     *
     * @param typ type of secret
     * @return list
     */
    public static List<String> shSecret(int typ) {
        ArrayList<String> l = new ArrayList<String>();
        switch (typ) {
            case 1:
                l.add("");
                l.add("   /~~~\\");
                l.add("  |     |_______");
                l.add("  | KEY |       |");
                l.add("   \\___/        |");
                l.add("");
                break;
            case 2:
                l.add("");
                l.add("   /~~\\   /~~\\");
                l.add("  |    \\_/    |");
                l.add("   \\         /");
                l.add("    \\  L0VE /");
                l.add("     \\     /");
                l.add("      \\   /");
                l.add("       \\ /");
                l.add("        V");
                l.add("");
                break;
            case 3:
                l.add("");
                l.add("                 \\   /");
                l.add("                 .\\-/.");
                l.add("             /\\  () ()  /\\");
                l.add("            /  \\ /~-~\\ /  \\");
                l.add("                y  Y  V");
                l.add("          ,-^-./   |   \\,-^-.");
                l.add("         /    {   BUG   }    \\");
                l.add("               \\   |   /");
                l.add("               /\\  A  /\\");
                l.add("              /  \\/ \\/  \\");
                l.add("             /           \\");
                l.add("");
                break;
        }
        return l;
    }

    /**
     * get show platform text
     *
     * @return list
     */
    public static List<String> shPlat() {
        List<String> sa = new ArrayList<String>();
        sa.add(headLine);
        sa.add("");
        Runtime rt = Runtime.getRuntime();
        sa.add("name: " + cfgAll.hostName);
        sa.add("hwid: " + cfgInit.hwIdNum);
        sa.add("hwsn: " + cfgInit.hwSnNum);
        sa.add("uptime: since " + bits.time2str(cfgAll.timeZoneName, cfgInit.started + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(cfgInit.started));
        sa.add("reload: " + bits.lst2str(bits.txt2buf(myReloadFile()), " "));
        sa.add("hwcfg: " + cfgInit.cfgFileHw);
        sa.add("swcfg: " + cfgInit.cfgFileSw);
        sa.add("cpu: " + getCPUname());
        sa.add("mem: free=" + bits.toUser(rt.freeMemory()) + ", max=" + bits.toUser(rt.maxMemory()) + ", used=" + bits.toUser(rt.totalMemory()));
        sa.add("host: " + getKernelName());
        sa.add("java: " + getJavaVer("java") + " @ " + getProp("java.home"));
        sa.add("jspec: " + getJavaVer("java.specification"));
        sa.add("vm: " + getVMname());
        sa.add("vmspec: " + getJavaVer("java.vm.specification"));
        sa.add("class: v" + getProp("java.class.version") + " @ " + getFileName());
        return sa;
    }

    private static String getJavaVer(String s) {
        String vnd = getProp(s + ".vendor");
        String nam = getProp(s + ".name");
        String ver = getProp(s + ".version");
        if (nam != null) {
            nam = " (" + nam + ")";
        } else {
            nam = "";
        }
        return vnd + nam + " v" + ver;
    }

    private static String getProp(String s) {
        try {
            return System.getProperty(s);
        } catch (Exception e) {
            return "?";
        }
    }

    /**
     * get archive date
     *
     * @return date of jar
     */
    public static long getFileDate() {
        return new File(version.getFileName()).lastModified();
    }

    /**
     * get archive name
     *
     * @return pathname jar filename
     */
    public static String getFileName() {
        return getProp("java.class.path");
    }

    /**
     * get archive path name
     *
     * @return filename without extension
     */
    public static String myPathName() {
        String s = getFileName();
        int i = s.lastIndexOf(".");
        int o = s.lastIndexOf("/");
        if (o < 0) {
            o = 0;
        }
        if (i < o) {
            return "unknown";
        }
        return s.substring(0, i);
    }

    /**
     * get archive path dir
     *
     * @return only path
     */
    public static String myWorkDir() {
        String s = getFileName();
        int i = s.lastIndexOf("/");
        if (i < 0) {
            return "./";
        }
        return s.substring(0, i + 1);
    }

    /**
     * get reload file name
     *
     * @return filename without path
     */
    public static String myReloadFile() {
        return myPathName() + ".rld";
    }

    /**
     * get errors file name
     *
     * @return filename without path
     */
    public static String myErrorFile() {
        return myPathName() + ".err";
    }

    /**
     * get memory info
     *
     * @return memory
     */
    public static String getMemoryInfo() {
        Runtime rt = Runtime.getRuntime();
        return bits.toUser(rt.totalMemory()) + "/" + bits.toUser(rt.maxMemory());
    }

    /**
     * get kernel name
     *
     * @return name of kernel
     */
    public static String getKernelName() {
        return getProp("os.name").trim() + " v" + getProp("os.version").trim();
    }

    /**
     * get vm name
     *
     * @return name of vm
     */
    public static String getVMname() {
        return getJavaVer("java.vm").trim();
    }

    /**
     * get cpu name
     *
     * @return name of cpu
     */
    public static String getCPUname() {
        return (Runtime.getRuntime().availableProcessors() + "*" + getProp("os.arch")).trim();
    }

}
