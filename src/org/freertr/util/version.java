package org.freertr.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeShell;
import org.freertr.prt.prtRedun;
import org.freertr.serv.servOpenflow;
import org.freertr.serv.servP4lang;
import org.freertr.serv.servStack;
import org.freertr.user.userHelping;

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
            array2list(sa, verCore.logo);
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
            array2list(sa, verCore.license);
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
            sa.add(bits.time2str(cfgAll.timeZoneName, getFileDate(), 4));
        }
        if ((head & 0x1000) != 0) {
            sa.add(usrAgnt);
        }
        if ((head & 0x2000) != 0) {
            sa.add(verCore.homeUrl);
        }
        if ((head & 0x4000) != 0) {
            array2list(sa, verCore.quotes);
        }
        return sa;
    }

    /**
     * put array to list
     *
     * @param trg target
     * @param src source
     */
    public final static void array2list(List<String> trg, String[] src) {
        for (int i = 0; i < src.length; i++) {
            trg.add(src[i]);
        }
    }

    /**
     * generate help list
     *
     * @param hl help to update
     * @param beg beginning
     */
    public static void secretHelp(userHelping hl, int beg) {
        for (int i = 0; i < verCore.secrets.length; i++) {
            String a = verCore.secrets[i][0];
            hl.add(null, beg + " .  " + a + "   sh0w m30www s0m30www " + a);
        }
    }

    /**
     * find in secret list
     *
     * @param a string to find
     * @return found, -1 if nothing
     */
    public static int secretFind(String a) {
        for (int i = 0; i < verCore.secrets.length; i++) {
            if (a.equals(verCore.secrets[i][0])) {
                return i;
            }
        }
        return -1;
    }

    /**
     * get show secret text
     *
     * @param typ type of secret
     * @return list
     */
    public static List<String> secretGet(int typ) {
        ArrayList<String> l = new ArrayList<String>();
        array2list(l, verCore.secrets[typ]);
        l.remove(0);
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
        sa.add("name: " + cfgAll.hostName + ", prnt: " + cfgInit.prntNam + ", hwid: " + cfgInit.hwIdNum + " hwsn: " + cfgInit.hwSnNum);
        sa.add("hwfw: " + getHWfwd1liner());
        sa.add("uptime: since " + bits.time2str(cfgAll.timeZoneName, cfgInit.started + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(cfgInit.started));
        sa.add("pid: " + pipeShell.myProcessNum() + ", reload: " + bits.lst2str(bits.txt2buf(myReloadFile()), " "));
        sa.add("hwc: " + cfgInit.cfgFileHw + ", swc: " + cfgInit.cfgFileSw);
        sa.add("class: v" + getProp("java.class.version") + " @ " + getFileName() + ", rwp: " + getRWpath());
        sa.add("cpu: " + getCPUname() + ", mem: free=" + bits.toUser(rt.freeMemory()) + ", max=" + bits.toUser(rt.maxMemory()) + ", used=" + bits.toUser(rt.totalMemory()));
        long l = pipeShell.getKernelUptime();
        sa.add("host: " + getKernelName() + ", since " + bits.time2str(cfgAll.timeZoneName, l + cfgAll.timeServerOffset, 3) + ", for " + bits.timePast(l));
        sa.add("java: " + getJavaVer("java") + " @ " + getProp("java.home"));
        sa.add("jspec: " + getJavaVer("java.specification"));
        sa.add("vm: " + getVMname());
        sa.add("vmspec: " + getJavaVer("java.vm.specification"));
        return sa;
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public static String getHWfwd1liner() {
        servStack stk = cfgAll.dmnStack.get(0);
        if (stk != null) {
            return prtRedun.getShGenOneLiner() + stk.getShGenOneLiner();
        }
        servP4lang p4l = cfgAll.dmnP4lang.get(0);
        if (p4l != null) {
            return prtRedun.getShGenOneLiner() + p4l.getShGenOneLiner();
        }
        servOpenflow ovs = cfgAll.dmnOpenflow.get(0);
        if (ovs != null) {
            return prtRedun.getShGenOneLiner() + ovs.getShGenOneLiner();
        }
        return prtRedun.getShGenOneLiner() + "swonly";
    }

    /**
     * get java executable
     *
     * @return path of jvms
     */
    public static String getJvmExec() {
        return getProp("java.home") + "/bin/java";
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
        return new File(getFileName()).lastModified();
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
            return "rtr";
        }
        return s.substring(0, i);
    }

    /**
     * get read-write path name
     *
     * @return path
     */
    public static String getRWpath() {
        String a = cfgInit.rwPath;
        if (a == null) {
            a = cfgInit.cfgFileSw;
        }
        if (a == null) {
            a = cfgInit.cfgFileHw;
        }
        if (a == null) {
            a = "./";
        }
        int i = a.lastIndexOf("/");
        if (i < 0) {
            a = "./";
        } else {
            a = a.substring(0, i + 1);
        }
        return a;
    }

    /**
     * get state file name
     *
     * @return filename without path
     */
    public static String myStateFile() {
        if (cfgInit.stateFile != null) {
            return cfgInit.stateFile;
        }
        return getRWpath() + "state.txt";
    }

    /**
     * get reload file name
     *
     * @return filename without path
     */
    public static String myReloadFile() {
        return getRWpath() + "reload.log";
    }

    /**
     * get errors file name
     *
     * @return filename without path
     */
    public static String myErrorFile() {
        return getRWpath() + "errors.log";
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
