package net.freertr.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.lang.management.CompilationMXBean;
import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgEvntmgr;
import net.freertr.cfg.cfgInit;
import net.freertr.clnt.clntIrc;
import net.freertr.clnt.clntSyslog;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.user.userFlash;
import net.freertr.user.userFormat;
import net.freertr.user.userUpgrade;

/**
 * logging features
 *
 * @author matecsaba
 */
public class logger {

    private logger() {
    }

    /**
     * log level
     */
    public enum logLev {

        /**
         * debugging message
         */
        msgDebg,
        /**
         * informational message
         */
        msgInfo,
        /**
         * warning message
         */
        msgWarn,
        /**
         * error message
         */
        msgEror,
        /**
         * exception
         */
        msgExcp

    }

    /**
     * name of log file
     */
    public static String logFilNam = "";

    /**
     * level of log file
     */
    public static logLev logFilLev = logLev.msgDebg;

    /**
     * size of log file
     */
    public static int logRotLim;

    /**
     * name of rotate file
     */
    public static String logRotNam = "";

    /**
     * log milliseconds
     */
    public static boolean logMillis = false;

    /**
     * level of terminal
     */
    public static logLev logPipLev = logLev.msgDebg;

    /**
     * syslog handler
     */
    public static List<clntSyslog> logSylHnd = new ArrayList<clntSyslog>();

    /**
     * facility of syslog
     */
    public static int logSylFac = 0;

    /**
     * level of syslog
     */
    public static logLev logSylLev = logLev.msgDebg;

    /**
     * irchandler
     */
    public static clntIrc logIrcHnd = new clntIrc(null, null);

    /**
     * level of irc
     */
    public static logLev logIrcLev = logLev.msgDebg;

    /**
     * level of buffer
     */
    public static logLev logBufLev = logLev.msgDebg;

    /**
     * log position format 0=none, 1=brief, 2=normal, 3=full
     */
    public static int logPosForm = 2;

    private static PrintStream logFilHnd;

    private static int logFilSiz;

    private static logBuf logBufLst = new logBuf(512);

    private static final tabGen<loggerTerm> logPipLst = new tabGen<loggerTerm>();

    private static String dumpTraceClass(StackTraceElement s) {
        final String myModule = "net.freertr.";
        String a = s.getClassName();
        if (!a.startsWith(myModule)) {
            return a;
        }
        a = a.substring(myModule.length(), a.length());
        int i = a.indexOf(".");
        if (i < 0) {
            return a;
        }
        return a.substring(i + 1, a.length());
    }

    private static String dumpTraceElement(StackTraceElement s) {
        return dumpTraceClass(s) + "." + s.getMethodName() + ":" + s.getFileName() + ":" + s.getLineNumber();
    }

    /**
     * get size of buffer
     *
     * @return lines
     */
    public static int getBufSize() {
        return logBufLst.size();
    }

    /**
     * dump stack trace
     *
     * @param st stack list
     * @return dumped
     */
    public static String dumpStackTrace(StackTraceElement[] st) {
        if (st == null) {
            return "null ";
        }
        String s = "";
        for (int i = 0; i < st.length; i++) {
            s = s + dumpTraceElement(st[i]) + "/";
        }
        return s + " ";
    }

    /**
     * get stack tracek
     *
     * @return stack trace
     */
    public static StackTraceElement[] getStackTrace() {
        try {
            return new Throwable().getStackTrace();
        } catch (Exception e) {
            return null;
        }
    }

    private static StackTraceElement getParent() {
        StackTraceElement[] sl = getStackTrace();
        for (int i = 0; i < sl.length; i++) {
            StackTraceElement se = sl[i];
            String sn = dumpTraceClass(se);
            if (sn.equals("logger")) {
                continue;
            }
            if (sn.equals("counter")) {
                continue;
            }
            if (sn.equals("notifier")) {
                continue;
            }
            if (sn.equals("cmds")) {
                continue;
            }
            return se;
        }
        return null;
    }

    private static void logOneLine(logLev level, String typ, String msg) {
        String a;
        switch (logPosForm) {
            case 1:
                a = dumpTraceClass(getParent());
                break;
            case 2:
                a = dumpTraceElement(getParent());
                break;
            case 3:
                a = dumpStackTrace(getStackTrace());
                break;
            default:
                a = "";
                break;
        }
        msg = typ + " " + a + " " + msg;
        if (cfgEvntmgr.processEvent(msg)) {
            return;
        }
        if (logPipLev.compareTo(level) <= 0) {
            a = msg + "\r\n";
            byte[] bb = a.getBytes();
            for (int i = logPipLst.size() - 1; i >= 0; i--) {
                loggerTerm ntry = logPipLst.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.pip.nonBlockPut(bb, 0, bb.length) != pipeLine.wontWork) {
                    continue;
                }
                logPipLst.del(ntry);
            }
        }
        if (logSylLev.compareTo(level) <= 0) {
            for (int i = 0; i < logSylHnd.size(); i++) {
                logSylHnd.get(i).logMsg(level, msg);
            }
        }
        if (logIrcLev.compareTo(level) <= 0) {
            logIrcHnd.logMsg(msg);
        }
        msg = getTimestamp() + " " + msg;
        if (logBufLev.compareTo(level) <= 0) {
            logBufLst.add(msg);
        }
        if (logFilHnd == null) {
            return;
        }
        if (logFilLev.compareTo(level) > 0) {
            return;
        }
        try {
            logFilHnd.println(msg);
            logFilHnd.flush();
        } catch (Exception e) {
            return;
        }
        logFilSiz += msg.length() + 2;
        if (logRotLim < 1) {
            return;
        }
        if (logFilSiz < logRotLim) {
            return;
        }
        try {
            logFilHnd.close();
        } catch (Exception e) {
            traceback(e);
        }
        userFlash.rename(logFilNam, logRotNam, true, true);
        try {
            logFilHnd = new PrintStream(new FileOutputStream(logFilNam, true));
        } catch (Exception e) {
            traceback(e);
        }
        logFilSiz = 0;
    }

    /**
     * get timestamp
     *
     * @return timestamp
     */
    public static String getTimestamp() {
        long tim = bits.getTime() + cfgAll.timeServerOffset;
        String a = "";
        if (logMillis) {
            a = "." + bits.padBeg("" + (tim % 1000), 3, "0");
        }
        return bits.time2str(cfgAll.timeZoneName, tim, 3) + a;
    }

    /**
     * convert level to string
     *
     * @param i level
     * @return string
     */
    public static String level2string(logLev i) {
        switch (i) {
            case msgDebg:
                return "debug";
            case msgInfo:
                return "informational";
            case msgWarn:
                return "warning";
            case msgEror:
                return "error";
            case msgExcp:
                return "exception";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to level
     *
     * @param s string to convert
     * @return returned level
     */
    public static logLev string2level(String s) {
        logLev i = logLev.msgInfo;
        if (s.equals("debug")) {
            i = logLev.msgDebg;
        }
        if (s.equals("informational")) {
            i = logLev.msgInfo;
        }
        if (s.equals("warning")) {
            i = logLev.msgWarn;
        }
        if (s.equals("error")) {
            i = logLev.msgEror;
        }
        if (s.equals("exception")) {
            i = logLev.msgExcp;
        }
        return i;
    }

    /**
     * convert format to string
     *
     * @param i format
     * @return string
     */
    public static String format2string(int i) {
        switch (i) {
            case 0:
                return "none";
            case 1:
                return "brief";
            case 2:
                return "normal";
            case 3:
                return "full";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to format
     *
     * @param s string
     * @return format
     */
    public static int string2format(String s) {
        int i = 2;
        if (s.equals("none")) {
            i = 0;
        }
        if (s.equals("brief")) {
            i = 1;
        }
        if (s.equals("normal")) {
            i = 2;
        }
        if (s.equals("full")) {
            i = 3;
        }
        return i;
    }

    /**
     * debugging message
     *
     * @param a string to log
     */
    public static void debug(String a) {
        logOneLine(logLev.msgDebg, "debug", a);
    }

    /**
     * informational message
     *
     * @param a string to log
     */
    public static void info(String a) {
        logOneLine(logLev.msgInfo, "info", a);
    }

    /**
     * warning message
     *
     * @param a string to log
     */
    public static void warn(String a) {
        logOneLine(logLev.msgWarn, "warning", a);
    }

    /**
     * error message
     *
     * @param a string to log
     */
    public static void error(String a) {
        logOneLine(logLev.msgEror, "error", a);
    }

    /**
     * exception detected
     *
     * @param e exception
     */
    public static void exception(Throwable e) {
        String a = dumpException(e);
        try {
            bits.buf2txt(false, bits.str2lst(a), version.myErrorFile());
        } catch (Exception ee) {
        }
        logOneLine(logLev.msgExcp, "exception", a);
        if (userUpgrade.inProgress == 2) {
            userUpgrade.doRevert();
        }
        cfgInit.stopRouter(false, 8, "exception " + a);
    }

    /**
     * not so critical exception detected
     *
     * @param e exception
     */
    public static void traceback(Throwable e) {
        String a = dumpException(e);
        try {
            bits.buf2txt(false, bits.str2lst(a), version.myErrorFile());
        } catch (Exception ee) {
        }
        logOneLine(logLev.msgExcp, "traceback", a);
    }

    /**
     * dump one exception
     *
     * @param e exception
     * @return dumped
     */
    public static String dumpException(Throwable e) {
        return e + " at " + dumpStackTrace(e.getStackTrace());
    }

    /**
     * start file logger
     *
     * @param fn name of file
     * @return false if successful, true if error happened
     */
    public static boolean fileStart(String fn) {
        try {
            logFilHnd.flush();
            logFilHnd.close();
        } catch (Exception e) {
        }
        logFilHnd = null;
        logFilSiz = 0;
        logFilNam = fn;
        if (fn.length() < 2) {
            logFilNam = "";
            return true;
        }
        try {
            logFilHnd = new PrintStream(new FileOutputStream(fn, true));
            List<String> buf = bufferRead();
            for (int i = 0; i < buf.size(); i++) {
                logFilHnd.println(buf.get(i));
            }
            logFilHnd.flush();
            logFilSiz = (int) new File(fn).length();
        } catch (Exception e) {
        }
        return logFilHnd == null;
    }

    /**
     * read logging buffer
     *
     * @param num number of lines
     * @return list of strings
     */
    public static List<String> bufferRead(int num) {
        return logBufLst.read(num);
    }

    /**
     * read logging buffer
     *
     * @return list of strings
     */
    public static List<String> bufferRead() {
        return logBufLst.read();
    }

    /**
     * clear logging buffer
     */
    public static void bufferClear() {
        logBufLst.clear();
    }

    private static void listThreads(userFormat l, ThreadMXBean m, ThreadGroup g) {
        if (g == null) {
            return;
        }
        Thread[] ts = new Thread[g.activeCount()];
        ThreadGroup[] gs = new ThreadGroup[g.activeGroupCount()];
        g.enumerate(ts, false);
        g.enumerate(gs, false);
        String b = g.getName() + "|";
        for (int i = 0; i < ts.length; i++) {
            Thread t = ts[i];
            if (t == null) {
                continue;
            }
            l.add(b + t.getName() + "|" + (m.getThreadUserTime(t.getId()) / 1000000) + "|" + t.getState() + "|" + dumpStackTrace(t.getStackTrace()));
        }
        for (int i = 0; i < gs.length; i++) {
            listThreads(l, m, gs[i]);
        }
    }

    /**
     * list threads
     *
     * @return list of threads
     */
    public static userFormat listThreads() {
        ThreadGroup r = Thread.currentThread().getThreadGroup();
        ThreadMXBean tb = ManagementFactory.getThreadMXBean();
        userFormat l = new userFormat("|", "grp|name|time|state|stack");
        listThreads(l, tb, r);
        return l;
    }

    /**
     * list sys
     *
     * @return list of sys
     */
    public static userFormat listSys() {
        userFormat l = new userFormat("|", "category|value");
        try {
            MBeanServer mb = ManagementFactory.getPlatformMBeanServer();
            AttributeList atrs = mb.getAttributes(ObjectName.getInstance("java.lang:type=OperatingSystem"), new String[]{"CommittedVirtualMemorySize", "FreePhysicalMemorySize", "ProcessCpuLoad", "ProcessCpuTime", "SystemCpuLoad", "SystemLoadAverage"});
            for (int i = 0; i < atrs.size(); i++) {
                Attribute atr = (Attribute) atrs.get(i);
                l.add(atr.getName() + "|" + atr.getValue());
            }
        } catch (Exception e) {
            traceback(e);
        }
        return l;
    }

    /**
     * list gcs
     *
     * @return list of gcs
     */
    public static userFormat listGcs() {
        userFormat l = new userFormat("|", "category|value");
        Runtime rt = Runtime.getRuntime();
        l.add("memory maximum|" + rt.maxMemory());
        l.add("memory used|" + rt.totalMemory());
        l.add("memory free|" + rt.freeMemory());
        CompilationMXBean cmp = ManagementFactory.getCompilationMXBean();
        l.add("compiler name|" + cmp.getName());
        l.add("compiler time|" + cmp.getTotalCompilationTime());
        List<GarbageCollectorMXBean> gcs = ManagementFactory.getGarbageCollectorMXBeans();
        for (int i = 0; i < gcs.size(); i++) {
            GarbageCollectorMXBean gc = gcs.get(i);
            String n = gc.getName();
            l.add(n + " ran|" + gc.getCollectionCount());
            l.add(n + " time|" + gc.getCollectionTime());
        }
        return l;
    }

    /**
     * start buffer logger
     *
     * @param siz size of buffer in lines
     */
    public static void bufferStart(int siz) {
        logBufLst.resize(siz);
    }

    /**
     * start pipe logger
     *
     * @param pip pipeline
     * @return false on sucecss, true if already added
     */
    public static boolean pipeStart(pipeSide pip) {
        if (pip == null) {
            return true;
        }
        return logPipLst.add(new loggerTerm(pip)) != null;
    }

    /**
     * stop pipe logger
     *
     * @param pip pipeline
     * @return false on sucecss, true if already removed
     */
    public static boolean pipeStop(pipeSide pip) {
        if (pip == null) {
            return true;
        }
        return logPipLst.del(new loggerTerm(pip)) != null;
    }

}

class loggerTerm implements Comparator<loggerTerm> {

    public final pipeSide pip;

    public final int hsh;

    public loggerTerm(pipeSide pipe) {
        pip = pipe;
        hsh = pipe.hashCode();
    }

    public int compare(loggerTerm o1, loggerTerm o2) {
        if (o1.hsh < o2.hsh) {
            return -1;
        }
        if (o1.hsh > o2.hsh) {
            return +1;
        }
        return 0;
    }

}
