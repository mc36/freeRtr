package org.freertr.pipe;

import java.io.InputStream;
import java.io.OutputStream;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * provide shell access
 *
 * @author matecsaba
 */
public class pipeShell {

    /**
     * console to use
     */
    protected pipeSide console;

    private Process process;

    private InputStream stdIn;

    private InputStream stdErr;

    private OutputStream stdOut;

    /**
     * state, 1=stdin, 2=stderr, 4=stdout, 128=needed
     */
    protected int running;

    /**
     * get kernel uptime
     *
     * @return uptime
     */
    public static long getKernelUptime() {
        List<String> txt = bits.txt2buf("/proc/stat");
        if (txt == null) {
            return 0;
        }
        long o = 0;
        for (int i = 0; i < txt.size(); i++) {
            String s = txt.get(i);
            s = s.toLowerCase();
            if (!s.startsWith("btime ")) {
                continue;
            }
            s = s.substring(6, s.length()).trim();
            o = bits.str2long(s) * 1000;
        }
        return o;
    }

    /**
     * get my process id
     *
     * @return pid
     */
    public static long myProcessNum() {
        List<String> txt = bits.txt2buf("/proc/self/stat");
        if (txt == null) {
            return 0;
        }
        if (txt.size() < 1) {
            return 0;
        }
        String s = txt.get(0);
        int i = s.indexOf(" ");
        if (i < 1) {
            return 0;
        }
        return bits.str2long(s.substring(0, i));
    }

    /**
     * create shell pipe
     *
     * @param console console to use
     * @param command command to execute
     * @param fincmd final long command
     * @param closeOnExit close on exit
     * @param needStderr need stderr
     * @param convertCrLf convert cr or lf to crlf
     * @param killChilds kill children on termination
     * @return shell container
     */
    public static pipeShell exec(pipeSide console, String command, String fincmd, boolean closeOnExit, boolean needStderr, boolean convertCrLf, boolean killChilds) {
        List<String> l = new ArrayList<String>();
        cmds cmd = new cmds("", command);
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            l.add(s);
        }
        if (fincmd != null) {
            l.add(fincmd);
        }
        String[] cm = new String[l.size()];
        for (int i = 0; i < cm.length; i++) {
            cm[i] = l.get(i);
        }
        try {
            Runtime rtm = Runtime.getRuntime();
            Process prc = rtm.exec(cm);
            return new pipeShell(console, prc, closeOnExit, needStderr, convertCrLf, killChilds);
        } catch (Exception ex) {
            return null;
        }
    }

    /**
     * execute shell command
     *
     * @param command command to execute
     * @param fincmd final long command
     * @param needStderr need stderr
     * @param convertCrLf convert cr or lf to crlf
     * @param killChilds kill children on termination
     * @return resulting lines
     */
    public static List<String> exec(String command, String fincmd, boolean needStderr, boolean convertCrLf, boolean killChilds) {
        pipeReader rd = new pipeReader();
        rd.setLineMode(pipeSide.modTyp.modeCRorLF);
        pipeShell sh = pipeShell.exec(rd.getPipe(), command, fincmd, true, needStderr, convertCrLf, killChilds);
        if (sh == null) {
            return null;
        }
        sh.waitFor();
        rd.waitFor();
        return rd.getResult();
    }

    /**
     * start shell on a runing process
     *
     * @param con console to use
     * @param prc process to view
     * @param closeOnExit close on exit
     * @param needStderr need stderr
     * @param convertCrLf convert cr or lf to crlf
     * @param killChilds kill children on termination
     */
    protected pipeShell(pipeSide con, Process prc, boolean closeOnExit, boolean needStderr, boolean convertCrLf, boolean killChilds) {
        console = con;
        process = prc;
        stdIn = prc.getInputStream();
        stdErr = prc.getErrorStream();
        stdOut = prc.getOutputStream();
        running = 0x87;
        if (closeOnExit) {
            running |= 0x40;
        }
        if (killChilds) {
            running |= 0x20;
        }
        new pipeShellInput(this, stdIn, 1, convertCrLf);
        if (needStderr) {
            new pipeShellInput(this, stdErr, 2, convertCrLf);
        }
        new pipeShellOutput(this, stdOut, 4);
    }

    /**
     * check if running needed
     *
     * @return true if running, false if shutting down
     */
    public boolean isRunning() {
        return (running & 0x80) != 0;
    }

    /**
     * kill the whole process
     */
    public void kill() {
        kill(0);
    }

    /**
     * kill the whole process
     *
     * @param stat status code, 0 if external
     */
    protected synchronized void kill(int stat) {
        running &= 0x7f - stat;
        Object[] childs = new ProcessHandle[0];
        try {
            Stream<ProcessHandle> descends = process.descendants();
            childs = descends.toArray();
        } catch (Exception e) {
        }
        try {
            process.destroy();
        } catch (Exception e) {
        }
        if ((running & 0x20) != 0) {
            for (int i = 0; i < childs.length; i++) {
                try {
                    ProcessHandle chld = (ProcessHandle) (childs[i]);
                    chld.destroy();
                } catch (Exception e) {
                }
            }
            running &= ~0x20;
        }
        bits.sleep(100);
        try {
            stdIn.close();
        } catch (Exception e) {
        }
        try {
            stdOut.close();
        } catch (Exception e) {
        }
        try {
            stdErr.close();
        } catch (Exception e) {
        }
        if ((running & 0x40) != 0) {
            running &= 0x3f;
            console.setClose();
        }
    }

    /**
     * read result code
     *
     * @return result code
     */
    public int resultNum() {
        try {
            return process.exitValue();
        } catch (Exception e) {
            return -1;
        }
    }

    /**
     * read result code
     *
     * @return result code
     */
    public String resultStr() {
        int i = resultNum();
        if (i == 0) {
            return null;
        }
        return "exitcode=" + i;
    }

    /**
     * wait for termination
     */
    public void waitFor() {
        for (;;) {
            if (console.isClosed() != 0) {
                kill(0);
                return;
            }
            if (running == 0) {
                return;
            }
            bits.sleep(100);
        }
    }

    /**
     * get information
     *
     * @return return info
     */
    public String info() {
        String a = "0|0";
        String b = "0";
        String c = "0";
        ProcessHandle hnd = null;
        try {
            hnd = process.toHandle();
            a = hnd.pid() + "|" + hnd.descendants().count();
        } catch (Exception e) {
        }
        try {
            ProcessHandle.Info nfo = hnd.info();
            Duration dur = nfo.totalCpuDuration().get();
            b = dur.getSeconds() + "." + dur.getNano();
        } catch (Exception e) {
        }
        List<String> txt = bits.txt2buf("/proc/" + hnd.pid() + "/status");
        if (txt != null) {
            for (int i = 0; i < txt.size(); i++) {
                String s = txt.get(i);
                s = s.toLowerCase();
                if (!s.startsWith("vmrss:")) {
                    continue;
                }
                if (!s.endsWith("kb")) {
                    continue;
                }
                c = s.substring(6, s.length() - 2).trim();
            }
        }
        return a + "|" + b + "|" + c;
    }

    /**
     * get information
     *
     * @param p process
     * @return return info
     */
    public static String info(pipeShell p) {
        if (p == null) {
            return "|";
        }
        return p.info();
    }

}

class pipeShellInput implements Runnable {

    private final static int maxBuf = 1024;

    private pipeShell prnt;

    private InputStream strm;

    private int stat;

    private boolean crlf;

    public pipeShellInput(pipeShell parent, InputStream stream, int state, boolean convert) {
        prnt = parent;
        strm = stream;
        stat = state;
        crlf = convert;
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            byte[] buf = null;
            int siz = 0;
            try {
                siz = strm.available();
                if (siz < 1) {
                    if (!prnt.isRunning()) {
                        break;
                    }
                    siz = 1;
                }
                if (siz > maxBuf) {
                    siz = maxBuf;
                }
                buf = new byte[siz];
                siz = strm.read(buf);
            } catch (Exception e) {
            }
            if (buf == null) {
                break;
            }
            if (siz < 0) {
                break;
            }
            if (!crlf) {
                prnt.console.blockingPut(buf, 0, siz);
                continue;
            }
            byte[] lf = new byte[2];
            lf[0] = 13;
            lf[1] = 10;
            for (int i = 0; i < siz; i++) {
                int ch = buf[i] & 0xff;
                if ((ch != 13) && (ch != 10)) {
                    prnt.console.blockingPut(buf, i, 1);
                    continue;
                }
                prnt.console.blockingPut(lf, 0, lf.length);
            }
        }
        prnt.kill(stat);
    }

}

class pipeShellOutput implements Runnable {

    private pipeShell prnt;

    private OutputStream strm;

    private int stat;

    public pipeShellOutput(pipeShell parent, OutputStream stream, int state) {
        prnt = parent;
        strm = stream;
        stat = state;
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            byte[] buf = new byte[1024];
            int siz = prnt.console.nonBlockGet(buf, 0, buf.length);
            if (siz < 1) {
                if (!prnt.isRunning()) {
                    break;
                }
                prnt.console.notif.sleep(100);
                continue;
            }
            try {
                strm.write(buf, 0, siz);
                strm.flush();
            } catch (Exception e) {
            }
        }
        prnt.kill(stat);
    }

}
