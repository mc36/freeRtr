package pipe;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.cmds;

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
     * create shell pipe
     *
     * @param console console to use
     * @param command command to execute
     * @param fincmd final long command
     * @param closeOnExit close on exit
     * @param needStderr need stderr
     * @return shell container
     */
    public static pipeShell exec(pipeSide console, String command, String fincmd, boolean closeOnExit, boolean needStderr) {
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
        String cm[] = new String[l.size()];
        for (int i = 0; i < cm.length; i++) {
            cm[i] = l.get(i);
        }
        try {
            Runtime rtm = Runtime.getRuntime();
            Process prc = rtm.exec(cm);
            return new pipeShell(console, prc, closeOnExit, needStderr);
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
     * @return resulting lines
     */
    public static List<String> exec(String command, String fincmd, boolean needStderr) {
        pipeReader rd = new pipeReader();
        rd.setLineMode(pipeSide.modTyp.modeCRorLF);
        pipeShell sh = pipeShell.exec(rd.getPipe(), command, fincmd, true, needStderr);
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
     */
    protected pipeShell(pipeSide con, Process prc, boolean closeOnExit, boolean needStderr) {
        console = con;
        process = prc;
        stdIn = prc.getInputStream();
        stdErr = prc.getErrorStream();
        stdOut = prc.getOutputStream();
        running = 0x87;
        if (closeOnExit) {
            running |= 0x40;
        }
        new pipeShellInput(this, stdIn, 1);
        if (needStderr) {
            new pipeShellInput(this, stdErr, 2);
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
     *
     * @param stat status code, 0 if external
     */
    public synchronized void kill(int stat) {
        running &= 0x7f - stat;
        if ((running & 0x40) != 0) {
            running &= 0x3f;
            console.setClose();
        }
        try {
            process.destroy();
        } catch (Exception e) {
        }
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
            bits.sleep(500);
        }
    }

}

class pipeShellInput implements Runnable {

    private static final int maxBuf = 1024;

    private pipeShell prnt;

    private InputStream strm;

    private int stat;

    public pipeShellInput(pipeShell parent, InputStream stream, int state) {
        prnt = parent;
        strm = stream;
        stat = state;
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            if (!prnt.isRunning()) {
                break;
            }
            byte buf[] = null;
            try {
                int siz = strm.available();
                if (siz < 1) {
                    siz = 1;
                }
                if (siz > maxBuf) {
                    siz = maxBuf;
                }
                buf = new byte[siz];
                if (strm.read(buf) != siz) {
                    break;
                }
            } catch (Exception e) {
            }
            if (buf == null) {
                break;
            }
            prnt.console.blockingPut(buf, 0, buf.length);
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
            byte buf[] = new byte[1024];
            int siz = prnt.console.nonBlockGet(buf, 0, buf.length);
            if (siz < 1) {
                if (!prnt.isRunning()) {
                    break;
                }
                bits.sleep(200);
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
