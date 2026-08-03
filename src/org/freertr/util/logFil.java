package org.freertr.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import org.freertr.cfg.cfgAll;
import org.freertr.tab.tabTime;
import org.freertr.user.userFlash;

/**
 * one log file
 *
 * @author matecsaba
 */
public class logFil {

    private final String logFilNam;

    private long logRotSiz;

    private int logRotTim;

    private int logRotLin;

    private String logRotNam;

    private PrintStream logFilHnd;

    private long logFilSiz;

    private int logFilLin;

    private long logFilTim;

    /**
     * create instance
     *
     * @param fn name of file
     */
    public logFil(String fn) {
        logFilNam = fn;
    }

    public String toString() {
        return logFilNam;
    }

    /**
     * get file name
     *
     * @return name
     */
    public String name() {
        return logFilNam;
    }

    /**
     * start file rotation
     *
     * @param fn name of file
     * @param siz size of file
     * @param tim age of file
     * @param lin line of file
     */
    public void rotate(String fn, long siz, int tim, int lin) {
        logRotNam = fn;
        logRotSiz = siz;
        logRotTim = tim;
        logRotLin = lin;
        if (fn != null) {
            if (fn.length() > 0) {
                return;
            }
        }
        logRotNam = null;
        logRotSiz = 0;
        logRotTim = 0;
        logRotLin = 0;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public String rotate1() {
        if (logRotNam == null) {
            return null;
        }
        String a = logRotSiz + " " + logRotNam;
        if (logRotTim > 0) {
            a = a + " " + logRotTim;
        }
        return a;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public String rotate2() {
        if (logRotNam == null) {
            return null;
        }
        String a = logRotTim + " " + logRotNam;
        if (logRotSiz > 0) {
            a = a + " " + logRotSiz;
        }
        return a;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public String rotateN() {
        return logRotNam;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public int rotateT() {
        return logRotTim;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public long rotateS() {
        return logRotSiz;
    }

    /**
     * get file rotation
     *
     * @return configuration
     */
    public int rotateL() {
        return logRotLin;
    }

    /**
     * open file
     *
     * @param rot rotate
     * @return false if successful, true if error happened
     */
    public synchronized boolean open(boolean rot) {
        if (rot) {
            String a = tabTime.patchFileName(logRotNam, cfgAll.timeZoneName, bits.getTime());
            userFlash.rename(logFilNam, a, true, true);
            logFilTim = bits.getTime();
        }
        try {
            logFilHnd = new PrintStream(new FileOutputStream(logFilNam, true));
            logFilSiz = new File(logFilNam).length();
            logFilLin = (int) (logFilSiz / 80);
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    /**
     * close file
     */
    public synchronized void close() {
        try {
            logFilHnd.flush();
            logFilHnd.close();
        } catch (Exception e) {
        }
    }

    private void doRotate() {
        boolean ned = false;
        if (logRotSiz > 0) {
            ned |= logFilSiz > logRotSiz;
        }
        if (logRotTim > 0) {
            ned |= (bits.getTime() - logFilTim) > logRotTim;
        }
        if (logRotLin > 0) {
            ned |= logFilLin > logRotLin;
        }
        ned &= logRotNam != null;
        if (!ned) {
            return;
        }
        try {
            logFilHnd.close();
        } catch (Exception e) {
        }
        String a = tabTime.patchFileName(logRotNam, cfgAll.timeZoneName, bits.getTime());
        userFlash.rename(logFilNam, a, true, true);
        try {
            logFilHnd = new PrintStream(new FileOutputStream(logFilNam, true));
        } catch (Exception e) {
        }
        logFilSiz = 0;
        logFilLin = 0;
        logFilTim = bits.getTime();
    }

    /**
     * add a line to file
     *
     * @param msg message
     */
    public synchronized void add(String msg) {
        doRotate();
        try {
            logFilHnd.println(msg);
            logFilHnd.flush();
        } catch (Exception e) {
            return;
        }
        logFilSiz += msg.length() + 2;
        logFilLin++;
    }

    /**
     * add a bytes to file
     *
     * @param buf1 first buffer
     * @param ofs1 first offset
     * @param siz1 first size
     * @param buf2 second buffer
     * @param ofs2 second offset
     * @param siz2 second size
     */
    public synchronized void add(byte[] buf1, int ofs1, int siz1, byte[] buf2, int ofs2, int siz2) {
        doRotate();
        try {
            logFilHnd.write(buf1, ofs1, siz1);
            logFilHnd.write(buf2, ofs2, siz2);
            logFilHnd.flush();
        } catch (Exception e) {
            return;
        }
        logFilSiz += siz1 - ofs1;
        logFilSiz += siz2 - ofs2;
        logFilLin++;
    }

}
