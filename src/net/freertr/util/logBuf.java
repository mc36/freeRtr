package net.freertr.util;

import java.util.ArrayList;
import java.util.List;

/**
 * one log buffer
 *
 * @author matecsaba
 */
public class logBuf {

    private String[] logBufLst;

    private int logBufPos = 0;

    /**
     * create instance
     *
     * @param siz initial capacity
     */
    public logBuf(int siz) {
        logBufLst = new String[siz];
    }

    /**
     * get size of buffer
     *
     * @param o buffer to check
     * @return lines, -1 if no buffer given
     */
    public static int getSize(logBuf o) {
        if (o == null) {
            return -1;
        }
        return o.logBufLst.length;
    }

    /**
     * get contents of buffer
     *
     * @param o buffer to check
     * @return lines, null if no buffer given
     */
    public static List<String> getLines(logBuf o) {
        if (o == null) {
            return null;
        }
        return o.read();
    }

    /**
     * get size of buffer
     *
     * @return lines
     */
    public int size() {
        return logBufLst.length;
    }

    /**
     * add a line to buffer
     *
     * @param msg message
     */
    public void add(String msg) {
        synchronized (logBufLst) {
            logBufPos = (logBufPos + 1) % logBufLst.length;
            logBufLst[logBufPos] = msg;
        }
    }

    /**
     * read logging buffer
     *
     * @param num number of lines
     * @return list of strings
     */
    public List<String> read(int num) {
        List<String> l = new ArrayList<String>();
        if (num > logBufLst.length) {
            num = logBufLst.length;
        }
        int o = logBufPos - num + 1;
        if (o < 0) {
            o = logBufLst.length + o;
        }
        if (o < 0) {
            num = num - o;
            o = 0;
        }
        synchronized (logBufLst) {
            for (int i = 0; i < num; i++) {
                String s = logBufLst[o];
                o++;
                if (o >= logBufLst.length) {
                    o = 0;
                }
                if (s == null) {
                    continue;
                }
                l.add(s);
            }
        }
        return l;
    }

    /**
     * read logging buffer
     *
     * @return list of strings
     */
    public List<String> read() {
        List<String> l = new ArrayList<String>();
        synchronized (logBufLst) {
            for (int i = logBufPos + 1; i < logBufLst.length; i++) {
                String s = logBufLst[i];
                if (s == null) {
                    continue;
                }
                l.add(s);
            }
            for (int i = 0; i <= logBufPos; i++) {
                String s = logBufLst[i];
                if (s == null) {
                    continue;
                }
                l.add(s);
            }
        }
        return l;
    }

    /**
     * clear logging buffer
     */
    public void clear() {
        synchronized (logBufLst) {
            for (int i = 0; i < logBufLst.length; i++) {
                logBufLst[i] = null;
            }
        }
    }

    /**
     * resize buffer logger
     *
     * @param siz size of buffer in lines
     */
    public void resize(int siz) {
        List<String> old = read();
        final int min = 16;
        if (siz < min) {
            siz = min;
        }
        synchronized (logBufLst) {
            String[] nw = new String[siz];
            int o = 0;
            for (int i = 0; i < old.size(); i++) {
                o = (o + 1) % siz;
                nw[o] = old.get(i);
            }
            logBufPos = o;
            logBufLst = nw;
        }
    }

}
