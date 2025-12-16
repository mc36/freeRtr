package org.freertr.pipe;

import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.notifier;

/**
 * one side of a pipeline
 *
 * @author matecsaba
 */
public class pipeSide {

    /**
     * line mode type
     */
    public enum modTyp {

        /**
         * nothing
         */
        modeNone,
        /**
         * a cr
         */
        modeCR,
        /**
         * an lf
         */
        modeLF,
        /**
         * a cr followed by an lf
         */
        modeCRLF,
        /**
         * an lf followed by a cr
         */
        modeLFCR,
        /**
         * a cr or an lf
         */
        modeCRorLF,
        /**
         * a cr or an lf and possibly the other
         */
        modeCRtorLF,
        /**
         * a cr and possibly an lf
         */
        modeCRtryLF,
        /**
         * an lf and possibly a cr
         */
        modeLFtryCR,
        /**
         * backspace
         */
        modeBS

    }

    /**
     * get ending
     *
     * @param typ type
     * @return bytes written
     */
    public static byte[] getEnding(modTyp typ) {
        if (typ == null) {
            return null;
        }
        switch (typ) {
            case modeNone:
                return new byte[0];
            case modeCR:
                byte[] buf = new byte[1];
                buf[0] = (byte) 13;
                return buf;
            case modeLF:
                buf = new byte[1];
                buf[0] = (byte) 10;
                return buf;
            case modeBS:
                buf = new byte[1];
                buf[0] = (byte) 8;
                return buf;
            case modeCRLF:
                buf = new byte[2];
                buf[0] = 13;
                buf[1] = 10;
                return buf;
            case modeLFCR:
                buf = new byte[2];
                buf[0] = 10;
                buf[1] = 13;
                return buf;
            default:
                return null;
        }
    }

    /**
     * get type of line termination
     *
     * @param buf value from buffer
     * @param nul value returned on error
     * @return line termination type
     */
    public static modTyp getType(int buf, modTyp nul) {
        switch (buf) {
            case 13:
                return modTyp.modeCR;
            case 10:
                return modTyp.modeLF;
            case 0:
                return modTyp.modeNone;
            case 8:
            case 127:
                return modTyp.modeBS;
            default:
                return nul;
        }
    }

    /**
     * get status of pipeline
     *
     * @param pipe pipe to check
     * @return string describing buffer status
     */
    public static String getStatus(pipeSide pipe) {
        if (pipe == null) {
            return "none";
        }
        return "max=" + pipe.getBufSize() + " rx=" + pipe.ready2rx() + " tx=" + pipe.ready2tx();
    }

    /**
     * line termination at receive all are applicable
     */
    public modTyp lineRx = modTyp.modeNone;

    /**
     * line termination at transmit only 1..4 are applicable
     */
    public modTyp lineTx = modTyp.modeNone;

    /**
     * notifier of this side
     */
    public final notifier notif;

    /**
     * peer side of pipeline
     */
    protected pipeSide peerSideOfPipeLine;

    private final tabGen<pipeSetting> settings; // pipeline settings

    private int timeout = 0; // timeout in milliseconds

    private long activity; // time ot last activity

    private final int headSize; // packet boundary size

    private final byte[] bufD; // bytes

    private final int bufS; // size

    private final Object lck = new Object(); // locker

    private int bufU; // used

    private int bufW; // next write pos

    private int bufR; // next read pos

    private boolean closed; // closed state

    private boolean ready; // ready state

    private final static int blockMaxSiz = 64512;

    /**
     * create new pipeline side
     *
     * @param bufferSize size of one direction buffer
     * @param blockMode set true to keep block boundaries
     * @param stngs settings storage to use
     */
    public pipeSide(int bufferSize, boolean blockMode, tabGen<pipeSetting> stngs) {
        settings = stngs;
        if (blockMode) {
            headSize = 2;
        } else {
            headSize = 0;
        }
        bufS = bufferSize;
        bufD = new byte[bufS];
        bufU = 0;
        bufW = 0;
        closed = false;
        ready = false;
        notif = new notifier();
    }

    /**
     * set timeout
     *
     * @param t timeout
     */
    public void setTime(int t) {
        timeout = t;
        peerSideOfPipeLine.timeout = t;
    }

    /**
     * get timeout
     *
     * @return timeout
     */
    public int getTime() {
        return timeout;
    }

    private void bytePut(int b) {
        bufD[bufW] = (byte) b;
        bufU = bufU + 1;
        bufW = (bufW + 1) % bufS;
    }

    private int byteGet() {
        bufU = bufU - 1;
        int i = bufD[bufR];
        bufR = (bufR + 1) % bufS;
        return i & 0xff;
    }

    private int byteUnGet(int ch) {
        bufU = bufU + 1;
        bufR = bufR - 1;
        if (bufR < 0) {
            bufR = bufS - 1;
        }
        bufD[bufR] = (byte) ch;
        return 1;
    }

    /**
     * nonblocking unget
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int ungetBuf(byte[] buf, int ofs, int len) {
        synchronized (lck) {
            if (len < 0) {
                return pipeLine.wontWork;
            }
            if (headSize > 0) {
                return pipeLine.wontWork;
            }
            if (len > bufS) {
                return pipeLine.wontWork;
            }
            for (int i = len - 1; i >= 0; i--) {
                byteUnGet(buf[ofs + i]);
            }
        }
        doInact(true);
        notif.wakeup();
        return len;
    }

    private int bufPut(byte[] buf, int ofs, int len) {
        synchronized (lck) {
            if (len < 0) {
                return pipeLine.wontWork;
            }
            if (len > (bufS - headSize)) {
                return pipeLine.wontWork;
            }
            if ((bufS - bufU - headSize) < len) {
                if (isClosed() != 0) {
                    return pipeLine.wontWork;
                }
                return pipeLine.tryLater;
            }
            if (headSize != 0) {
                if (len > blockMaxSiz) {
                    return pipeLine.wontWork;
                }
                bytePut(len >>> 8);
                bytePut(len & 0xff);
            }
            if ((bufW + len) < bufS) {
                bits.byteCopy(buf, ofs, bufD, bufW, len);
                bufW += len;
                bufU += len;
            } else {
                for (int i = 0; i < len; i++) {
                    bytePut(buf[ofs + i]);
                }
            }
            return len;
        }
    }

    private int bufGet(byte[] buf, int ofs, int len, boolean restorePos) {
        synchronized (lck) {
            if (len < 0) {
                return pipeLine.wontWork;
            }
            if (bufU < 1) {
                if (isClosed() != 0) {
                    return pipeLine.wontWork;
                }
                return pipeLine.tryLater;
            }
            int ou = bufU;
            int op = bufR;
            if (headSize > 0) {
                if (bufU < 2) {
                    return pipeLine.tryLater;
                }
                int i = byteGet() << 8;
                i |= byteGet();
                if (i > len) {
                    bufU = ou;
                    bufR = op;
                    return pipeLine.wontWork;
                }
                len = i;
            } else {
                if (len > bufU) {
                    len = bufU;
                }
            }
            if ((bufR + len) < bufS) {
                bits.byteCopy(bufD, bufR, buf, ofs, len);
                bufR += len;
                bufU -= len;
            } else {
                for (int i = 0; i < len; i++) {
                    buf[ofs + i] = (byte) byteGet();
                }
            }
            if (restorePos) {
                bufU = ou;
                bufR = op;
            }
            return len;
        }
    }

    /**
     * update inactivity timer if needed
     *
     * @param active set true if activity happened, false otherwise
     */
    protected void doInact(boolean active) {
        long tim = bits.getTime();
        if (active) {
            synchronized (lck) {
                activity = tim;
                peerSideOfPipeLine.activity = tim;
            }
            return;
        }
        boolean b = false;
        synchronized (lck) {
            b |= (timeout > 0) && ((tim - activity > timeout));
            b |= (peerSideOfPipeLine.timeout > 0) && ((tim - activity > peerSideOfPipeLine.timeout));
        }
        if (!b) {
            return;
        }
        setClose();
    }

    /**
     * clear receiver side
     *
     * @return number of bytes skipped
     */
    protected int flushRecvSide() {
        int i;
        synchronized (lck) {
            i = bufU;
            bufU = 0;
            bufR = bufW;
        }
        doInact(i > 0);
        peerSideOfPipeLine.notif.wakeup();
        return i;
    }

    /**
     * check if pipeline in block mode
     *
     * @return true means yes, false means no
     */
    public boolean isBlockMode() {
        return (headSize != 0);
    }

    /**
     * close this side
     *
     * @return false if now done, true already
     */
    public boolean setClose() {
        if (closed) {
            return true;
        }
        doInact(true);
        closed = true;
        notif.wakeup();
        peerSideOfPipeLine.notif.wakeup();
        return false;
    }

    /**
     * test if the any side of pipe was closed
     *
     * @return 0=no, 1=this side, 2=other side, 3=both sides
     */
    public int isClosed() {
        doInact(false);
        int i = 0;
        if (closed) {
            i |= 1;
        }
        if (peerSideOfPipeLine.closed) {
            i |= 2;
        }
        return i;
    }

    /**
     * close this side
     *
     * @return false if now done, true already
     */
    public boolean setReady() {
        if (ready) {
            return true;
        }
        doInact(true);
        ready = true;
        notif.wakeup();
        peerSideOfPipeLine.notif.wakeup();
        return false;
    }

    /**
     * test if the any side of pipe is ready
     *
     * @return 0=no, 1=this side, 1=other side, 3=both sides
     */
    public int isReady() {
        doInact(false);
        int i = 0;
        if (ready) {
            i |= 1;
        }
        if (peerSideOfPipeLine.ready) {
            i |= 2;
        }
        return i;
    }

    /**
     * get number of bytes free in tx buffer
     *
     * @return number of bytes free in tx buffer
     */
    public int ready2tx() {
        doInact(false);
        if (isClosed() != 0) {
            return pipeLine.wontWork;
        }
        return bufS - peerSideOfPipeLine.ready2rx();
    }

    /**
     * get number of bytes used in rx buffer
     *
     * @return number of bytes used in rx buffer
     */
    public int ready2rx() {
        synchronized (lck) {
            return bufU - headSize;
        }
    }

    /**
     * maximum number of bytes in buffer
     *
     * @return bytes in buffer
     */
    public int getBufSize() {
        return bufS;
    }

    /**
     * wait until pipeline comes up
     *
     * @param msec millisecs to wait, 0 means forever
     * @return false on success, true on error
     */
    public boolean wait4ready(int msec) {
        for (int i = 0; i < 10; i++) {
            doInact(false);
            if (isClosed() != 0) {
                return true;
            }
            if (peerSideOfPipeLine.ready) {
                return false;
            }
            notif.misleep(msec / 10);
        }
        return true;
    }

    /**
     * nonblocking tx
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int nonBlockPut(byte[] buf, int ofs, int len) {
        int i = peerSideOfPipeLine.bufPut(buf, ofs, len);
        doInact(i >= 0);
        if (i > 0) {
            peerSideOfPipeLine.notif.wakeup();
        }
        return i;
    }

    /**
     * nondestructive rx read but will be readable next time again
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int nonDestructiveGet(byte[] buf, int ofs, int len) {
        return bufGet(buf, ofs, len, true);
    }

    /**
     * destructive skip delete bytes without read
     *
     * @param len length
     * @return bytes done, or negative error code
     */
    public int nonBlockSkip(int len) {
        byte[] buf = new byte[len + 16];
        int i = bufGet(buf, 0, len, false);
        doInact(i >= 0);
        if (i > 0) {
            peerSideOfPipeLine.notif.wakeup();
        }
        return i;
    }

    /**
     * nonblocking rx
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int nonBlockGet(byte[] buf, int ofs, int len) {
        int i = bufGet(buf, ofs, len, false);
        doInact(i >= 0);
        if (i > 0) {
            peerSideOfPipeLine.notif.wakeup();
        }
        return i;
    }

    /**
     * blocking tx
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int blockingPut(byte[] buf, int ofs, int len) {
        for (;;) {
            int i = nonBlockPut(buf, ofs, len);
            if (i == pipeLine.tryLater) {
                notif.misleep(timeout);
                continue;
            }
            return i;
        }
    }

    /**
     * blocking rx
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int blockingGet(byte[] buf, int ofs, int len) {
        for (;;) {
            int i = nonBlockGet(buf, ofs, len);
            if (i == pipeLine.tryLater) {
                notif.misleep(timeout);
                continue;
            }
            return i;
        }
    }

    /**
     * blocking skip
     *
     * @param len bytes to do
     * @return bytes done, or negative error code
     */
    public int blockingSkip(int len) {
        byte[] buf = new byte[len];
        for (;;) {
            int i = nonBlockGet(buf, 0, len);
            if (i == pipeLine.tryLater) {
                notif.misleep(timeout);
                continue;
            }
            return i;
        }
    }

    /**
     * blocking tx this will do in more blocks if needed
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done
     */
    public int morePut(byte[] buf, int ofs, int len) {
        final int max = 1024;
        int done = 0;
        for (; len > 0;) {
            int o = len;
            if (o > max) {
                o = max;
            }
            int i = blockingPut(buf, ofs, o);
            if (i == pipeLine.wontWork) {
                break;
            }
            if (i < 1) {
                continue;
            }
            done += i;
            ofs += i;
            len -= i;
        }
        return done;
    }

    /**
     * blocking rx this will do in more blocks if needed
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param len bytes to do
     * @return bytes done
     */
    public int moreGet(byte[] buf, int ofs, int len) {
        int done = 0;
        for (; len > 0;) {
            int i = blockingGet(buf, ofs, len);
            if (i == pipeLine.wontWork) {
                break;
            }
            if (i < 1) {
                continue;
            }
            done += i;
            ofs += i;
            len -= i;
        }
        return done;
    }

    /**
     * blocking skip this will do in more blocks if needed
     *
     * @param len bytes to do
     * @return bytes done
     */
    public int moreSkip(int len) {
        int done = 0;
        for (; len > 0;) {
            int i = blockingSkip(len);
            if (i == pipeLine.wontWork) {
                break;
            }
            if (i < 1) {
                continue;
            }
            done += i;
            len -= i;
        }
        return done;
    }

    /**
     * best effort put string
     *
     * @param s string to put
     */
    public void strPut(String s) {
        byte[] buf = s.getBytes();
        morePut(buf, 0, buf.length);
    }

    /**
     * get string
     *
     * @param len bytes to get
     * @return string read, null on error
     */
    public String strGet(int len) {
        byte[] buf = new byte[len];
        int o = moreGet(buf, 0, len);
        if (o < 1) {
            return null;
        }
        return new String(buf, 0, o);
    }

    /**
     * get a character
     *
     * @param quest question to put
     * @param need needed characters
     * @return character read
     */
    public String strChr(String quest, String need) {
        for (;;) {
            strPut(quest);
            byte[] buf = new byte[1];
            int o = moreGet(buf, 0, buf.length);
            if (o != buf.length) {
                return "";
            }
            String s = new String(buf, 0, buf.length);
            if (need.indexOf(s) >= 0) {
                return s;
            }
            buf = getEnding(modTyp.modeCRLF);
            morePut(buf, 0, buf.length);
        }
    }

    /**
     * best effort put line
     *
     * @param s string to write
     */
    public void linePut(String s) {
        byte[] buf = s.getBytes();
        int i = morePut(buf, 0, buf.length);
        if (i < buf.length) {
            return;
        }
        buf = getEnding(lineTx);
        if (buf.length < 1) {
            return;
        }
        morePut(buf, 0, buf.length);
    }

    private boolean gotOneChar(modTyp last, modTyp curr) {
        byte[] buf = new byte[4];
        switch (lineRx) {
            case modeCR:
            case modeLF:
                if (curr == lineRx) {
                    return true;
                }
                break;
            case modeCRLF:
                if ((curr == modTyp.modeLF) && (last == modTyp.modeCR)) {
                    return true;
                }
                break;
            case modeLFCR:
                if ((curr == modTyp.modeCR) && (last == modTyp.modeLF)) {
                    return true;
                }
                break;
            case modeCRorLF:
                if ((curr == modTyp.modeLF) || (curr == modTyp.modeCR)) {
                    return true;
                }
                break;
            case modeCRtorLF:
                if ((curr != modTyp.modeLF) && (curr != modTyp.modeCR)) {
                    break;
                }
                if (nonDestructiveGet(buf, 0, 1) != 1) {
                    return true;
                }
                if (curr == modTyp.modeLF) {
                    curr = modTyp.modeCR;
                } else {
                    curr = modTyp.modeLF;
                }
                if (getType(buf[0], modTyp.modeNone) == curr) {
                    nonBlockSkip(1);
                }
                return true;
            case modeCRtryLF:
                if (curr != modTyp.modeCR) {
                    break;
                }
                if (nonDestructiveGet(buf, 0, 1) != 1) {
                    return true;
                }
                if (getType(buf[0], modTyp.modeNone) == modTyp.modeLF) {
                    nonBlockSkip(1);
                }
                return true;
            case modeLFtryCR:
                if (curr != modTyp.modeLF) {
                    break;
                }
                if (nonDestructiveGet(buf, 0, 1) != 1) {
                    return true;
                }
                if (getType(buf[0], modTyp.modeNone) == modTyp.modeCR) {
                    nonBlockSkip(1);
                }
                return true;
            default:
                break;
        }
        return false;
    }

    /**
     * read one line
     *
     * @param editing editing mode 1=nothing 2=same 3=masked 0x10=process
     * backspace 0x20=newline after, 0x40=binary, 0x80=nonblock
     * @return string read
     */
    public String lineGet(int editing) {
        String s = "";
        modTyp last = null;
        for (;;) {
            byte[] buf = new byte[4];
            int i;
            if ((editing & 0x80) == 0) {
                i = blockingGet(buf, 0, 1);
                if (i == pipeLine.tryLater) {
                    continue;
                }
            } else {
                i = nonBlockGet(buf, 0, 1);
            }
            if (i != 1) {
                break;
            }
            int chr = buf[0] & 0xff;
            modTyp curr = getType(chr, modTyp.modeNone);
            if ((curr == modTyp.modeBS) && ((editing & 0x10) != 0)) {
                i = s.length() - 1;
                if (i < 0) {
                    continue;
                }
                s = s.substring(0, i);
                switch (editing & 0xf) {
                    case 2:
                    case 3:
                        buf = new byte[3];
                        buf[0] = getEnding(modTyp.modeBS)[0];
                        buf[1] = 32;
                        buf[2] = buf[0];
                        blockingPut(buf, 0, buf.length);
                        break;
                    default:
                        break;
                }
                continue;
            }
            if (gotOneChar(last, curr)) {
                break;
            }
            last = curr;
            if ((editing & 0x40) == 0) {
                if (chr < 32) {
                    chr = 32;
                }
                if (chr > 127) {
                    chr = 32;
                }
            }
            s += (char) chr;
            switch (editing & 0xf) {
                case 1:
                    break;
                case 2:
                    buf[0] = (byte) chr;
                    blockingPut(buf, 0, 1);
                    break;
                case 3:
                    buf[0] = 42;
                    blockingPut(buf, 0, 1);
                    break;
                default:
                    break;
            }
        }
        if ((editing & 0x20) != 0) {
            linePut("");
        }
        switch (lineRx) {
            case modeCRLF:
            case modeLFCR:
                int i = s.length() - 1;
                if (i < 0) {
                    i = 0;
                }
                s = s.substring(0, i);
                break;
            default:
                break;
        }
        return s;
    }

    /**
     * read packet from pipeline
     *
     * @param pck packet where to store data
     * @param len maximum length of data
     * @param blocking set true of blocking read
     * @return packet readed, null=nothing
     */
    public packHolder readPacket(packHolder pck, int len, boolean blocking) {
        if (len > packHolder.maxHead) {
            len = packHolder.maxHead;
        }
        pck.putStart();
        int i;
        if (blocking) {
            i = 143;
        } else {
            i = 142;
        }
        i = pck.pipeRecv(this, 0, len, i);
        if (i < 1) {
            return null;
        }
        return pck;
    }

    /**
     * read packet from pipeline
     *
     * @param blocking set true of blocking read
     * @return packet readed, null=nothing
     */
    public packHolder readPacket(boolean blocking) {
        return readPacket(new packHolder(true, true), 0, blocking);
    }

    /**
     * put one setting, overwrite if already exists
     *
     * @param nam name
     * @param val value, null to remove
     */
    public void settingsPut(int nam, Object val) {
        if (val == null) {
            return;
        }
        pipeSetting ntry = new pipeSetting(nam);
        ntry.data = val;
        synchronized (settings) {
            settings.put(ntry);
        }
    }

    /**
     * add one setting, do nothing if already exists
     *
     * @param nam name
     * @param val value
     */
    public void settingsAdd(int nam, Object val) {
        if (val == null) {
            return;
        }
        pipeSetting ntry = new pipeSetting(nam);
        ntry.data = val;
        synchronized (settings) {
            settings.add(ntry);
        }
    }

    /**
     * copy all the settings
     *
     * @param o other pipe to copy
     */
    public void settingsCopy(pipeSide o) {
        synchronized (settings) {
            settings.clear();
            for (int i = 0; i < o.settings.size(); i++) {
                pipeSetting ntry = o.settings.get(i);
                if (ntry == null) {
                    continue;
                }
                settings.put(ntry);
            }
        }
    }

    /**
     * get one setting
     *
     * @param <T> type of object
     * @param nam name
     * @param def default
     * @return value, default if not found
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> T settingsGet(int nam, T def) {
        pipeSetting ntry = new pipeSetting(nam);
        synchronized (settings) {
            ntry = settings.find(ntry);
        }
        if (ntry == null) {
            return def;
        }
        try {
            T fin = (T) ntry.data;
            return fin;
        } catch (Exception e) {
            return def;
        }
    }

}
