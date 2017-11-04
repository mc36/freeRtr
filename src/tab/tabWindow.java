package tab;

/**
 * represents one window handler
 *
 * @author matecsaba
 */
public class tabWindow {

    /**
     * usage bitmap
     */
    protected final byte mapDat[];

    private final Object mapPay[];

    private int mapSeq;

    private int mapPos;

    /**
     * initialize windowing
     *
     * @param siz window size
     */
    public tabWindow(int siz) {
        mapDat = new byte[siz];
        mapPay = new Object[siz];
    }

    public String toString() {
        int o = 0;
        for (int i = 0; i < mapDat.length; i++) {
            if (mapDat[i] == 0) {
                o++;
            }
        }
        String s = seq1st() + "-" + seqLst() + ", " + o + " miss";
        return s;
    }

    /**
     * dump buffer
     *
     * @return buffer dumped
     */
    public String dump() {
        String s = "";
        for (int i = mapPos + 1; i < mapDat.length; i++) {
            s += mapDat[i];
        }
        for (int i = 0; i < mapPos; i++) {
            s += mapDat[i];
        }
        return s;
    }

    /**
     * get size of window
     *
     * @param w window to check
     * @return number of packets
     */
    public static int getSize(tabWindow w) {
        if (w == null) {
            return 0;
        }
        return w.mapDat.length;
    }

    private int seq2pos(int seq) {
        int i = seq - mapSeq;
        if (i > 0) {
            return -1;
        }
        if (i <= -mapDat.length) {
            return -1;
        }
        i = (i + mapPos) % mapDat.length;
        if (i < 0) {
            i += mapDat.length;
        }
        return i;
    }

    /**
     * got data packet
     *
     * @param seq sequence number
     * @return true if already got, false if not
     */
    public boolean gotDat(int seq) {
        synchronized (mapDat) {
            if (seq > mapSeq) {
                if (seq >= (mapSeq + mapDat.length)) {
                    return true;
                }
                gotSet(seq);
                mapDat[mapPos] = 1;
                mapPay[mapPos] = null;
                return false;
            }
            int i = seq2pos(seq);
            if (i < 0) {
                return true;
            }
            if (mapDat[i] != 0) {
                return true;
            }
            mapDat[i] = 1;
            mapPay[i] = null;
            return false;
        }
    }

    /**
     * got sequence number
     *
     * @param seq sequence number
     */
    public void gotSet(int seq) {
        synchronized (mapDat) {
            int m = seq - mapSeq;
            if ((m >= mapDat.length) || (m < 0)) {
                mapPos = 0;
                mapSeq = seq;
                for (int i = 0; i < mapDat.length; i++) {
                    mapDat[i] = 0;
                    mapPay[i] = null;
                }
                return;
            }
            for (int i = 0; i < m; i++) {
                mapSeq++;
                mapPos = (mapPos + 1) % mapDat.length;
                mapDat[mapPos] = 0;
                mapPay[mapPos] = null;
            }
        }
    }

    /**
     * set payload
     *
     * @param seq sequence number
     * @param dat data to store
     */
    public void paySet(int seq, Object dat) {
        synchronized (mapDat) {
            int i = seq2pos(seq);
            if (i < 0) {
                return;
            }
            mapPay[i] = dat;
        }
    }

    /**
     * get payload
     *
     * @param seq sequence number
     * @return payload
     */
    public Object payGet(int seq) {
        synchronized (mapDat) {
            int i = seq2pos(seq);
            if (i < 0) {
                return null;
            }
            return mapPay[i];
        }
    }

    /**
     * clear payload
     *
     * @param seq sequence number
     */
    public void payClr(int seq) {
        synchronized (mapDat) {
            int i = seq2pos(seq);
            if (i < 0) {
                return;
            }
            mapPay[i] = null;
        }
    }

    /**
     * get first sequence number
     *
     * @return seq number
     */
    public int seq1st() {
        return mapSeq - mapDat.length;
    }

    /**
     * get last sequence number
     *
     * @return seq number
     */
    public int seqLst() {
        return mapSeq;
    }

}
