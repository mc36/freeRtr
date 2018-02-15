package tab;

import java.util.Comparator;
import util.bits;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGenV2<T extends Comparator<? super T>> {

    private final static int thrsElm = 1000;

    private final static int thrsBlkA = 100;

    private final static int thrsBlkD = 200;

    private Object[][] valD = new Object[1][thrsElm]; // values

    private int[] begD = new int[1]; // beginnings of blocks

    private int[] sizD = new int[1]; // sizes of blocks

    private int valN = 0; // number of values

    private int blkN = 1; // number of blocks

    private int lstB = 0; // last block accessed

    private int lstI = 0; // last index in block

    private final Object lck = new Object(); // locker

    /**
     * create one generic table
     */
    public tabGenV2() {
        doArrays(thrsBlkA);
    }

    /**
     * clone one generic table
     *
     * @param old where from copy
     */
    public tabGenV2(tabGenV2<T> old) {
        synchronized (old.lck) {
            begD = old.begD;
            sizD = old.sizD;
            valD = old.valD;
            valN = old.valN;
            blkN = old.blkN;
            doArrays(valD.length);
            for (int o = 0; o < blkN; o++) {
                Object[] rowO = valD[o];
                Object[] rowN = new Object[rowO.length];
                bits.objCopy(rowO, 0, rowN, 0, sizD[o]);
                valD[o] = rowN;
            }
        }
    }

    /**
     * optimize for lookup
     */
    public void optimize4lookup() {
        synchronized (lck) {
            Object[] row = new Object[valN];
            int pos = 0;
            for (int o = 0; o < blkN; o++) {
                bits.objCopy(valD[o], 0, row, pos, sizD[o]);
                pos += sizD[o];
            }
            lstB = 0;
            blkN = 1;
            valD = new Object[1][0];
            valD[0] = row;
            sizD[0] = valN;
            begD[0] = 0;
        }
    }

    /**
     * get table info
     *
     * @return info
     */
    public String getTableInfo() {
        synchronized (lck) {
            return "len=" + valN + ",blk=" + blkN + ",avg=" + (valN / blkN);
        }
    }

    /**
     * delete all values
     */
    public void clear() {
        synchronized (lck) {
            valN = 0;
            blkN = 1;
            valD[0] = new Object[thrsElm];
            begD[0] = 0;
            sizD[0] = 0;
            lstB = 0;
            doArrays(thrsBlkA);
        }
    }

    /**
     * get size of list
     *
     * @return size of list
     */
    public int size() {
        synchronized (lck) {
            return valN;
        }
    }

    /**
     * get value
     *
     * @param idx sequence number
     * @return read value, null if not found
     */
    public T get(int idx) {
        synchronized (lck) {
            if (idx < 0) {
                return null;
            }
            if (idx >= valN) {
                return null;
            }
            return doGet(idx);
        }
    }

    /**
     * find one value
     *
     * @param val value to find
     * @return value in list, null if not found
     */
    public T find(T val) {
        synchronized (lck) {
            int i = doFind(val);
            if (i < 0) {
                return null;
            }
            return doGet(i);
        }
    }

    /**
     * delete value
     *
     * @param val value to delete
     * @return value deleted, null if not found
     */
    public T del(T val) {
        synchronized (lck) {
            int i = doFind(val);
            if (i < 0) {
                return null;
            }
            T old = doGet(i);
            doDel();
            return old;
        }
    }

    /**
     * add one value, do not update if already
     *
     * @param val value to add
     * @return null on addition, old value if already exists
     */
    public T add(T val) {
        synchronized (lck) {
            int i = doFind(val);
            if (i >= 0) {
                return doGet(i);
            }
            doIns(-i - 1, val);
            return null;
        }
    }

    /**
     * put to list, update if already
     *
     * @param val value to put
     * @return old value if updated, null if added
     */
    public T put(T val) {
        synchronized (lck) {
            int i = doFind(val);
            if (i < 0) {
                doIns(-i - 1, val);
                return null;
            }
            T old = doGet(i);
            valD[lstB][lstI] = val;
            return old;
        }
    }

    @SuppressWarnings("unchecked")
    private int doFind(T val) {
        int lower = 0;
        int upper = blkN - 1;
        if (upper > 0) {
            while (lower <= upper) {
                lstB = (lower + upper) >>> 1;
                int cmp = val.compare((T) valD[lstB][0], val);
                if (cmp < 0) {
                    lower = lstB + 1;
                    continue;
                }
                if (cmp > 0) {
                    upper = lstB - 1;
                    continue;
                }
                lstI = 0;
                return begD[lstB];
            }
            lstB = lower - 1;
            if (lstB < 0) {
                lstB = 0;
            }
        } else {
            lstB = 0;
        }
        lower = 0;
        upper = sizD[lstB] - 1;
        int rowB = begD[lstB];
        Object[] rowD = valD[lstB];
        while (lower <= upper) {
            lstI = (lower + upper) >>> 1;
            int cmp = val.compare((T) rowD[lstI], val);
            if (cmp < 0) {
                lower = lstI + 1;
                continue;
            }
            if (cmp > 0) {
                upper = lstI - 1;
                continue;
            }
            return rowB + lstI;
        }
        return -rowB - lower - 1;
    }

    private void doCache(int idx) {
        int lower = 0;
        int upper = blkN - 1;
        while (lower <= upper) {
            lstB = (lower + upper) >>> 1;
            int cmp = isCache(idx);
            if (cmp < 0) {
                upper = lstB - 1;
                continue;
            }
            if (cmp > 0) {
                lower = lstB + 1;
                continue;
            }
            return;
        }
        lstB = blkN - 1;
        lstI = sizD[lstB];
    }

    private int isCache(int idx) {
        int beg = begD[lstB];
        if (idx < beg) {
            return -1;
        }
        if (idx >= (beg + sizD[lstB])) {
            return +1;
        }
        lstI = idx - beg;
        return 0;
    }

    @SuppressWarnings("unchecked")
    private T doGet(int idx) {
        if (blkN <= 1) {
            lstI = idx;
            return (T) valD[0][idx];
        }
        if (isCache(idx) != 0) {
            doCache(idx);
        }
        return (T) valD[lstB][lstI];
    }

    private void doDel() {
        Object[] rowD = valD[lstB];
        int siz = sizD[lstB] - 1;
        bits.objCopy(rowD, lstI + 1, rowD, lstI, siz - lstI);
        rowD[siz] = null;
        sizD[lstB] = siz;
        valN--;
        doBegs(lstB + 1, -1);
        if (siz > 0) {
            return;
        }
        if (blkN <= 1) {
            return;
        }
        blkN--;
        for (int i = lstB; i < blkN; i++) {
            valD[i] = valD[i + 1];
            sizD[i] = sizD[i + 1];
            begD[i] = begD[i + 1];
        }
        valD[blkN] = null;
        lstB = 0;
        if (blkN > (valD.length - thrsBlkD)) {
            return;
        }
        doArrays(blkN + thrsBlkA);
    }

    private void doIns(int idx, T val) {
        if (isCache(idx) != 0) {
            doCache(idx);
        }
        int siz = sizD[lstB];
        Object[] row = valD[lstB];
        bits.objCopy(row, lstI, row, lstI + 1, siz - lstI);
        siz++;
        row[lstI] = val;
        sizD[lstB] = siz;
        valN++;
        doBegs(lstB + 1, +1);
        if (siz < thrsElm) {
            return;
        }
        if (blkN >= valD.length) {
            doArrays(blkN + thrsBlkA);
        }
        for (int i = blkN; i > lstB; i--) {
            valD[i] = valD[i - 1];
            begD[i] = begD[i - 1];
            sizD[i] = sizD[i - 1];
        }
        blkN++;
        Object[] rowO = valD[lstB];
        int mid = siz / 2;
        Object[] rowN = new Object[rowO.length];
        bits.objCopy(rowO, 0, rowN, 0, mid);
        valD[lstB] = rowN;
        sizD[lstB] = mid;
        rowN = new Object[rowO.length];
        siz -= mid;
        bits.objCopy(rowO, mid, rowN, 0, siz);
        lstB++;
        valD[lstB] = rowN;
        sizD[lstB] = siz;
        begD[lstB] = begD[lstB - 1] + mid;
    }

    private void doBegs(int beg, int add) {
        for (int i = beg; i < blkN; i++) {
            begD[i] += add;
        }
    }

    private void doArrays(int siz) {
        int[] begF = new int[siz];
        int[] sizF = new int[siz];
        Object[][] valF = new Object[siz][];
        for (int i = 0; i < blkN; i++) {
            sizF[i] = sizD[i];
            valF[i] = valD[i];
            begF[i] = begD[i];
        }
        begD = begF;
        sizD = sizF;
        valD = valF;
    }

}
