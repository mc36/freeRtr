package org.freertr.tab;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGen<T extends Comparable<? super T>> {

    private final static int thrsElm = 1000;

    private final static int thrsBlkA = 100;

    private final static int thrsBlkD = 200;

    @SuppressWarnings({"unchecked", "rawtypes"})
    private T[][] valD = (T[][]) new Comparable[1][thrsElm]; // values

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
    public tabGen() {
        doArrays(thrsBlkA);
    }

    /**
     * clone one generic table
     *
     * @param old where from copy
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public tabGen(tabGen<T> old) {
        synchronized (old.lck) {
            begD = old.begD;
            sizD = old.sizD;
            valD = old.valD;
            valN = old.valN;
            blkN = old.blkN;
            doArrays(valD.length);
            for (int o = 0; o < blkN; o++) {
                T[] rowO = valD[o];
                T[] rowN = (T[]) new Comparable[rowO.length];
                System.arraycopy(rowO, 0, rowN, 0, sizD[o]);
                valD[o] = rowN;
            }
        }
    }

    /**
     * get table info
     *
     * @return info
     */
    public String tableInfo() {
        synchronized (lck) {
            return "len=" + valN + ",blk=" + blkN + ",avg=" + (valN / blkN);
        }
    }

    /**
     * delete all values
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public void clear() {
        synchronized (lck) {
            valN = 0;
            blkN = 1;
            valD[0] = (T[]) new Comparable[thrsElm];
            begD[0] = 0;
            sizD[0] = 0;
            lstB = 0;
            for (int i = 1; i < valD.length; i++) {
                valD[i] = null;
            }
            if (valD.length < thrsBlkD) {
                return;
            }
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
        if (idx < 0) {
            return null;
        }
        synchronized (lck) {
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
     * find one value
     *
     * @param val value to find
     * @return index in list, negative if not found
     */
    public int index(T val) {
        synchronized (lck) {
            return doFind(val);
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

    private int doFind(T val) {
        int lower = 0;
        int upper = blkN - 1;
        if (upper > 0) {
            int cmp = valD[lstB][0].compareTo(val);
            if (cmp >= 0) {
                upper = lstB;
            }
            if (cmp <= 0) {
                lower = lstB;
                cmp = sizD[lstB] - 1;
                cmp = valD[lstB][cmp].compareTo(val);
                if (cmp >= 0) {
                    upper = lstB;
                }
            }
            while (lower <= upper) {
                lstB = (lower + upper) >>> 1;
                cmp = valD[lstB][0].compareTo(val);
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
        T[] rowD = valD[lstB];
        if (upper >= 0) {
            if (lstB <= 0) {
                int cmp = rowD[0].compareTo(val);
                if (cmp > 0) {
                    lstI = 0;
                    return -rowB - 1;
                }
            }
            if (lstB >= (blkN - 1)) {
                int cmp = rowD[upper].compareTo(val);
                if (cmp < 0) {
                    lstI = upper;
                    return -rowB - (upper + 1) - 1;
                }
            }
        }
        while (lower <= upper) {
            lstI = (lower + upper) >>> 1;
            int cmp = rowD[lstI].compareTo(val);
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

    private T doGet(int idx) {
        if (blkN <= 1) {
            lstI = idx;
            return valD[0][idx];
        }
        if (isCache(idx) != 0) {
            doCache(idx);
        }
        return valD[lstB][lstI];
    }

    private void doDel() {
        T[] rowD = valD[lstB];
        int siz = sizD[lstB] - 1;
        System.arraycopy(rowD, lstI + 1, rowD, lstI, siz - lstI);
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
        System.arraycopy(valD, lstB + 1, valD, lstB, blkN - lstB);
        System.arraycopy(sizD, lstB + 1, sizD, lstB, blkN - lstB);
        System.arraycopy(begD, lstB + 1, begD, lstB, blkN - lstB);
        valD[blkN] = null;
        lstB = 0;
        if (blkN > (valD.length - thrsBlkD)) {
            return;
        }
        doArrays(blkN + thrsBlkA);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void doIns(int idx, T val) {
        if (isCache(idx) != 0) {
            doCache(idx);
        }
        int siz = sizD[lstB];
        T[] row = valD[lstB];
        System.arraycopy(row, lstI, row, lstI + 1, siz - lstI);
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
        System.arraycopy(valD, lstB, valD, lstB + 1, blkN - lstB);
        System.arraycopy(sizD, lstB, sizD, lstB + 1, blkN - lstB);
        System.arraycopy(begD, lstB, begD, lstB + 1, blkN - lstB);
        blkN++;
        T[] rowO = valD[lstB];
        int mid = siz / 2;
        T[] rowN = (T[]) new Comparable[rowO.length];
        System.arraycopy(rowO, 0, rowN, 0, mid);
        valD[lstB] = rowN;
        sizD[lstB] = mid;
        rowN = (T[]) new Comparable[rowO.length];
        siz -= mid;
        System.arraycopy(rowO, mid, rowN, 0, siz);
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

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void doArrays(int siz) {
        int[] begF = new int[siz];
        int[] sizF = new int[siz];
        T[][] valF = (T[][]) new Comparable[siz][];
        System.arraycopy(valD, 0, valF, 0, blkN);
        System.arraycopy(sizD, 0, sizF, 0, blkN);
        System.arraycopy(begD, 0, begF, 0, blkN);
        begD = begF;
        sizD = sizF;
        valD = valF;
    }

}
