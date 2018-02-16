package util;

/**
 * one synchronized integer
 *
 * @author matecsaba
 */
public class syncInt {

    private final Object lck = new Object(); // locker

    private int val = 0; // value

    private int upd = 0; // version

    /**
     * create one instance
     *
     * @param v value
     */
    public syncInt(int v) {
        val = v;
    }

    public String toString() {
        return "" + get();
    }

    /**
     * get value
     *
     * @return value
     */
    public int get() {
        synchronized (lck) {
            return val;
        }
    }

    /**
     * get version
     *
     * @return version
     */
    public int ver() {
        synchronized (lck) {
            return upd;
        }
    }

    /**
     * set value
     *
     * @param v value
     */
    public void set(int v) {
        synchronized (lck) {
            upd++;
            val = v;
        }
    }

    /**
     * add value
     *
     * @param v value
     * @return value
     */
    public int add(int v) {
        synchronized (lck) {
            upd++;
            val += v;
            return val;
        }
    }

    /**
     * substract value
     *
     * @param v value
     * @return value
     */
    public int sub(int v) {
        synchronized (lck) {
            upd++;
            val -= v;
            return val;
        }
    }

    /**
     * negate value
     *
     * @return value
     */
    public int neg() {
        synchronized (lck) {
            upd++;
            val = -val;
            return val;
        }
    }

    /**
     * and value
     *
     * @param v value
     * @return value
     */
    public int and(int v) {
        synchronized (lck) {
            upd++;
            val &= v;
            return val;
        }
    }

    /**
     * or value
     *
     * @param v value
     * @return value
     */
    public int or(int v) {
        synchronized (lck) {
            upd++;
            val |= v;
            return val;
        }
    }

    /**
     * xor value
     *
     * @param v value
     * @return value
     */
    public int xor(int v) {
        synchronized (lck) {
            upd++;
            val ^= v;
            return val;
        }
    }

    /**
     * set value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int setIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val = v;
            return val;
        }
    }

    /**
     * add value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int addIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val += v;
            return val;
        }
    }

    /**
     * substract value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int subIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val -= v;
            return val;
        }
    }

    /**
     * negate value
     *
     * @param r required version
     * @return value
     */
    public int negIf(int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val = -val;
            return val;
        }
    }

    /**
     * and value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int andIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val &= v;
            return val;
        }
    }

    /**
     * or value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int orIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val |= v;
            return val;
        }
    }

    /**
     * xor value
     *
     * @param v value
     * @param r required version
     * @return value
     */
    public int xorIf(int v, int r) {
        synchronized (lck) {
            if (upd != r) {
                return val;
            }
            upd++;
            val ^= v;
            return val;
        }
    }

}
