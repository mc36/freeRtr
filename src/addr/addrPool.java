package addr;

import java.math.BigInteger;
import util.bits;
import util.logger;

/**
 * address pool
 *
 * @param <T> type of address
 * @author matecsaba
 */
public class addrPool<T extends addrType> {

    /**
     * name of this pool
     */
    public String poolName;

    private T first;

    private BigInteger frst;

    private BigInteger incr;

    private boolean[] used;

    /**
     * setup address pool
     *
     * @param adr first address
     * @param nxt increment of next address
     * @param num number of addresses in pool
     */
    @SuppressWarnings("unchecked")
    public addrPool(T adr, T nxt, int num) {
        first = (T) adr.copyBytes();
        used = new boolean[num];
        for (int i = 0; i < num; i++) {
            used[i] = false;
        }
        frst = addr2bn(first);
        incr = addr2bn(nxt);
    }

    private BigInteger addr2bn(T adr) {
        byte[] buf1 = adr.getBytes();
        byte[] buf2 = new byte[buf1.length + 1];
        bits.byteCopy(buf1, 0, buf2, 1, buf1.length);
        buf2[0] = 0;
        return new BigInteger(buf2);
    }

    @SuppressWarnings("unchecked")
    private T bn2addr(BigInteger n) {
        T a = (T) first.copyBytes();
        final int as = a.getSize();
        byte[] buf1 = n.toByteArray();
        if (buf1.length > as) {
            a.fromBuf(buf1, buf1.length - as);
            return a;
        }
        byte[] buf2 = new byte[as];
        for (int i = 0; i < as; i++) {
            buf2[i] = 0;
        }
        bits.byteCopy(buf1, 0, buf2, as - buf1.length, buf1.length);
        a.fromBuf(buf2, 0);
        return a;
    }

    /**
     * return total number of addresses
     *
     * @return total number of addresses
     */
    public int addrTotal() {
        return used.length;
    }

    /**
     * return free number of addresses
     *
     * @return free number of addresses
     */
    public int addrFree() {
        int o = 0;
        for (int i = 0; i < used.length; i++) {
            if (used[i] == false) {
                o++;
            }
        }
        return o;
    }

    /**
     * allocate one address
     *
     * @return address allocated, null if none
     */
    public synchronized T addrAlloc() {
        for (int retry = 0; retry < 64; retry++) {
            int i = bits.random(0, used.length);
            if (used[i]) {
                continue;
            }
            used[i] = true;
            BigInteger b = new BigInteger("" + i).multiply(incr).add(frst);
            return bn2addr(b);
        }
        logger.warn("failed to allocate new address");
        return null;
    }

    /**
     * release one address
     *
     * @param adr address to release
     */
    public void addrRelease(T adr) {
        BigInteger i = addr2bn(adr);
        i = i.subtract(frst).divide(incr);
        int o = i.intValue();
        if ((o < 0) || (o >= used.length)) {
            return;
        }
        used[o] = false;
    }

}
