package tab;

import ip.ipFwd;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * bier forwarder
 *
 * @author matecsaba
 */
public class tabLabelBier {

    /**
     * label base
     */
    public int base;

    /**
     * bit string length
     */
    public int bsl;

    /**
     * own index
     */
    public int idx;

    /**
     * forwarder instance
     */
    public ipFwd fwdr = null;

    /**
     * forwarder peers
     */
    public tabGen<tabLabelBierN> peers = new tabGen<tabLabelBierN>();

    /**
     * encode bit string length
     *
     * @param i bsl
     * @return encoded value
     */
    public static int num2bsl(int i) {
        int o = Integer.bitCount(Integer.highestOneBit(i) - 1) - 5;
        if (o < 0) {
            o = 0;
        }
        if (o > 7) {
            o = 7;
        }
        return o;
    }

    /**
     * decode bit string length
     *
     * @param i encoded value
     * @return bsl
     */
    public static int bsl2num(int i) {
        return 1 << (i + 5);
    }

    /**
     * normalize bit string length
     *
     * @param i bsl
     * @return bsl
     */
    public static int normalizeBsl(int i) {
        return bsl2num(num2bsl(i));
    }

    /**
     * get si mask
     *
     * @param i bsl
     * @return mask
     */
    public static BigInteger bsl2msk(int i) {
        return BigInteger.ONE.shiftLeft(bsl2num(i)).subtract(BigInteger.ONE);
    }

    /**
     * merge from other
     *
     * @param src source
     */
    public void mergeFrom(tabLabelBier src) {
        if (src == null) {
            return;
        }
        for (int i = 0; i < src.peers.size(); i++) {
            tabLabelBierN ntry = src.peers.get(i);
            if (ntry == null) {
                continue;
            }
            peers.add(ntry);
        }
        if (idx != 0) {
            return;
        }
        idx = src.idx;
    }

    /**
     * dump this entry
     *
     * @return dump
     */
    public List<String> getShow() {
        List<String> lst = new ArrayList<String>();
        lst.add("bier base|" + base);
        lst.add("bier bsl|" + bsl + "-" + bsl2num(bsl));
        lst.add("bier idx|" + idx);
        lst.add("bier fwdr|" + fwdr);
        for (int i = 0; i < peers.size(); i++) {
            lst.add("bier peer|" + peers.get(i));
        }
        return lst;
    }

}
