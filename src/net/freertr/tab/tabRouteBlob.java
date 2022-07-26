package net.freertr.tab;

import java.util.Comparator;
import net.freertr.util.bits;

/**
 * represents one attribute blob
 *
 * @author matecsaba
 */
public class tabRouteBlob implements Comparator<tabRouteBlob> {

    /**
     * create instance
     */
    public tabRouteBlob() {
    }

    /**
     * type of attribute
     */
    public int type;

    /**
     * flag of attribute
     */
    public int flag;

    /**
     * data of attribute
     */
    public byte[] data;

    public String toString() {
        return type + ":" + bits.byteDump(data, 0, -1);
    }

    /**
     * copy data
     *
     * @return copy
     */
    public tabRouteBlob copyBytes() {
        tabRouteBlob d = new tabRouteBlob();
        d.type = type;
        d.flag = flag;
        d.data = new byte[data.length];
        bits.byteCopy(data, 0, d.data, 0, data.length);
        return d;
    }

    public int compare(tabRouteBlob o1, tabRouteBlob o2) {
        if (o1.type < o2.type) {
            return -1;
        }
        if (o1.type > o2.type) {
            return +1;
        }
        if (o1.flag < o2.flag) {
            return -1;
        }
        if (o1.flag > o2.flag) {
            return +1;
        }
        if (o1.data.length < o2.data.length) {
            return -1;
        }
        if (o1.data.length > o2.data.length) {
            return +1;
        }
        return bits.byteComp(o1.data, 0, o2.data, 0, o1.data.length);
    }

}
