package org.freertr.tab;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.util.bits;
import org.freertr.util.cmds;

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
        return type + " " + bits.byteDump(data, 0, -1);
    }

    /**
     * convert from string
     *
     * @param cmd command to parse
     * @return true if error, false if success
     */
    public boolean fromString(cmds cmd) {
        String a = cmd.word();
        if (a.length() < 1) {
            return true;
        }
        type = bits.str2num(a);
        flag = rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive;
        List<Integer> buf = new ArrayList<Integer>();
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals(",")) {
                break;
            }
            buf.add(bits.fromHex(a));
        }
        data = new byte[buf.size()];
        for (int i = 0; i < data.length; i++) {
            int o = buf.get(i);
            data[i] = (byte) o;
        }
        return false;
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
