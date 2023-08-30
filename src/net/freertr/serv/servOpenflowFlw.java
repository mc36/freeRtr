package net.freertr.serv;

import java.util.Comparator;
import net.freertr.util.bits;

/**
 * one openflow flow
 *
 * @author matecsaba
 */
public class servOpenflowFlw implements Comparator<servOpenflowFlw> {

    public byte[] match = new byte[0];

    public byte[] action = new byte[0];

    public int cookie;

    public int prio;

    public int compare(servOpenflowFlw o1, servOpenflowFlw o2) {
        if (o1.match.length < o2.match.length) {
            return -1;
        }
        if (o1.match.length > o2.match.length) {
            return +1;
        }
        return bits.byteComp(o1.match, 0, o2.match, 0, o1.match.length);
    }

    public boolean sameAct(servOpenflowFlw other) {
        if (other.action.length != action.length) {
            return false;
        }
        return bits.byteComp(action, 0, other.action, 0, action.length) == 0;
    }

    public String dump() {
        return "cookie=" + cookie + " prio=" + prio + " match=" + bits.byteDump(match, 0, -1) + " action=" + bits.byteDump(action, 0, -1);
    }

}
