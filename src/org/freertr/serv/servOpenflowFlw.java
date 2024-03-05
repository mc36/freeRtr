package org.freertr.serv;

import java.util.Comparator;
import org.freertr.util.bits;

/**
 * one openflow flow
 *
 * @author matecsaba
 */
class servOpenflowFlw implements Comparator<servOpenflowFlw> {

    /**
     * match bytes
     */
    protected byte[] match = new byte[0];

    /**
     * action bytes
     */
    protected byte[] action = new byte[0];

    /**
     * cookie info
     */
    protected int cookie;

    /**
     * flow priority
     */
    protected int prio;

    /**
     * compare two flows
     *
     * @param o1 first
     * @param o2 second
     * @return -1 if less, +1 if greated 0 if equals
     */
    public int compare(servOpenflowFlw o1, servOpenflowFlw o2) {
        if (o1.match.length < o2.match.length) {
            return -1;
        }
        if (o1.match.length > o2.match.length) {
            return +1;
        }
        return bits.byteComp(o1.match, 0, o2.match, 0, o1.match.length);
    }

    /**
     * compare if same action done
     *
     * @param other other flow
     * @return true if yes
     */
    protected boolean sameAct(servOpenflowFlw other) {
        if (other.action.length != action.length) {
            return false;
        }
        return bits.byteComp(action, 0, other.action, 0, action.length) == 0;
    }

    /**
     * dump this flow
     *
     * @return string
     */
    protected String dump() {
        return "cookie=" + cookie + " prio=" + prio + " match=" + bits.byteDump(match, 0, -1) + " action=" + bits.byteDump(action, 0, -1);
    }

}
