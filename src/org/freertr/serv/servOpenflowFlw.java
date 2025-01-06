package org.freertr.serv;

import org.freertr.util.bits;

/**
 * one openflow flow
 *
 * @author matecsaba
 */
class servOpenflowFlw implements Comparable<servOpenflowFlw> {

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
     * @param o second
     * @return -1 if less, +1 if greated 0 if equals
     */
    public int compareTo(servOpenflowFlw o) {
        if (match.length < o.match.length) {
            return -1;
        }
        if (match.length > o.match.length) {
            return +1;
        }
        return bits.byteComp(match, 0, o.match, 0, match.length);
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
