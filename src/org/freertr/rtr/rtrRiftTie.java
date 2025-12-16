package org.freertr.rtr;

import org.freertr.addr.addrEui;
import org.freertr.enc.encThriftEntry;
import org.freertr.util.bits;

/**
 * rift database entry
 *
 * @author matecsaba
 */
public class rtrRiftTie implements Comparable<rtrRiftTie> {

    /**
     * expiration time
     */
    public long expire;

    /**
     * direction, 1=south, 2=north
     */
    public int direct;

    /**
     * originator
     */
    public long origin;

    /**
     * type, 2=node, 3=prefix, 4=positiveDisaggregate, 5=negativeDisaggregate,
     * 6=pgPrefix, 7=keyValue, 8=externalPrefix, 9=positiveExtrenalDisaggregate
     */
    public int type;

    /**
     * number
     */
    public int number;

    /**
     * sequence
     */
    public long sequence;

    /**
     * elements
     */
    public encThriftEntry elements;

    /**
     * create instance
     */
    public rtrRiftTie() {
    }

    public int compareTo(rtrRiftTie o) {
        if (direct < o.direct) {
            return -1;
        }
        if (direct > o.direct) {
            return +1;
        }
        if (origin < o.origin) {
            return -1;
        }
        if (origin > o.origin) {
            return +1;
        }
        if (number < o.number) {
            return -1;
        }
        if (number > o.number) {
            return +1;
        }
        return 0;
    }

    /**
     * clone this data
     *
     * @return copied data object
     */
    public rtrRiftTie copyHead() {
        rtrRiftTie n = new rtrRiftTie();
        n.direct = direct;
        n.origin = origin;
        n.number = number;
        n.type = type;
        n.sequence = sequence;
        n.expire = expire;
        return n;
    }

    /**
     * test if differs from other
     *
     * @param other data to compare
     * @return true if differs, false if not
     */
    public boolean differs(rtrRiftTie other) {
        if (other == null) {
            return true;
        }
        return other.sequence != sequence;
    }

    /**
     * test if better from other
     *
     * @param other data to compare
     * @return true if better, false if not
     */
    public boolean better(rtrRiftTie other) {
        if (other == null) {
            return true;
        }
        if (other.sequence < sequence) {
            return true;
        }
        return false;
    }

    /**
     * convert to string
     *
     * @return converted string
     */
    public String toString() {
        String a;
        switch (direct) {
            case 1:
                a = "s";
                break;
            case 2:
                a = "n";
                break;
            default:
                a = "unk=" + direct;
        }
        return a + "|" + origin + "|" + number + "|" + type + "|" + sequence + "|" + bits.timeLeft(expire);
    }

    /**
     * get header from thrift
     *
     * @param th1 thrift to read
     * @return true on error, false on success
     */
    public boolean getHeader(encThriftEntry th1) {
        if (th1 == null) {
            return true;
        }
        encThriftEntry th2 = th1.getField(2, 0); // tieid
        if (th2 == null) {
            return true;
        }
        encThriftEntry th3 = th2.getField(1, 0);
        if (th3 == null) {
            return true;
        }
        direct = (int) th3.val;
        th3 = th2.getField(2, 0);
        if (th3 == null) {
            return true;
        }
        origin = th3.val;
        th3 = th2.getField(3, 0);
        if (th3 == null) {
            return true;
        }
        type = (int) th3.val;
        th3 = th2.getField(4, 0);
        if (th3 == null) {
            return true;
        }
        number = (int) th3.val;
        th2 = th1.getField(3, 0);
        if (th2 == null) {
            return true;
        }
        sequence = th2.val;
        return false;
    }

    /**
     * put header to thrift
     *
     * @return thrift to write
     */
    public encThriftEntry putHeader1() {
        encThriftEntry th1 = new encThriftEntry();
        th1.putField(1, encThriftEntry.tpI32, direct);
        th1.putField(2, encThriftEntry.tpI64, origin);
        th1.putField(3, encThriftEntry.tpI32, type);
        th1.putField(4, encThriftEntry.tpI32, number);
        return th1;
    }

    /**
     * put header to thrift
     *
     * @return thrift to write
     */
    public encThriftEntry putHeader2() {
        encThriftEntry th2 = new encThriftEntry();
        th2.putField(2, encThriftEntry.tpStr, putHeader1().elm); // tieid
        th2.putField(3, encThriftEntry.tpI64, sequence);
        th2.typ = encThriftEntry.tpStr;
        return th2;
    }

    /**
     * put header to thrift
     *
     * @return thrift to write
     */
    public encThriftEntry putHeader3() {
        encThriftEntry th3 = new encThriftEntry();
        th3.putField(1, encThriftEntry.tpStr, putHeader2().elm);
        th3.putField(2, encThriftEntry.tpI32, getRemain());
        return th3;
    }

    /**
     * get remaining lifetime
     *
     * @return seconds
     */
    public int getRemain() {
        return (int) ((expire - bits.getTime()) / 1000);
    }

    /**
     * check if expired
     *
     * @return true if yes, false if not
     */
    public boolean isExpired() {
        return getRemain() < 60;
    }

    /**
     * get origin as address
     *
     * @return address
     */
    public addrEui getOrigin() {
        addrEui adr = new addrEui();
        bits.msbPutQ(adr.getBytes(), 0, origin);
        return adr;
    }

}
