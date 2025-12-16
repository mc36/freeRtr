package org.freertr.auth;

import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashHmac;
import org.freertr.util.bits;

/**
 * one time password for events (rfc2289) and time (rfc6238)
 *
 * @author matecsaba
 */
public class autherOtp {

    private autherOtp() {
    }

    /**
     * time interval
     */
    public final static int timeInt = 30;

    /**
     * calculate hotp
     *
     * @param key key
     * @param vnt event
     * @param digs digits
     * @param cry crypto
     * @return result
     */
    public static String calcHotp(byte[] key, long vnt, int digs, cryHashGeneric cry) {
        final int[] digPow = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};
        byte[] buf = new byte[8];
        bits.msbPutQ(buf, 0, vnt);
        buf = cryHashGeneric.compute(new cryHashHmac(cry, key), buf);
        int i = buf[buf.length - 1] & 0xf;
        i = bits.msbGetD(buf, i) & 0x7fffffff;
        return bits.padBeg("" + (i % digPow[digs]), digs, "0");
    }

    /**
     * calculate totp
     *
     * @param key key
     * @param tim time
     * @param ntr interval
     * @param digs digits
     * @param cry crypto
     * @return result
     */
    public static String calcTotp(byte[] key, long tim, int ntr, int digs, cryHashGeneric cry) {
        return calcHotp(key, tim / ntr, digs, cry);
    }

}
