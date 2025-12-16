package org.freertr.cry;

/**
 * secure hash algorithm 3-32 (fips202) hash
 *
 * @author matecsaba
 */
public class cryHashShake128 extends cryHashShake {

    /**
     * create instance
     */
    public cryHashShake128() {
        bitLength = 128;
    }

}
