package org.freertr.cry;

/**
 * secure hash algorithm 3-32 (fips202) hash
 *
 * @author matecsaba
 */
public class cryHashShake256 extends cryHashShake {

    /**
     * create instance
     */
    public cryHashShake256() {
        bitLength = 256;
    }

}
