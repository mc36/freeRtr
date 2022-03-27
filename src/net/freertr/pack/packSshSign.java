package net.freertr.pack;

import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.cry.cryKeyRSA;

/**
 * secure shell signature (rfc4432) protocol
 *
 * @author matecsaba
 */
public class packSshSign {

    /**
     * key algorithms
     */
    public final static String[] keySignAlgs = {cryKeyRSA.sshName2, cryKeyRSA.sshName3, cryKeyRSA.sshName, cryKeyDSA.sshName};

    /**
     * selected algorithm
     */
    public int[] algo;

    /**
     * create instance
     *
     * @param s string to read
     */
    public packSshSign(String s) {
        fromString(s);
    }

    /**
     * populate from algorithm list
     *
     * @param s string to read
     */
    public void fromString(String s) {
        algo = packSshInit.algoParseList(s, keySignAlgs);
    }

    /**
     * get chosen key verifier
     *
     * @return key verifier
     */
    public cryKeyGeneric getKeyVerifier() {
        if (algo.length < 1) {
            return null;
        }
        switch (algo[0]) {
            case 0:
                return new cryKeyRSA();
            case 1:
                return new cryKeyRSA();
            case 2:
                return new cryKeyRSA();
            case 3:
                return new cryKeyDSA();
            default:
                return null;
        }
    }

    /**
     * get key hash algorithm
     *
     * @return hash
     */
    public cryHashGeneric getKeyHashAlgo() {
        if (algo.length < 1) {
            return null;
        }
        switch (algo[0]) {
            case 0:
                return new cryHashSha2256();
            case 1:
                return new cryHashSha2512();
            case 2:
                return new cryHashSha1();
            case 3:
                return new cryHashSha1();
            default:
                return null;
        }
    }

    /**
     * get key algorithm name
     *
     * @return name
     */
    public String getKeyHashAlgn() {
        if (algo.length < 1) {
            return null;
        }
        switch (algo[0]) {
            case 0:
                return cryKeyRSA.sshName2;
            case 1:
                return cryKeyRSA.sshName3;
            case 2:
                return cryKeyRSA.sshName;
            case 3:
                return cryKeyDSA.sshName;
            default:
                return null;
        }
    }

    /**
     * get chosen key signer
     *
     * @param dss dss key to use
     * @param rsa rsa key to use
     * @return key signer
     */
    public cryKeyGeneric getKeySigner(cryKeyGeneric dss, cryKeyGeneric rsa) {
        if (algo.length < 1) {
            return null;
        }
        switch (algo[0]) {
            case 0:
                return rsa;
            case 1:
                return rsa;
            case 2:
                return rsa;
            case 3:
                return dss;
            default:
                return null;
        }
    }

}
