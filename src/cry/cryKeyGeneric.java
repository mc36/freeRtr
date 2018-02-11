package cry;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import pack.packHolder;
import util.bits;

/**
 * generic key storage
 *
 * @author matecsaba
 */
public abstract class cryKeyGeneric implements Comparator<cryKeyGeneric> {

    /**
     * name of this key
     */
    public String keyName;

    public int compare(cryKeyGeneric o1, cryKeyGeneric o2) {
        return o1.keyName.toLowerCase().compareTo(o2.keyName.toLowerCase());
    }

    /**
     * convert buffer to big integer
     *
     * @param src source buffer
     * @param ofs offset in buffer
     * @param siz size in bytes
     * @return integer
     */
    public static BigInteger buffer2bigInt(byte[] src, int ofs, int siz) {
        byte[] buf = new byte[siz + 1];
        for (int i = 0; i < siz; i++) {
            buf[i + 1] = src[ofs + i];
        }
        buf[0] = 0;
        return new BigInteger(buf);
    }

    /**
     * save big integer to buffer
     *
     * @param o big integer
     * @param siz size of buffer
     * @return byte buffer
     */
    public static byte[] bigInt2buffer(BigInteger o, int siz) {
        byte[] b1 = o.toByteArray();
        byte[] b2 = new byte[siz];
        if (b1.length >= b2.length) {
            for (int i = 0; i < b2.length; i++) {
                b2[i] = b1[b1.length - b2.length + i];
            }
            return b2;
        }
        for (int i = 0; i < b2.length; i++) {
            b2[i] = 0;
        }
        bits.byteCopy(b1, 0, b2, b2.length - b1.length, b1.length);
        return b2;
    }

    /**
     * generate random big integer
     *
     * @param siz size in bits
     * @return random number
     */
    public static BigInteger randomBigInt(int siz) {
        byte[] buf = new byte[(siz / 8) + 1];
        bits.randomS().nextBytes(buf);
        buf[0] = 0;
        BigInteger i = new BigInteger(buf);
        i = i.mod(BigInteger.ONE.shiftLeft(siz).subtract(BigInteger.ONE));
        return i;
    }

    /**
     * generate random prime
     *
     * @param siz size in bits
     * @return random prime
     */
    public static BigInteger randomPrime(int siz) {
        for (;;) {
            BigInteger i = BigInteger.probablePrime(siz, bits.randomS());
            if (i.bitLength() != siz) {
                continue;
            }
            if (!testPrime(i)) {
                continue;
            }
            return i;
        }
    }

    /**
     * test if a prime
     *
     * @param i number to test
     * @return true if prime, false if not
     */
    public static boolean testPrime(BigInteger i) {
        return i.isProbablePrime(100);
    }

    /**
     * return name of algorithm
     *
     * @return name of algorithm
     */
    public abstract String algName();

    /**
     * read key from asn1 format
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public abstract boolean privReader(packHolder pck);

    /**
     * write key to asn1 format
     *
     * @param pck packet to write to
     */
    public abstract void privWriter(packHolder pck);

    /**
     * read key from asn1 format
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public abstract boolean certReader(packHolder pck);

    /**
     * write key to asn1 format
     *
     * @param pck packet to write to
     */
    public abstract void certWriter(packHolder pck);

    /**
     * verify signature
     *
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean certVerify(byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] certSigning(byte[] hash);

    /**
     * verify signature
     *
     * @param ver protocol version
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean tlsVerify(int ver, byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param ver protocol version
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] tlsSigning(int ver, byte[] hash);

    /**
     * generate new key
     *
     * @param len length of key
     */
    public abstract void keyMake(int len);

    /**
     * verify a key
     *
     * @return false on success, true on error
     */
    public abstract boolean keyVerify();

    /**
     * size a key
     *
     * @return number of bits in key
     */
    public abstract int keySize();

    /**
     * read public key
     *
     * @param key key data
     * @return false on success, true on error
     */
    public abstract boolean sshReader(byte[] key);

    /**
     * save public key
     *
     * @return key data
     */
    public abstract byte[] sshWriter();

    /**
     * verify signature
     *
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean sshVerify(byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] sshSigning(byte[] hash);

    /**
     * read asn1 formatted key
     *
     * @param buf buffer to read
     * @param justPub just public key
     * @return false on success, true on error
     */
    public boolean asn1ReadBuf(byte[] buf, boolean justPub) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        if (justPub) {
            return certReader(pck);
        } else {
            return privReader(pck);
        }
    }

    /**
     * read pem formatted key
     *
     * @param s concatenated lines
     * @param justPub just public key
     * @return false on success, true on error
     */
    public boolean pemReadStr(String s, boolean justPub) {
        byte[] buf = cryBase64.decodeBytes(s);
        if (buf == null) {
            return true;
        }
        return asn1ReadBuf(buf, justPub);
    }

    /**
     * read pem formatted key
     *
     * @param sl list of lines
     * @param justPub just public key
     * @return false on success, true on error
     */
    public boolean pemReadArr(String[] sl, boolean justPub) {
        String s = "";
        for (int i = 0; i < sl.length; i++) {
            s = s + sl[i];
        }
        return pemReadStr(s, justPub);
    }

    /**
     * read pem formatted key
     *
     * @param sl list of lines
     * @param justPub just public key
     * @return false on success, true on error
     */
    public boolean pemReadLst(List<String> sl, boolean justPub) {
        String s = "";
        for (int i = 0; i < sl.size(); i++) {
            s = s + sl.get(i);
        }
        return pemReadStr(s, justPub);
    }

    /**
     * write to buffer
     *
     * @param justPub just public key
     * @return asn1 buffer
     */
    public byte[] asn1WriteBuf(boolean justPub) {
        packHolder p = new packHolder(true, true);
        if (justPub) {
            certWriter(p);
        } else {
            privWriter(p);
        }
        return p.getCopy();
    }

    /**
     * write to pem format string
     *
     * @param justPub just public key
     * @return pem string
     */
    public String pemWriteStr(boolean justPub) {
        byte[] buf = asn1WriteBuf(justPub);
        return cryBase64.encodeBytes(buf, 0, buf.length);
    }

    /**
     * write to pem format strings
     *
     * @param justPub just public key
     * @return list of string
     */
    public List<String> pemWriteLst(boolean justPub) {
        final int max = 64;
        String s = pemWriteStr(justPub);
        List<String> l = new ArrayList<String>();
        for (;;) {
            int i = s.length();
            if (i > max) {
                i = max;
            }
            String a = s.substring(0, i);
            if (a.length() < 1) {
                break;
            }
            s = s.substring(i, s.length());
            l.add(a);
        }
        return l;
    }

    /**
     * write to pem format strings
     *
     * @param justPub just public key
     * @return array of string
     */
    public String[] pemWriteArr(boolean justPub) {
        List<String> l = pemWriteLst(justPub);
        String[] a = new String[l.size()];
        for (int i = 0; i < a.length; i++) {
            a[i] = l.get(i);
        }
        return a;
    }

}
