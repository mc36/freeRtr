package org.freertr.cry;

import org.freertr.enc.encBase64;
import java.util.ArrayList;
import java.util.List;
import org.freertr.pack.packHolder;

/**
 * generic key storage
 *
 * @author matecsaba
 */
public abstract class cryKeyGeneric implements Comparable<cryKeyGeneric> {

    /**
     * create instance
     */
    public cryKeyGeneric() {
    }

    /**
     * name of this key
     */
    public String keyName;

    public int compareTo(cryKeyGeneric o) {
        return keyName.toLowerCase().compareTo(o.keyName.toLowerCase());
    }

    /**
     * return name of algorithm
     *
     * @return name of algorithm
     */
    public abstract String algName();

    /**
     * get ssh name
     *
     * @return name
     */
    public abstract String sshName();

    /**
     * get ssh hash
     *
     * @return hasher
     */
    public abstract cryHashGeneric sshHash();

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
     * @param pkcs oid of hash
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean certVerify(cryHashGeneric pkcs, byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param pkcs oid of hash
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] certSigning(cryHashGeneric pkcs, byte[] hash);

    /**
     * verify signature
     *
     * @param ver protocol version
     * @param pkcs oid of hash
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean tlsVerify(int ver, cryHashGeneric pkcs, byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param ver protocol version
     * @param pkcs oid of hash
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] tlsSigning(int ver, cryHashGeneric pkcs, byte[] hash);

    /**
     * generate new key
     *
     * @param len length of key
     * @return true on error, false on success
     */
    public abstract boolean keyMakeSize(int len);

    /**
     * generate new key
     *
     * @param nam name of key
     * @return true on error, false on success
     */
    public abstract boolean keyMakeName(String nam);

    /**
     * generate new key
     *
     * @param id tls id
     * @return true on error, false on success
     */
    public abstract boolean keyMakeTls(int id);

    /**
     * generate new key
     *
     * @param id ike id
     * @return true on error, false on success
     */
    public abstract boolean keyMakeIke(int id);

    /**
     * id value used to generate
     *
     * @return -1 on error, id on success
     */
    public abstract int keyMakeVal();

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
     * dump a key
     *
     * @return dumped
     */
    public abstract String keyDump();

    /**
     * client key exchange initialization
     */
    public abstract void keyClntInit();

    /**
     * server key exchange initialization
     */
    public abstract void keyServInit();

    /**
     * client common secret computation
     */
    public abstract void keyClntCalc();

    /**
     * server common secret computation
     */
    public abstract void keyServCalc();

    /**
     * get common secret for tls
     *
     * @return common secret
     */
    public abstract byte[] keyCommonTls();

    /**
     * get common secret for ssh
     *
     * @return common secret
     */
    public abstract byte[] keyCommonSsh();

    /**
     * get common secret for ike
     *
     * @return common secret
     */
    public abstract byte[] keyCommonIke();

    /**
     * get client public for tls
     *
     * @return common secret
     */
    public abstract byte[] keyClntTls();

    /**
     * get server public for tls
     *
     * @return common secret
     */
    public abstract byte[] keyServTls();

    /**
     * get client public for tls
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyClntTls(byte[] buf, int ofs);

    /**
     * get server public for tls
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyServTls(byte[] buf, int ofs);

    /**
     * get client public for ssh
     *
     * @return common secret
     */
    public abstract byte[] keyClntSsh();

    /**
     * get server public for ssh
     *
     * @return common secret
     */
    public abstract byte[] keyServSsh();

    /**
     * get client public for ssh
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyClntSsh(byte[] buf, int ofs);

    /**
     * get server public for ssh
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyServSsh(byte[] buf, int ofs);

    /**
     * get client public for ike
     *
     * @return common secret
     */
    public abstract byte[] keyClntIke();

    /**
     * get server public for ike
     *
     * @return common secret
     */
    public abstract byte[] keyServIke();

    /**
     * get client public for ike
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyClntIke(byte[] buf, int ofs);

    /**
     * get server public for ike
     *
     * @param buf buffer to use
     * @param ofs offset to use
     * @return true on error, false on success
     */
    public abstract boolean keyServIke(byte[] buf, int ofs);

    /**
     * get parameters for tls
     *
     * @return parameters
     */
    public abstract byte[][] keyParamTls();

    /**
     * get parameters for ssh
     *
     * @return parameters
     */
    public abstract byte[][] keyParamSsh();

    /**
     * set parameters for tls
     *
     * @param buf buffer to use
     * @return true on error, false on success
     */
    public abstract boolean keyParamTls(byte[][] buf);

    /**
     * set parameters for ssh
     *
     * @param buf buffer to use
     * @return true on error, false on success
     */
    public abstract boolean keyParamSsh(byte[][] buf);

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
     * @param algo hash algorithm
     * @param algn hash name
     * @param hash generated hash
     * @param sign received
     * @return false on success, true on error
     */
    public abstract boolean sshVerify(cryHashGeneric algo, String algn, byte[] hash, byte[] sign);

    /**
     * generate sign
     *
     * @param algo hash algorithm
     * @param algn hash name
     * @param hash generated hash
     * @return generated signature
     */
    public abstract byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash);

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
        byte[] buf = encBase64.decodeBytes(s);
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
            if (cryCertificate.isHeaderLine(sl[i])) {
                continue;
            }
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
            String a = sl.get(i);
            if (cryCertificate.isHeaderLine(a)) {
                continue;
            }
            s = s + a;
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
        return encBase64.encodeBytes(buf, 0, buf.length);
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
