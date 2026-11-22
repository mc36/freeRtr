package org.freertr.enc;

import org.freertr.pack.packHolder;
import org.freertr.user.userFormat;
import org.freertr.util.bits;

/**
 * transceiver decoding
 *
 * @author matecsaba
 */
public class encXcvr {

    /**
     * default constructor
     */
    private encXcvr() {
    }

    /**
     * decode sfp eeprom
     *
     * @return result
     */
    public static userFormat decode() {
        return new userFormat("|", "category|value");
    }

    /**
     * decode sfp eeprom
     *
     * @param pck buffer
     * @return result
     */
    public static userFormat decode(packHolder pck) {
        userFormat res = decode();
        decode(res, pck);
        return res;
    }

    /**
     * decode sfp eeprom
     *
     * @param res result
     * @param pck buffer
     */
    public static void decode(userFormat res, packHolder pck) {
        int i = pck.getByte(0);
        res.add("identifier|" + identifier(i));
        switch (i) {
            case 0x01:
            case 0x02:
            case 0x03:
                decodeSfp(res, pck);
                break;
            case 0x0c:
            case 0x0d:
            case 0x11:
                decodeQsfp(res, pck);
                break;
        }
    }

    /**
     * decode sff-8636 eeprom
     *
     * @param res result
     * @param pck buffer
     */
    public static void decodeQsfp(userFormat res, packHolder pck) {
        res.add("vendor name|" + enc7bit.doOneString(pck.getAsciiZ(148, 16, 0)));
        res.add("part number|" + enc7bit.doOneString(pck.getAsciiZ(168, 16, 0)));
        res.add("revision|" + enc7bit.doOneString(pck.getAsciiZ(184, 2, 0)));
        res.add("serial|" + enc7bit.doOneString(pck.getAsciiZ(196, 16, 0)));
        res.add("assembled|20" + enc7bit.doOneString(pck.getAsciiZ(212, 8, 0)));
        String a = "";
        int i = pck.getByte(3);
        a += bits.bit2str(i, 0x80, "", " tx4-los");
        a += bits.bit2str(i, 0x40, "", " tx3-los");
        a += bits.bit2str(i, 0x20, "", " tx2-los");
        a += bits.bit2str(i, 0x10, "", " tx1-los");
        a += bits.bit2str(i, 0x08, "", " rx4-los");
        a += bits.bit2str(i, 0x08, "", " rx3-los");
        a += bits.bit2str(i, 0x08, "", " rx2-los");
        a += bits.bit2str(i, 0x08, "", " rx1-los");
        i = pck.getByte(4);
        a += bits.bit2str(i, 0x80, "", " tx4-eq");
        a += bits.bit2str(i, 0x40, "", " tx3-eq");
        a += bits.bit2str(i, 0x20, "", " tx2-eq");
        a += bits.bit2str(i, 0x10, "", " tx1-eq");
        a += bits.bit2str(i, 0x08, "", " tx4-laser");
        a += bits.bit2str(i, 0x04, "", " tx3-laser");
        a += bits.bit2str(i, 0x02, "", " tx2-laser");
        a += bits.bit2str(i, 0x01, "", " tx1-laser");
        i = pck.getByte(5);
        a += bits.bit2str(i, 0x80, "", " tx4-cdr");
        a += bits.bit2str(i, 0x40, "", " tx3-cdr");
        a += bits.bit2str(i, 0x20, "", " tx2-cdr");
        a += bits.bit2str(i, 0x10, "", " tx1-cdr");
        a += bits.bit2str(i, 0x08, "", " rx4-cdr");
        a += bits.bit2str(i, 0x04, "", " rx3-cdr");
        a += bits.bit2str(i, 0x02, "", " rx2-cdr");
        a += bits.bit2str(i, 0x01, "", " rx1-cdr");
        res.add("indicators|" + a);
        a = "";
        i = pck.getByte(6);
        a += hiloew(a, "temp", i >>> 4);
        a += bits.bit2str(i, 0x02, "", " ready");
        a += bits.bit2str(i, 0x01, "", " inited");
        i = pck.getByte(7);
        a += hiloew(a, "vcc", i >>> 4);
        res.add("environment|" + a);
        a = "";
        i = pck.getByte(9);
        a += hiloew(a, "rx1", i >>> 4);
        a += hiloew(a, "rx2", i & 0xf);
        i = pck.getByte(10);
        a += hiloew(a, "rx3", i >>> 4);
        a += hiloew(a, "rx4", i & 0xf);
        res.add("power|" + a);
        a = "";
        i = pck.getByte(11);
        a += hiloew(a, "tx1", i >>> 4);
        a += hiloew(a, "tx2", i & 0xf);
        i = pck.getByte(12);
        a += hiloew(a, "tx3", i >>> 4);
        a += hiloew(a, "tx4", i & 0xf);
        res.add("bias|" + a);
        a = "";
        i = pck.getByte(13);
        a += hiloew(a, "tx1", i >>> 4);
        a += hiloew(a, "tx2", i & 0xf);
        i = pck.getByte(14);
        a += hiloew(a, "tx3", i >>> 4);
        a += hiloew(a, "tx4", i & 0xf);
        res.add("power|" + a);
        res.add("temperature|" + temp(pck.msbGetW(22)));
        res.add("voltage|" + volt(pck.msbGetW(26)));
        res.add("rx1 power|" + power(pck.msbGetW(34)));
        res.add("rx2 power|" + power(pck.msbGetW(36)));
        res.add("rx3 power|" + power(pck.msbGetW(38)));
        res.add("rx4 power|" + power(pck.msbGetW(40)));
        res.add("tx1 bias|" + bias(pck.msbGetW(42)));
        res.add("tx2 bias|" + bias(pck.msbGetW(44)));
        res.add("tx3 bias|" + bias(pck.msbGetW(46)));
        res.add("tx4 bias|" + bias(pck.msbGetW(48)));
        res.add("tx1 power|" + power(pck.msbGetW(50)));
        res.add("tx2 power|" + power(pck.msbGetW(52)));
        res.add("tx3 power|" + power(pck.msbGetW(54)));
        res.add("tx4 power|" + power(pck.msbGetW(56)));
        res.add("max power|" + power(pck.getByte(107)));
        res.add("delay|" + (pck.getByte(108) * 10) + " ns");
        res.add("connector|" + connector(pck.getByte(130)));
        res.add("encoding|" + encoding(pck.getByte(139)));
        res.add("nominal signal|" + (pck.getByte(140) * 100) + " mbps");
        res.add("nominal baud|" + (pck.getByte(222) * 250) + " mbps");
        res.add("smf length|" + pck.getByte(142) + " km");
        res.add("om1 length|" + pck.getByte(145) + " m");
        res.add("om2 length|" + pck.getByte(144) + " m");
        res.add("om4 length|" + (pck.getByte(146) * 2) + " m");
        res.add("om3 length|" + (pck.getByte(143) * 2) + " m");
        res.add("wavelength|" + wavel(pck.msbGetW(186)));
        res.add("tolerance|" + wavet(pck.msbGetW(188)));
    }

    /**
     * decode sff-8472 eeprom
     *
     * @param res result
     * @param pck buffer
     */
    public static void decodeSfp(userFormat res, packHolder pck) {
        res.add("vendor name|" + enc7bit.doOneString(pck.getAsciiZ(20, 16, 0)));
        res.add("part number|" + enc7bit.doOneString(pck.getAsciiZ(40, 16, 0)));
        res.add("revision|" + enc7bit.doOneString(pck.getAsciiZ(56, 4, 0)));
        res.add("serial|" + enc7bit.doOneString(pck.getAsciiZ(68, 16, 0)));
        res.add("assembled|20" + enc7bit.doOneString(pck.getAsciiZ(84, 6, 0)));
        res.add("connector|" + connector(pck.getByte(2)));
        String a = "";
        int i = pck.getByte(3);
        a += bits.bit2str(i, 0x80, "", " 10gbase-er");
        a += bits.bit2str(i, 0x40, "", " 10gbase-erm");
        a += bits.bit2str(i, 0x20, "", " 10gbase-lr");
        a += bits.bit2str(i, 0x10, "", " 10gbase-sr");
        a += bits.bit2str(i, 0x08, "", " ib 1x sx");
        a += bits.bit2str(i, 0x04, "", " ib 1x lx");
        a += bits.bit2str(i, 0x02, "", " ib copper active");
        a += bits.bit2str(i, 0x01, "", " ib copper passive");
        i = pck.getByte(4);
        a += bits.bit2str(i, 0x80, "", " escon mmf");
        a += bits.bit2str(i, 0x40, "", " escon smf");
        a += bits.bit2str(i, 0x20, "", " sonet oc192 short");
        a += bits.bit2str(i, 0x10, "", " sonet oc192 intermediate");
        a += bits.bit2str(i, 0x08, "", " sonet oc192 long");
        a += bits.bit2str(i, 0x04, "", " sonet oc48 long");
        a += bits.bit2str(i, 0x02, "", " sonet oc48 intermediate");
        a += bits.bit2str(i, 0x01, "", " sonet oc48 short");
        i = pck.getByte(5);
        a += bits.bit2str(i, 0x40, "", " sonet oc12 long");
        a += bits.bit2str(i, 0x20, "", " sonet oc12 intermediate");
        a += bits.bit2str(i, 0x10, "", " sonet oc12 short");
        a += bits.bit2str(i, 0x04, "", " sonet oc3 long");
        a += bits.bit2str(i, 0x02, "", " sonet oc3 intermediate");
        a += bits.bit2str(i, 0x01, "", " sonet oc3 short");
        i = pck.getByte(6);
        a += bits.bit2str(i, 0x80, "", " 10base-px");
        a += bits.bit2str(i, 0x40, "", " 10base-bx");
        a += bits.bit2str(i, 0x20, "", " 100base-fx");
        a += bits.bit2str(i, 0x10, "", " 100base-lx");
        a += bits.bit2str(i, 0x08, "", " 1000base-t");
        a += bits.bit2str(i, 0x04, "", " 1000base-cx");
        a += bits.bit2str(i, 0x02, "", " 1000base-lx");
        a += bits.bit2str(i, 0x01, "", " 1000base-sx");
        i = pck.getByte(7);
        a += bits.bit2str(i, 0x80, "", " verylong");
        a += bits.bit2str(i, 0x40, "", " short");
        a += bits.bit2str(i, 0x20, "", " intermediate");
        a += bits.bit2str(i, 0x10, "", " long");
        a += bits.bit2str(i, 0x08, "", " medium");
        a += bits.bit2str(i, 0x04, "", " sa");
        a += bits.bit2str(i, 0x02, "", " lc");
        a += bits.bit2str(i, 0x01, "", " inter");
        i = pck.getByte(8);
        a += bits.bit2str(i, 0x80, "", " intra");
        a += bits.bit2str(i, 0x40, "", " sn");
        a += bits.bit2str(i, 0x20, "", " sl");
        a += bits.bit2str(i, 0x10, "", " ll");
        a += bits.bit2str(i, 0x08, "", " active");
        a += bits.bit2str(i, 0x08, "", " passive");
        i = pck.getByte(9);
        a += bits.bit2str(i, 0x80, "", " fc tw");
        a += bits.bit2str(i, 0x40, "", " fc tp");
        a += bits.bit2str(i, 0x20, "", " fc mi");
        a += bits.bit2str(i, 0x10, "", " fc tv");
        a += bits.bit2str(i, 0x08, "", " fc m6");
        a += bits.bit2str(i, 0x04, "", " fc m5");
        a += bits.bit2str(i, 0x01, "", " fc sm");
        i = pck.getByte(10);
        a += bits.bit2str(i, 0x80, "", " 1200 mb");
        a += bits.bit2str(i, 0x40, "", " 800 mb");
        a += bits.bit2str(i, 0x20, "", " 1600 mb");
        a += bits.bit2str(i, 0x10, "", " 400 mb");
        a += bits.bit2str(i, 0x08, "", " 3200 mb");
        a += bits.bit2str(i, 0x02, "", " 200 mb");
        a += bits.bit2str(i, 0x01, "", " 100 mb");
        res.add("transceiver|" + a);
        res.add("encoding|" + encoding(pck.getByte(11)));
        res.add("nominal rate|" + (pck.getByte(12) * 100) + " mbps");
        res.add("rate range|" + pck.getByte(67) + "-" + pck.getByte(66));
        res.add("smf length|" + pck.getByte(14) + " km");
        res.add("smf length|" + (pck.getByte(15) * 100) + " m");
        res.add("om1 length|" + (pck.getByte(17) * 10) + " m");
        res.add("om2 length|" + (pck.getByte(16) * 10) + " m");
        res.add("om3 length|" + (pck.getByte(19) * 10) + " m");
        res.add("om4 length|" + (pck.getByte(18) * 10) + " m");
        res.add("wavelength|" + pck.msbGetW(60) + " nm");
        res.add("temperature|" + temp(pck.msbGetW(256 + 96)));
        res.add("voltage|" + volt(pck.msbGetW(256 + 98)));
        res.add("tx bias|" + bias(pck.msbGetW(256 + 100)));
        res.add("tx power|" + power(pck.msbGetW(256 + 102)));
        res.add("rx power|" + power(pck.msbGetW(256 + 104)));
    }

    private static String identifier(int i) {
        switch (i) {
            case 001:
                return "gbic";
            case 0x02:
                return "soldered";
            case 0x03:
                return "sfp+";
            case 0x04:
                return "xbi";
            case 0x05:
                return "xenpak";
            case 0x06:
                return "xfp";
            case 0x07:
                return "xff";
            case 0x08:
                return "xfp-e";
            case 0x09:
                return "xpak";
            case 0x0a:
                return "x2";
            case 0x0b:
                return "dwdm-sfp";
            case 0x0c:
                return "qsfp";
            case 0x0d:
                return "qsfp+";
            case 0x0e:
                return "cxp";
            case 0x0f:
                return "hd-4x";
            case 0x10:
                return "hd-8x";
            case 0x11:
                return "qsfp28";
            case 0x12:
                return "cxp28";
            case 0x13:
                return "cdfp12";
            case 0x14:
                return "hd-4x-fanout";
            case 0x15:
                return "hd-8x-fanout";
            case 0x16:
                return "cdfp3";
            case 0x17:
                return "micro-qsfp";
            case 0x18:
                return "qsfp-dd";
            case 0x19:
                return "osfp-8x";
            case 0x1a:
                return "sfp-dd";
            case 0x1b:
                return "dsfp";
            case 0x1c:
                return "minilink-x4";
            case 0x1d:
                return "minilink-x8";
            case 0x1e:
                return "qsfp+";
            case 0x1f:
                return "sfp-dd";
            case 0x20:
                return "sfp+";
            case 0x21:
                return "osfp-xd";
            case 0x22:
                return "oif-elsfp";
            case 0x23:
                return "cdfp-x4";
            case 0x24:
                return "cdfp-x8";
            case 0x25:
                return "cdfp-x16";
            case 0x26:
                return "xpo";
            default:
                return "unknown=" + i;
        }
    }

    private static String encoding(int i) {
        switch (i) {
            case 0x01:
                return "8b/10b";
            case 0x02:
                return "4b/5b";
            case 0x03:
                return "nrz";
            case 0x04:
                return "manchester";
            case 0x05:
                return "scrambled";
            case 0x06:
                return "64b/66b";
            case 0x07:
                return "256b/257b";
            case 0x08:
                return "pam4";
            default:
                return "unknown=" + i;
        }
    }

    private static String connector(int i) {
        switch (i) {
            case 0x01:
                return "sc";
            case 0x02:
                return "fc copper1";
            case 0x03:
                return "fc copper2";
            case 0x04:
                return "bnc";
            case 0x05:
                return "fc coax";
            case 0x06:
                return "fc jack";
            case 0x07:
                return "lc";
            case 0x08:
                return "mtrj";
            case 0x09:
                return "mu";
            case 0x0a:
                return "sg";
            case 0x0b:
                return "optical pigtail";
            case 0x0c:
                return "mpo 1x12";
            case 0x0d:
                return "mpo 2x16";
            case 0x20:
                return "hssdc";
            case 0x21:
                return "copper pigtail";
            case 0x22:
                return "rj45";
            case 0x23:
                return "none";
            case 0x24:
                return "mxc 2x16";
            case 0x25:
                return "cs";
            case 0x26:
                return "sn";
            case 0x27:
                return "mpo 2x12";
            case 0x28:
                return "mpo 1x16";
            default:
                return "unknown=" + i;
        }
    }

    private static String hiloew(String a, String w, int i) {
        a += bits.bit2str(i, 8, "", " " + w + "-hi-err");
        a += bits.bit2str(i, 4, "", " " + w + "-lo-err");
        a += bits.bit2str(i, 2, "", " " + w + "-hi-wrn");
        a += bits.bit2str(i, 1, "", " " + w + "-lo-wrn");
        return a;
    }

    private static String temp(int i) {
        return String.format("%.4f", i / 256.0) + " c";
    }

    private static String volt(int i) {
        return String.format("%.4f", i / 10000.0) + " v";
    }

    private static String bias(int i) {
        return String.format("%.4f", (i * 131.0) / 65536.0) + " ma";
    }

    private static String wavel(int i) {
        return String.format("%.4f", i / 20.0) + " nm";
    }

    private static String wavet(int i) {
        return String.format("%.4f", i / 200.0) + " nm";
    }

    private static String power(int i) {
        double dbm = 10.0 * Math.log10(i * 0.0001);
        return String.format("%.4f", i / 10000.0) + " mw, " + String.format("%.4f", dbm) + " dbm";
    }

}
