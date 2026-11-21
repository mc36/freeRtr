package org.freertr.enc;

import org.freertr.pack.packHolder;
import org.freertr.user.userFormat;
import org.freertr.util.bits;

/**
 * sfp decoding
 *
 * @author matecsaba
 */
public class encSff {

    /**
     * default constructor
     */
    private encSff() {
    }

    /**
     * decode sfp eeprom
     *
     * @param res result
     * @param pck buffer
     */
    public static void decode(userFormat res, packHolder pck) {
        res.add("vendor name|" + enc7bit.doOneString(pck.getAsciiZ(20, 16, 0)));
        res.add("part number|" + enc7bit.doOneString(pck.getAsciiZ(40, 16, 0)));
        res.add("revision|" + enc7bit.doOneString(pck.getAsciiZ(56, 4, 0)));
        res.add("serial|" + enc7bit.doOneString(pck.getAsciiZ(68, 16, 0)));
        res.add("assembled|20" + enc7bit.doOneString(pck.getAsciiZ(84, 6, 0)));
        String a;
        int i = pck.getByte(2);
        switch (i) {
            case 0x01:
                a = "sc";
                break;
            case 0x02:
                a = "fc copper1";
                break;
            case 0x03:
                a = "fc copper2";
                break;
            case 0x04:
                a = "bnc";
                break;
            case 0x05:
                a = "fc coax";
                break;
            case 0x06:
                a = "fc jack";
                break;
            case 0x07:
                a = "lc";
                break;
            case 0x08:
                a = "mtrj";
                break;
            case 0x09:
                a = "mu";
                break;
            case 0x0a:
                a = "sg";
                break;
            case 0x0b:
                a = "optical pigtail";
                break;
            case 0x0c:
                a = "mpo 1x12";
                break;
            case 0x0d:
                a = "mpo 2x16";
                break;
            case 0x20:
                a = "hssdc";
                break;
            case 0x21:
                a = "copper pigtail";
                break;
            case 0x22:
                a = "rj45";
                break;
            case 0x23:
                a = "none";
                break;
            case 0x24:
                a = "mxc 2x16";
                break;
            case 0x25:
                a = "cs";
                break;
            case 0x26:
                a = "sn";
                break;
            case 0x27:
                a = "mpo 2x12";
                break;
            case 0x28:
                a = "mpo 1x16";
                break;
            default:
                a = "unknown=" + i;
                break;
        }
        res.add("connector|" + a);
        a = "";
        i = pck.getByte(3);
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
        i = pck.getByte(11);
        switch (i) {
            case 0x01:
                a = "8b/10b";
                break;
            case 0x02:
                a = "4b/5b";
                break;
            case 0x03:
                a = "nrz";
                break;
            case 0x04:
                a = "manchester";
                break;
            case 0x05:
                a = "scrambled";
                break;
            case 0x06:
                a = "64b/66b";
                break;
            case 0x07:
                a = "256b/257b";
                break;
            case 0x08:
                a = "pam4";
                break;
            default:
                a = "unknown=" + i;
                break;
        }
        res.add("encoding|" + a);
        res.add("nominal rate|" + (pck.getByte(12) * 100) + " mbps");
        res.add("rate range|" + pck.getByte(67) + "-" + pck.getByte(66));
        res.add("smf length|" + pck.getByte(14) + " km");
        res.add("smf length|" + (pck.getByte(15) * 100) + " m");
        res.add("om1 length|" + (pck.getByte(17) * 10) + " m");
        res.add("om2 length|" + (pck.getByte(16) * 10) + " m");
        res.add("om3 length|" + (pck.getByte(19) * 10) + " m");
        res.add("om4 length|" + (pck.getByte(18) * 10) + " m");
        res.add("wavelength|" + pck.msbGetW(60) + " nm");
        i = pck.msbGetW(256 + 96);
        res.add("temperature|" + muldiv(i, 1.0, 256.0) + " c");
        i = pck.msbGetW(256 + 98);
        res.add("voltage|" + muldiv(i, 1.0, 10000.0) + " v");
        i = pck.msbGetW(256 + 100);
        res.add("tx bias|" + muldiv(i, 131.0, 65536.0) + " ma");
        i = pck.msbGetW(256 + 102);
        res.add("tx power|" + getpwr(i));
        i = pck.msbGetW(256 + 104);
        res.add("rx power|" + getpwr(i));
    }

    private static String muldiv(double val, double mul, double div) {
        return String.format("%.4f", (mul * val) / div);
    }

    private static String getpwr(double val) {
        double dbm = 10.0 * Math.log10(val * 0.0001);
        return String.format("%.4f", val / 10000) + " mw, " + String.format("%.4f", dbm) + " dbm";
    }

}
