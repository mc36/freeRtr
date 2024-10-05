package org.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * domain name protocol (rfc1035) resource record
 *
 * @author matecsaba
 */
public class packDnsRec implements Comparable<packDnsRec> {

    /**
     * create instance
     */
    public packDnsRec() {
    }

    /**
     * time of addition
     */
    public long added;

    /**
     * hit counter
     */
    public int asked;

    /**
     * name
     */
    public String name = "";

    /**
     * type
     */
    public int typ;

    /**
     * class
     */
    public int clss = classIN;

    /**
     * time to live
     */
    public int ttl = -1;

    /**
     * responses
     */
    public tabGen<packDnsRes> res = new tabGen<packDnsRes>();

    /**
     * the internet
     */
    public final static int classIN = 1;

    /**
     * the CSNET class
     */
    public final static int classCS = 2;

    /**
     * the CHAOS class
     */
    public final static int classCH = 3;

    /**
     * Hesiod / Dyer 87
     */
    public final static int classHS = 4;

    /**
     * any class
     */
    public final static int classALL = 255;

    /**
     * a host address
     */
    public final static int typeA = 1;

    /**
     * an authoritative name server
     */
    public final static int typeNS = 2;

    /**
     * a mail destination OBSOLETE - use MX
     */
    public final static int typeMD = 3;

    /**
     * a mail forwarder OBSOLETE - use MX
     */
    public final static int typeMF = 4;

    /**
     * the canonical name for an alias
     */
    public final static int typeCNAME = 5;

    /**
     * marks the start of a zone of authority
     */
    public final static int typeSOA = 6;

    /**
     * a mailbox domain name EXPERIMENTAL
     */
    public final static int typeMB = 7;

    /**
     * a mail group member EXPERIMENTAL
     */
    public final static int typeMG = 8;

    /**
     * a mail rename domain name EXPERIMENTAL
     */
    public final static int typeMR = 9;

    /**
     * a null RR EXPERIMENTAL
     */
    public final static int typeNULL = 10;

    /**
     * a well known service description
     */
    public final static int typeWKS = 11;

    /**
     * a domain name pointer
     */
    public final static int typePTR = 12;

    /**
     * host information
     */
    public final static int typeHINFO = 13;

    /**
     * mailbox or mail list information
     */
    public final static int typeMINFO = 14;

    /**
     * mail exchange
     */
    public final static int typeMX = 15;

    /**
     * text strings
     */
    public final static int typeTXT = 16;

    /**
     * responsible person
     */
    public final static int typeRP = 17;

    /**
     * x25 phone number
     */
    public final static int typeX25 = 19;

    /**
     * isdn phone number
     */
    public final static int typeISDN = 20;

    /**
     * route through
     */
    public final static int typeRT = 21;

    /**
     * network service access protocol
     */
    public final static int typeNSAP = 22;

    /**
     * a nsap pointer
     */
    public final static int typeNSAPPTR = 23;

    /**
     * signature
     */
    public final static int typeSIG = 24;

    /**
     * key
     */
    public final static int typeKEY = 25;

    /**
     * pointer to x400/rfc822 mapping
     */
    public final static int typePX = 26;

    /**
     * geographical position
     */
    public final static int typeGPOS = 27;

    /**
     * a host address
     */
    public final static int typeAAAA = 28;

    /**
     * location
     */
    public final static int typeLOC = 29;

    /**
     * next domain
     */
    public final static int typeNXT = 30;

    /**
     * endpoint identifier
     */
    public final static int typeEID = 31;

    /**
     * nimrod locator
     */
    public final static int typeNIMLOC = 32;

    /**
     * location of services
     */
    public final static int typeSRV = 33;

    /**
     * atm address
     */
    public final static int typeATMA = 34;

    /**
     * naming authority pointer
     */
    public final static int typeNAPTR = 35;

    /**
     * key exchange
     */
    public final static int typeKX = 36;

    /**
     * certification record
     */
    public final static int typeCERT = 37;

    /**
     * ipv6 address
     */
    public final static int typeA6 = 38;

    /**
     * non-terminal dname
     */
    public final static int typeDNAME = 39;

    /**
     * kitchen sink
     */
    public final static int typeSINK = 40;

    /**
     * edns0 option
     */
    public final static int typeOPT = 41;

    /**
     * address prefix list
     */
    public final static int typeAPL = 42;

    /**
     * delegation signer
     */
    public final static int typeDS = 43;

    /**
     * ssh fp
     */
    public final static int typeSSHFP = 44;

    /**
     * ipsec key
     */
    public final static int typeIPSEC = 45;

    /**
     * rr signature
     */
    public final static int typeRRSIG = 46;

    /**
     * name security
     */
    public final static int typeNSEC = 47;

    /**
     * dns key
     */
    public final static int typeDNSKEY = 48;

    /**
     * dhcid
     */
    public final static int typeDHCID = 49;

    /**
     * nsec3
     */
    public final static int typeNSEC3 = 50;

    /**
     * nsec3 param
     */
    public final static int typeNSEC3P = 51;

    /**
     * tlsa
     */
    public final static int typeTLSA = 52;

    /**
     * host identity protocol
     */
    public final static int typeHIP = 55;

    /**
     * ninfo
     */
    public final static int typeNINFO = 56;

    /**
     * rkey
     */
    public final static int typeRKEY = 57;

    /**
     * trust another link
     */
    public final static int typeTALINK = 58;

    /**
     * child ds
     */
    public final static int typeCDS = 59;

    /**
     * sender policy framework
     */
    public final static int typeSPF = 99;

    /**
     * uinfo
     */
    public final static int typeUINFO = 100;

    /**
     * uid
     */
    public final static int typeUID = 101;

    /**
     * gid
     */
    public final static int typeGID = 102;

    /**
     * unspecified
     */
    public final static int typeUNSPEC = 103;

    /**
     * key
     */
    public final static int typeTKEY = 249;

    /**
     * signature
     */
    public final static int typeTSIG = 250;

    /**
     * incremental zone transfer
     */
    public final static int typeIXFR = 251;

    /**
     * transfer of an entire zone
     */
    public final static int typeAXFR = 252;

    /**
     * mailbox-related records
     */
    public final static int typeMAILB = 253;

    /**
     * mail agent RRs
     */
    public final static int typeMAILA = 254;

    /**
     * all records
     */
    public final static int typeANY = 255;

    /**
     * uri
     */
    public final static int typeURI = 256;

    /**
     * caa
     */
    public final static int typeCAA = 257;

    /**
     * interface ipv4 address
     */
    public final static int typeInt4 = 0x10000 | typeA;

    /**
     * interface ipv6 address
     */
    public final static int typeInt6 = 0x10000 | typeAAAA;

    /**
     * ipv4 reverse suffix
     */
    public final static String revSuffix4 = "IN-ADDR.ARPA";

    /**
     * ipv6 reverse suffix
     */
    public final static String revSuffix6 = "IP6.ARPA";

    /**
     * copy bytes
     *
     * @return dns record
     */
    public packDnsRec copyBytes() {
        packDnsRec n = new packDnsRec();
        n.added = added;
        n.asked = asked;
        n.name = name;
        n.typ = typ;
        n.clss = clss;
        n.ttl = ttl;
        for (int i = 0; i < res.size(); i++) {
            n.res.add(res.get(i));
        }
        return n;
    }

    /**
     * generate ptr for ipv4 address
     *
     * @param adr address to generate for
     * @param suff suffix
     * @return string showint ptr name
     */
    protected static String generateReverse4(addrIPv4 adr, String suff) {
        String s = "";
        byte[] buf = adr.getBytes();
        for (int i = 0; i < buf.length; i++) {
            s = (buf[i] & 0xff) + "." + s;
        }
        return s + suff;
    }

    /**
     * generate ptr for ipv6 address
     *
     * @param adr address to generate for
     * @param suff suffix
     * @return string showint ptr name
     */
    protected static String generateReverse6(addrIPv6 adr, String suff) {
        String s = "";
        byte[] buf = adr.getBytes();
        for (int i = 0; i < buf.length; i++) {
            String a = bits.toHexB(buf[i] & 0xff);
            s = a.substring(1, 2) + "." + a.substring(0, 1) + "." + s;
        }
        return s + suff;
    }

    /**
     * generate ptr for ip address
     *
     * @param adr address to generate for
     * @return string showint ptr name
     */
    public static String generateReverse(addrIP adr) {
        if (adr.isIPv4()) {
            return generateReverse4(adr.toIPv4(), revSuffix4);
        } else {
            return generateReverse6(adr.toIPv6(), revSuffix6);
        }
    }

    /**
     * generate rbl for ip address
     *
     * @param adr address to generate for
     * @param suff suffix
     * @return string showint ptr name
     */
    public static String generateReverse(addrIP adr, String suff) {
        if (adr.isIPv4()) {
            return generateReverse4(adr.toIPv4(), suff);
        } else {
            return generateReverse6(adr.toIPv6(), suff);
        }
    }

    /**
     * convert class to string
     *
     * @param i opcode
     * @return string representing opcode
     */
    public static String class2str(int i) {
        switch (i) {
            case classIN:
                return "in";
            case classCS:
                return "cs";
            case classCH:
                return "ch";
            case classHS:
                return "hs";
            case classALL:
                return "all";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert class to string
     *
     * @param i opcode
     * @return string representing opcode
     */
    public static String type2str(int i) {
        switch (i) {
            case typeA:
                return "ip4a";
            case typeNS:
                return "ns";
            case typeMD:
                return "md";
            case typeMF:
                return "mf";
            case typeCNAME:
                return "cname";
            case typeSOA:
                return "soa";
            case typeMB:
                return "mb";
            case typeMG:
                return "mg";
            case typeMR:
                return "mr";
            case typeNULL:
                return "null";
            case typeWKS:
                return "wks";
            case typePTR:
                return "ptr";
            case typeHINFO:
                return "hinfo";
            case typeMINFO:
                return "minfo";
            case typeMX:
                return "mx";
            case typeTXT:
                return "txt";
            case typeRP:
                return "rp";
            case typeX25:
                return "x25";
            case typeISDN:
                return "isdn";
            case typeRT:
                return "rt";
            case typeNSAP:
                return "nsap";
            case typeNSAPPTR:
                return "nsap-ptr";
            case typePX:
                return "px";
            case typeGPOS:
                return "gpos";
            case typeAAAA:
                return "ip6a";
            case typeLOC:
                return "loc";
            case typeNXT:
                return "nxt";
            case typeEID:
                return "eid";
            case typeNIMLOC:
                return "nimloc";
            case typeSRV:
                return "srv";
            case typeATMA:
                return "atma";
            case typeNAPTR:
                return "naptr";
            case typeKX:
                return "kx";
            case typeCERT:
                return "cert";
            case typeA6:
                return "a6";
            case typeDNAME:
                return "dname";
            case typeSINK:
                return "sink";
            case typeOPT:
                return "opt";
            case typeAPL:
                return "apl";
            case typeDS:
                return "ds";
            case typeSSHFP:
                return "sshfp";
            case typeIPSEC:
                return "ipsec";
            case typeRRSIG:
                return "rrsig";
            case typeNSEC:
                return "nsec";
            case typeDNSKEY:
                return "dnskey";
            case typeDHCID:
                return "dhcid";
            case typeNSEC3:
                return "nsec3";
            case typeNSEC3P:
                return "nsec3p";
            case typeTLSA:
                return "tlsa";
            case typeHIP:
                return "hip";
            case typeNINFO:
                return "ninfo";
            case typeRKEY:
                return "rkey";
            case typeTALINK:
                return "talink";
            case typeCDS:
                return "cds";
            case typeSPF:
                return "spf";
            case typeUINFO:
                return "uinfo";
            case typeUID:
                return "uid";
            case typeGID:
                return "gid";
            case typeUNSPEC:
                return "unspec";
            case typeTKEY:
                return "tkey";
            case typeTSIG:
                return "tsig";
            case typeIXFR:
                return "ixfr";
            case typeAXFR:
                return "axfr";
            case typeMAILB:
                return "mailb";
            case typeMAILA:
                return "maila";
            case typeInt4:
                return "ip4i";
            case typeInt6:
                return "ip6i";
            case typeANY:
                return "any";
            case typeURI:
                return "uri";
            case typeCAA:
                return "caa";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * decode pointer
     *
     * @param pck packet to read
     * @param len where to stop
     * @param rnd round
     * @return pointer
     */
    public static String getPointer(packHolder pck, int len, int rnd) {
        int i = pck.msbGetW(0) & 0x3fff;
        pck.getSkip(2);
        int o = pck.dataSize();
        pck.setBytesLeft(len - i);
        String s = getChain(pck, len, rnd);
        pck.setBytesLeft(o);
        return s;
    }

    /**
     * decode string
     *
     * @param pck packet to read
     * @return string
     */
    public static String getString(packHolder pck) {
        int i = pck.getByte(0);
        pck.getSkip(1);
        String s = pck.getAsciiZ(0, i, -1);
        pck.getSkip(i);
        return s;
    }

    /**
     * decode chain
     *
     * @param pck packet to read
     * @param len where to stop
     * @param rnd round
     * @return readed domain name
     */
    public static String getChain(packHolder pck, int len, int rnd) {
        if (rnd > 64) {
            return "";
        }
        String s = "";
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            int i = pck.getByte(0);
            if ((i & 0xc0) == 0xc0) {
                s += "." + getPointer(pck, len, rnd + 1);
                break;
            }
            String a = getString(pck);
            if (a.length() < 1) {
                break;
            }
            s += "." + a;
        }
        if (s.length() < 1) {
            return "";
        }
        return s.substring(1, s.length());
    }

    /**
     * put string
     *
     * @param pck packet to update
     * @param s string
     */
    public static void putString(packHolder pck, String s) {
        final int max = 255;
        int i = s.length();
        if (i > max) {
            s = s.substring(0, max);
            i = max;
        }
        pck.putByte(0, i);
        pck.putAsciiZ(1, max, s, 0);
        pck.putSkip(i + 1);
    }

    /**
     * put domain name
     *
     * @param pck packet to update
     * @param s domain to encode
     */
    public static void putDomain(packHolder pck, String s) {
        for (;;) {
            String a = "";
            int i = s.indexOf(".");
            if (i < 1) {
                a = s;
                s = "";
            } else {
                a = s.substring(0, i);
                s = s.substring(i + 1, s.length());
            }
            if (a.length() < 1) {
                break;
            }
            putString(pck, a);
        }
        putString(pck, "");
    }

    public String toString() {
        return "name=" + name + " class=" + class2str(clss) + " type=" + type2str(typ) + " ttl=" + ttl;
    }

    /**
     * convert to user string
     *
     * @param sep separator
     * @param beg beginning
     * @param stat statistics
     * @return user string
     */
    public List<String> toUserStr(String sep, String beg, boolean stat) {
        String ad = "";
        if (stat) {
            ad = sep + asked + sep + bits.timeDump(ttl) + sep + bits.timePast(added);
        }
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < res.size(); i++) {
            String a = res.get(i).toUserStr(typ);
            if (a == null) {
                continue;
            }
            l.add(beg + name + sep + type2str(typ) + sep + a + ad);
        }
        return l;
    }

    /**
     * convert from user string
     *
     * @param cmd command to parse
     * @return false if successful, true if error happened
     */
    public boolean fromUserStr(cmds cmd) {
        name = cmd.word().toLowerCase();
        if (name.toLowerCase().equals("rev")) {
            name = cmd.word();
            addrIP adr = new addrIP();
            if (adr.fromString(name)) {
                return true;
            }
            name = generateReverse(adr).toLowerCase();
        }
        String s = cmd.word().toLowerCase();
        typ = -1;
        if (s.equals("ip4a")) {
            typ = typeA;
        }
        if (s.equals("ip6a")) {
            typ = typeAAAA;
        }
        if (s.equals("ip4i")) {
            typ = typeInt4;
        }
        if (s.equals("ip6i")) {
            typ = typeInt6;
        }
        if (s.equals("cname")) {
            typ = typeCNAME;
        }
        if (s.equals("ptr")) {
            typ = typePTR;
        }
        if (s.equals("ns")) {
            typ = typeNS;
        }
        if (s.equals("rp")) {
            typ = typeRP;
        }
        if (s.equals("mx")) {
            typ = typeMX;
        }
        if (s.equals("txt")) {
            typ = typeTXT;
        }
        if (s.equals("srv")) {
            typ = typeSRV;
        }
        if (s.equals("hinfo")) {
            typ = typeHINFO;
        }
        if (s.equals("soa")) {
            typ = typeSOA;
        }
        if (typ == -1) {
            return true;
        }
        packDnsRes r = new packDnsRes();
        if (r.fromUserStr(typ, cmd)) {
            return true;
        }
        res.add(r);
        return false;
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @param len message ending
     * @param question parse just header part
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck, int len, boolean question) {
        if (pck.dataSize() < 1) {
            return true;
        }
        name = getChain(pck, len, 0).toLowerCase();
        typ = pck.msbGetW(0); // type
        clss = pck.msbGetW(2); // class
        pck.getSkip(4);
        if (question) {
            packDnsRes r = new packDnsRes();
            res.add(r);
            return false;
        }
        ttl = pck.msbGetD(0); // time to live
        int siz = pck.msbGetW(4); // size of rdata
        pck.getSkip(6);
        siz = pck.dataSize() - siz;
        packDnsRes r = new packDnsRes();
        if (r.parseHeader(typ, pck, len)) {
            return true;
        }
        res.add(r);
        pck.setBytesLeft(siz);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     * @param question parse just header part
     */
    public void createHeader(packHolder pck, boolean question) {
        for (int i = 0; i < res.size(); i++) {
            putDomain(pck, name);
            pck.msbPutW(0, typ); // type
            pck.msbPutW(2, clss); // class
            pck.putSkip(4);
            if (question) {
                return;
            }
            pck.msbPutD(0, ttl); // ttl
            pck.msbPutD(4, 0); // size
            pck.putSkip(6);
            res.get(i).createHeader(typ, pck);
        }
    }

    public int compareTo(packDnsRec o) {
        final int cmp = 0xffff;
        if ((typ & cmp) < (o.typ & cmp)) {
            return -1;
        }
        if ((typ & cmp) > (o.typ & cmp)) {
            return +1;
        }
        return name.compareTo(o.name);
    }

}
