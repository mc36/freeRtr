package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cfg.cfgAll;
import cfg.cfgIfc;
import java.util.Comparator;
import util.bits;
import util.cmds;

/**
 * domain name protocol (rfc1035) resource record
 *
 * @author matecsaba
 */
public class packDnsRec implements Comparator<packDnsRec> {

    /**
     * time of addition
     */
    public long added;

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
     * target address
     */
    public String target = "";

    /**
     * email address
     */
    public String email = "";

    /**
     * sequence
     */
    public int sequence;

    /**
     * refresh
     */
    public int fresh;

    /**
     * retry
     */
    public int retry;

    /**
     * expire
     */
    public int expire;

    /**
     * min ttl
     */
    public int minttl;

    /**
     * address
     */
    public addrIP addr = new addrIP();

    /**
     * interface
     */
    public cfgIfc iface;

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
        n.name = name;
        n.typ = typ;
        n.clss = clss;
        n.ttl = ttl;
        n.target = target;
        n.email = email;
        n.sequence = sequence;
        n.fresh = fresh;
        n.retry = retry;
        n.expire = expire;
        n.minttl = minttl;
        n.addr = addr.copyBytes();
        n.iface = iface;
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
        byte buf[] = adr.getBytes();
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
        byte buf[] = adr.getBytes();
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

    private static String getPointer(packHolder pck, int beg) {
        int i = pck.msbGetW(0) & 0x3fff;
        pck.getSkip(2);
        int o = pck.dataSize();
        pck.setBytesLeft(beg - i);
        String s = getChain(pck, beg);
        pck.setBytesLeft(o);
        return s;
    }

    private static String getString(packHolder pck) {
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
     * @param beg where to start
     * @return readed domain name
     */
    public static String getChain(packHolder pck, int beg) {
        String s = "";
        for (;;) {
            int i = pck.getByte(0);
            if ((i & 0xc0) == 0xc0) {
                s += "." + getPointer(pck, beg);
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

    private static void putString(packHolder pck, String s) {
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
        return "name=" + name + " class=" + class2str(clss) + " type=" + type2str(typ) + " ttl=" + ttl + " target=" + target + " os=" + email + " addr=" + addr + " seq=" + sequence + " fresh=" + fresh + " retry=" + retry + " expire=" + expire + " minttl=" + minttl;
    }

    /**
     * convert to user string
     *
     * @param sep separator
     * @return user string
     */
    public String toUserStr(String sep) {
        String s = "";
        switch (typ) {
            case typeCNAME:
                s = target;
                break;
            case typeHINFO:
                s = target + " " + email;
                break;
            case typeMB:
            case typeMD:
            case typeMF:
            case typeMG:
            case typeMR:
                s = target;
                break;
            case typeMINFO:
            case typeRP:
                s = target + " " + email;
                break;
            case typeMX:
                s = sequence + " " + target;
                break;
            case typeSOA:
                s = target + " " + email + " " + sequence + " " + fresh + " " + retry + " " + expire + " " + minttl;
                break;
            case typeNULL:
            case typeNS:
            case typePTR:
            case typeTXT:
                s = target;
                break;
            case typeSRV:
                s = sequence + " " + fresh + " " + retry + " " + target;
                break;
            case typeAAAA:
            case typeA6:
            case typeA:
                s = "" + addr;
                break;
            case typeInt4:
            case typeInt6:
                s = iface.name;
                break;
        }
        return name + sep + type2str(typ) + sep + s;
    }

    /**
     * convert from user string
     *
     * @param cmd command to parse
     * @return false if successful, true if error happened
     */
    public boolean fromUserStr(cmds cmd) {
        name = cmd.word();
        if (name.toLowerCase().equals("rev")) {
            name = cmd.word();
            addrIP adr = new addrIP();
            if (adr.fromString(name)) {
                return true;
            }
            name = generateReverse(adr);
        }
        String s = cmd.word().trim().toLowerCase();
        if (s.equals("ip4a")) {
            addrIPv4 adr = new addrIPv4();
            if (adr.fromString(cmd.word())) {
                return true;
            }
            addr.fromIPv4addr(adr);
            typ = typeA;
            return false;
        }
        if (s.equals("ip6a")) {
            addrIPv6 adr = new addrIPv6();
            if (adr.fromString(cmd.word())) {
                return true;
            }
            addr.fromIPv6addr(adr);
            typ = typeAAAA;
            return false;
        }
        if (s.equals("ip4i")) {
            iface = cfgAll.ifcFind(cmd.word(), false);
            if (iface == null) {
                cmd.error("no such interface");
                return true;
            }
            typ = typeInt4;
            return false;
        }
        if (s.equals("ip6i")) {
            iface = cfgAll.ifcFind(cmd.word(), false);
            if (iface == null) {
                cmd.error("no such interface");
                return true;
            }
            typ = typeInt6;
            return false;
        }
        if (s.equals("cname")) {
            target = cmd.word();
            typ = typeCNAME;
            return false;
        }
        if (s.equals("ptr")) {
            target = cmd.word();
            typ = typePTR;
            return false;
        }
        if (s.equals("ns")) {
            target = cmd.word();
            typ = typeNS;
            return false;
        }
        if (s.equals("rp")) {
            target = cmd.word();
            email = cmd.word();
            typ = typeRP;
            return false;
        }
        if (s.equals("mx")) {
            sequence = bits.str2num(cmd.word());
            target = cmd.word();
            typ = typeMX;
            return false;
        }
        if (s.equals("txt")) {
            target = cmd.getRemaining();
            typ = typeTXT;
            return false;
        }
        if (s.equals("srv")) {
            sequence = bits.str2num(cmd.word());
            fresh = bits.str2num(cmd.word());
            retry = bits.str2num(cmd.word());
            target = cmd.getRemaining();
            typ = typeSRV;
            return false;
        }
        if (s.equals("hinfo")) {
            target = cmd.word();
            email = cmd.word();
            typ = typeHINFO;
            return false;
        }
        if (s.equals("soa")) {
            target = cmd.word();
            email = cmd.word();
            sequence = bits.str2num(cmd.word());
            fresh = bits.str2num(cmd.word());
            retry = bits.str2num(cmd.word());
            expire = bits.str2num(cmd.word());
            minttl = bits.str2num(cmd.word());
            typ = typeSOA;
            return false;
        }
        return true;
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @param beg message beginning
     * @param question parse just header part
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck, int beg, boolean question) {
        if (pck.dataSize() < 1) {
            return true;
        }
        name = getChain(pck, beg);
        typ = pck.msbGetW(0); // type
        clss = pck.msbGetW(2); // class
        pck.getSkip(4);
        if (question) {
            return false;
        }
        ttl = pck.msbGetD(0); // time to live
        int siz = pck.msbGetW(4); // size of rdata
        pck.getSkip(6);
        siz = pck.dataSize() - siz;
        switch (typ) {
            case typeCNAME:
                target = getChain(pck, beg);
                break;
            case typeHINFO:
                target = getString(pck);
                email = getString(pck);
                break;
            case typeMB:
            case typeMD:
            case typeMF:
            case typeMG:
            case typeMR:
                target = getChain(pck, beg);
                break;
            case typeMINFO:
            case typeRP:
                target = getChain(pck, beg);
                email = getChain(pck, beg);
                break;
            case typeMX:
                sequence = pck.msbGetW(0);
                pck.getSkip(2);
                target = getChain(pck, beg);
                break;
            case typeNS:
            case typePTR:
                target = getChain(pck, beg);
                break;
            case typeSOA:
                target = getChain(pck, beg);
                email = getChain(pck, beg);
                sequence = pck.msbGetD(0);
                fresh = pck.msbGetD(4);
                retry = pck.msbGetD(8);
                expire = pck.msbGetD(12);
                minttl = pck.msbGetD(16);
                pck.getSkip(20);
                break;
            case typeNULL:
            case typeTXT:
                target = getString(pck);
                break;
            case typeSRV:
                sequence = pck.msbGetW(0);
                fresh = pck.msbGetW(2);
                retry = pck.msbGetW(4);
                pck.getSkip(6);
                target = getChain(pck, beg);
                break;
            case typeA:
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                addr.fromIPv4addr(adr4);
                break;
            case typeAAAA:
            case typeA6:
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 0);
                addr.fromIPv6addr(adr6);
                break;
            default:
                break;
        }
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
        int siz = pck.headSize();
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
        int ofs = pck.headSize();
        switch (typ) {
            case typeCNAME:
                putDomain(pck, target);
                break;
            case typeHINFO:
                putString(pck, target);
                putString(pck, email);
                break;
            case typeMB:
            case typeMD:
            case typeMF:
            case typeMG:
            case typeMR:
                putDomain(pck, target);
                break;
            case typeMINFO:
            case typeRP:
                putDomain(pck, target);
                putDomain(pck, email);
                break;
            case typeMX:
                pck.msbPutW(0, sequence);
                pck.putSkip(2);
                putDomain(pck, target);
                break;
            case typeNULL:
            case typeTXT:
                putString(pck, target);
                break;
            case typeSRV:
                pck.msbPutW(0, sequence);
                pck.msbPutW(2, fresh);
                pck.msbPutW(4, retry);
                pck.putSkip(6);
                putDomain(pck, target);
                break;
            case typeNS:
            case typePTR:
                putDomain(pck, target);
                break;
            case typeSOA:
                putDomain(pck, target);
                putDomain(pck, email);
                pck.msbPutD(0, sequence);
                pck.msbPutD(4, fresh);
                pck.msbPutD(8, retry);
                pck.msbPutD(12, expire);
                pck.msbPutD(16, minttl);
                pck.putSkip(20);
                break;
            case typeA:
                addrIPv4 adr4 = addr.toIPv4();
                pck.putAddr(0, adr4);
                pck.putSkip(addrIPv4.size);
                break;
            case typeAAAA:
            case typeA6:
                addrIPv6 adr6 = addr.toIPv6();
                pck.putAddr(0, adr6);
                pck.putSkip(addrIPv6.size);
                break;
            case typeInt4:
                adr4 = iface.addr4;
                pck.putAddr(0, adr4);
                pck.putSkip(addrIPv4.size);
                break;
            case typeInt6:
                adr6 = iface.addr6;
                pck.putAddr(0, adr6);
                pck.putSkip(addrIPv6.size);
                break;
            default:
                break;
        }
        siz = pck.headSize() - ofs;
        pck.msbPutW(-siz - 2, siz);
    }

    public int compare(packDnsRec o1, packDnsRec o2) {
        final int cmp = 0xffff;
        if ((o1.typ & cmp) < (o2.typ & cmp)) {
            return -1;
        }
        if ((o1.typ & cmp) > (o2.typ & cmp)) {
            return +1;
        }
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

}
