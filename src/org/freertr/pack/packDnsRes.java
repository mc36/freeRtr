package org.freertr.pack;


import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * domain name protocol (rfc1035) resource record
 *
 * @author matecsaba
 */
public class packDnsRes implements Comparable<packDnsRes> {

    /**
     * create instance
     */
    public packDnsRes() {
    }

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

    public String toString() {
        return "target=" + target + " os=" + email + " addr=" + addr + " seq=" + sequence + " fresh=" + fresh + " retry=" + retry + " expire=" + expire + " minttl=" + minttl;
    }

    /**
     * copy bytes
     *
     * @return dns record
     */
    public packDnsRes copyBytes() {
        packDnsRes n = new packDnsRes();
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
     * convert to user string
     *
     * @param typ type of record
     * @return user string
     */
    public String toUserStr(int typ) {
        switch (typ) {
            case packDnsRec.typeCNAME:
                return target;
            case packDnsRec.typeHINFO:
                return target + " " + email;
            case packDnsRec.typeMB:
            case packDnsRec.typeMD:
            case packDnsRec.typeMF:
            case packDnsRec.typeMG:
            case packDnsRec.typeMR:
                return target;
            case packDnsRec.typeMINFO:
            case packDnsRec.typeRP:
                return target + " " + email;
            case packDnsRec.typeMX:
                return sequence + " " + target;
            case packDnsRec.typeSOA:
                return target + " " + email + " " + sequence + " " + fresh + " " + retry + " " + expire + " " + minttl;
            case packDnsRec.typeNULL:
            case packDnsRec.typeNS:
            case packDnsRec.typePTR:
            case packDnsRec.typeTXT:
                return target;
            case packDnsRec.typeSRV:
                return sequence + " " + fresh + " " + retry + " " + target;
            case packDnsRec.typeAAAA:
            case packDnsRec.typeA6:
            case packDnsRec.typeA:
                return "" + addr;
            case packDnsRec.typeInt4:
            case packDnsRec.typeInt6:
                return iface.name;
            default:
                return null;
        }

    }

    /**
     * convert from user string
     *
     * @param typ type of record
     * @param cmd command to parse
     * @return false if successful, true if error happened
     */
    public boolean fromUserStr(int typ, cmds cmd) {
        switch (typ) {
            case packDnsRec.typeA:
                addrIPv4 adr4 = new addrIPv4();
                if (adr4.fromString(cmd.word())) {
                    return true;
                }
                addr.fromIPv4addr(adr4);
                return false;
            case packDnsRec.typeAAAA:
                addrIPv6 adr6 = new addrIPv6();
                if (adr6.fromString(cmd.word())) {
                    return true;
                }
                addr.fromIPv6addr(adr6);
                return false;
            case packDnsRec.typeInt4:
                iface = cfgAll.ifcFind(cmd.word(), 0);
                if (iface == null) {
                    cmd.error("no such interface");
                    return true;
                }
                return false;
            case packDnsRec.typeInt6:
                iface = cfgAll.ifcFind(cmd.word(), 0);
                if (iface == null) {
                    cmd.error("no such interface");
                    return true;
                }
                return false;
            case packDnsRec.typeCNAME:
                target = cmd.word();
                return false;
            case packDnsRec.typePTR:
                target = cmd.word();
                return false;
            case packDnsRec.typeNS:
                target = cmd.word();
                return false;
            case packDnsRec.typeRP:
                target = cmd.word();
                email = cmd.word();
                return false;
            case packDnsRec.typeMX:
                sequence = bits.str2num(cmd.word());
                target = cmd.word();
                return false;
            case packDnsRec.typeTXT:
                target = cmd.getRemaining();
                return false;
            case packDnsRec.typeSRV:
                sequence = bits.str2num(cmd.word());
                fresh = bits.str2num(cmd.word());
                retry = bits.str2num(cmd.word());
                target = cmd.getRemaining();
                return false;
            case packDnsRec.typeHINFO:
                target = cmd.word();
                email = cmd.word();
                return false;
            case packDnsRec.typeSOA:
                target = cmd.word();
                email = cmd.word();
                sequence = bits.str2num(cmd.word());
                fresh = bits.str2num(cmd.word());
                retry = bits.str2num(cmd.word());
                expire = bits.str2num(cmd.word());
                minttl = bits.str2num(cmd.word());
                return false;
            default:
                return true;
        }
    }

    /**
     * parse header
     *
     * @param typ type of record
     * @param pck packet to use
     * @param len message ending
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(int typ, packHolder pck, int len) {
        switch (typ) {
            case packDnsRec.typeCNAME:
                target = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeHINFO:
                target = packDnsRec.getString(pck);
                email = packDnsRec.getString(pck);
                return false;
            case packDnsRec.typeMB:
            case packDnsRec.typeMD:
            case packDnsRec.typeMF:
            case packDnsRec.typeMG:
            case packDnsRec.typeMR:
                target = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeMINFO:
            case packDnsRec.typeRP:
                target = packDnsRec.getChain(pck, len, 0);
                email = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeMX:
                sequence = pck.msbGetW(0);
                pck.getSkip(2);
                target = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeNS:
            case packDnsRec.typePTR:
                target = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeSOA:
                target = packDnsRec.getChain(pck, len, 0);
                email = packDnsRec.getChain(pck, len, 0);
                sequence = pck.msbGetD(0);
                fresh = pck.msbGetD(4);
                retry = pck.msbGetD(8);
                expire = pck.msbGetD(12);
                minttl = pck.msbGetD(16);
                pck.getSkip(20);
                return false;
            case packDnsRec.typeNULL:
            case packDnsRec.typeTXT:
                target = packDnsRec.getString(pck);
                return false;
            case packDnsRec.typeSRV:
                sequence = pck.msbGetW(0);
                fresh = pck.msbGetW(2);
                retry = pck.msbGetW(4);
                pck.getSkip(6);
                target = packDnsRec.getChain(pck, len, 0);
                return false;
            case packDnsRec.typeA:
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                addr.fromIPv4addr(adr4);
                return false;
            case packDnsRec.typeAAAA:
            case packDnsRec.typeA6:
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 0);
                addr.fromIPv6addr(adr6);
                return false;
            default:
                return true;
        }
    }

    /**
     * create header
     *
     * @param typ type of record
     * @param pck packet to use
     */
    public void createHeader(int typ, packHolder pck) {
        int ofs = pck.headSize();
        switch (typ) {
            case packDnsRec.typeCNAME:
                packDnsRec.putDomain(pck, target);
                break;
            case packDnsRec.typeHINFO:
                packDnsRec.putString(pck, target);
                packDnsRec.putString(pck, email);
                break;
            case packDnsRec.typeMB:
            case packDnsRec.typeMD:
            case packDnsRec.typeMF:
            case packDnsRec.typeMG:
            case packDnsRec.typeMR:
                packDnsRec.putDomain(pck, target);
                break;
            case packDnsRec.typeMINFO:
            case packDnsRec.typeRP:
                packDnsRec.putDomain(pck, target);
                packDnsRec.putDomain(pck, email);
                break;
            case packDnsRec.typeMX:
                pck.msbPutW(0, sequence);
                pck.putSkip(2);
                packDnsRec.putDomain(pck, target);
                break;
            case packDnsRec.typeNULL:
            case packDnsRec.typeTXT:
                packDnsRec.putString(pck, target);
                break;
            case packDnsRec.typeSRV:
                pck.msbPutW(0, sequence);
                pck.msbPutW(2, fresh);
                pck.msbPutW(4, retry);
                pck.putSkip(6);
                packDnsRec.putDomain(pck, target);
                break;
            case packDnsRec.typeNS:
            case packDnsRec.typePTR:
                packDnsRec.putDomain(pck, target);
                break;
            case packDnsRec.typeSOA:
                packDnsRec.putDomain(pck, target);
                packDnsRec.putDomain(pck, email);
                pck.msbPutD(0, sequence);
                pck.msbPutD(4, fresh);
                pck.msbPutD(8, retry);
                pck.msbPutD(12, expire);
                pck.msbPutD(16, minttl);
                pck.putSkip(20);
                break;
            case packDnsRec.typeA:
                addrIPv4 adr4 = addr.toIPv4();
                pck.putAddr(0, adr4);
                pck.putSkip(addrIPv4.size);
                break;
            case packDnsRec.typeAAAA:
            case packDnsRec.typeA6:
                addrIPv6 adr6 = addr.toIPv6();
                pck.putAddr(0, adr6);
                pck.putSkip(addrIPv6.size);
                break;
            case packDnsRec.typeInt4:
                adr4 = iface.addr4;
                pck.putAddr(0, adr4);
                pck.putSkip(addrIPv4.size);
                break;
            case packDnsRec.typeInt6:
                adr6 = iface.addr6;
                pck.putAddr(0, adr6);
                pck.putSkip(addrIPv6.size);
                break;
            default:
                break;
        }
        int siz = pck.headSize() - ofs;
        pck.msbPutW(-siz - 2, siz);
    }

    public int compareTo(packDnsRes o) {
        if (sequence < o.sequence) {
            return -1;
        }
        if (sequence > o.sequence) {
            return +1;
        }
        if (fresh < o.fresh) {
            return -1;
        }
        if (fresh > o.fresh) {
            return +1;
        }
        if (retry < o.retry) {
            return -1;
        }
        if (retry > o.retry) {
            return +1;
        }
        if (expire < o.expire) {
            return -1;
        }
        if (expire > o.expire) {
            return +1;
        }
        if (minttl < o.minttl) {
            return -1;
        }
        if (minttl > o.minttl) {
            return +1;
        }
        int i = addr.compareTo(o.addr);
        if (i != 0) {
            return i;
        }
        i = target.compareTo(o.target);
        if (i != 0) {
            return i;
        }
        i = email.compareTo(o.email);
        if (i != 0) {
            return i;
        }
        if (iface == null) {
            if (o.iface == null) {
                return 0;
            }
            return +1;
        } else {
            if (o.iface == null) {
                return -1;
            }
            return iface.compareTo(o.iface);
        }
    }

}
