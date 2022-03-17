package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ip.ipIcmp;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * represents one nat config (source/target, orig/new)
 *
 * @author matecsaba
 */
public class tabNatCfgN extends tabListingEntry<addrIP> {

    /**
     * matching protocol
     */
    public int protocol = -1;

    /**
     * matching mask
     */
    public addrIP mask;

    /**
     * negated mask
     */
    public addrIP maskNot;

    /**
     * original source address
     */
    public addrIP origSrcAddr;

    /**
     * source access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> origSrcList;

    /**
     * original source interface
     */
    public cfgIfc origTrgIface;

    /**
     * original target address
     */
    public addrIP origTrgAddr;

    /**
     * original source port
     */
    public int origSrcPort = -1;

    /**
     * original target port
     */
    public int origTrgPort = -1;

    /**
     * new source address
     */
    public addrIP newSrcAddr;

    /**
     * new target address
     */
    public addrIP newTrgAddr;

    /**
     * new target interface
     */
    public cfgIfc newSrcIface;

    /**
     * new source port
     */
    public int newSrcPort = -1;

    /**
     * new target port
     */
    public int newTrgPort = -1;

    /**
     * port range
     */
    public int rangeMin = -1;

    /**
     * port range
     */
    public int rangeMax = -1;

    /**
     * log translations
     */
    public boolean logTrans = false;

    /**
     * create instance
     */
    public tabNatCfgN() {
        timeout = 300 * 1000;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return 0=ok, 1=error, 2=time, 3=range, 4=log
     */
    public int fromString(String s) {
        cmds cmd = new cmds("", s);
        int what = 0; // 1=source, 2=target
        s = cmd.word();
        if (s.equals("sequence")) {
            sequence = bits.str2num(cmd.word());
            s = cmd.word();
        }
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return 2;
        }
        if (s.equals("sessions")) {
            maxSess = bits.str2num(cmd.word());
            return 2;
        }
        if (s.equals("randomize")) {
            rangeMin = bits.str2num(cmd.word());
            rangeMax = bits.str2num(cmd.word());
            return 3;
        }
        if (s.equals("log-translations")) {
            logTrans = true;
            return 4;
        }
        if (s.equals("source")) {
            what = 1;
        }
        if (s.equals("target")) {
            what = 2;
        }
        if (s.equals("srcport")) {
            protocol = bits.str2num(cmd.word());
            what = 1;
        }
        if (s.equals("trgport")) {
            protocol = bits.str2num(cmd.word());
            what = 2;
        }
        if (s.equals("srclist")) {
            what = 3;
        }
        if (s.equals("srcpref")) {
            what = 1;
            mask = new addrIP();
        }
        if (s.equals("trgpref")) {
            what = 2;
            mask = new addrIP();
        }
        if (what < 1) {
            return 1;
        }
        addrIP orgA = new addrIP();
        addrIP newA = new addrIP();
        int orgP = -1;
        int newP = -1;
        if (what == 3) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                return 1;
            }
            origSrcList = acl.aceslst;
            orgA = null;
        } else {
            s = cmd.word();
            if (s.equals("interface")) {
                origTrgIface = cfgAll.ifcFind(cmd.word(), false);
                if (origTrgIface == null) {
                    cmd.error("no such interface");
                    return 1;
                }
            } else if (orgA.fromString(s)) {
                return 1;
            }
        }
        if (protocol >= 0) {
            orgP = bits.str2num(cmd.word());
        }
        s = cmd.word();
        if (s.equals("interface")) {
            newSrcIface = cfgAll.ifcFind(cmd.word(), false);
            if (newSrcIface == null) {
                cmd.error("no such interface");
                return 1;
            }
        } else {
            if (newA.fromString(s)) {
                return 1;
            }
        }
        if (protocol >= 0) {
            newP = bits.str2num(cmd.word());
        }
        if (mask != null) {
            if (mask.fromString(cmd.word())) {
                return 1;
            }
            orgA.setAnd(orgA, mask);
            newA.setAnd(newA, mask);
            maskNot = new addrIP();
            maskNot.setNot(mask);
        }
        if (what == 2) {
            origTrgAddr = orgA;
            origTrgPort = orgP;
            newTrgAddr = newA;
            newTrgPort = newP;
        } else {
            origSrcAddr = orgA;
            origSrcPort = orgP;
            newSrcAddr = newA;
            newSrcPort = newP;
        }
        return 0;
    }

    public String toString() {
        int what = 0;
        addrIP orgA = new addrIP();
        addrIP newA = new addrIP();
        int orgP = -1;
        int newP = -1;
        if (origSrcAddr != null) {
            what = 1;
            orgA = origSrcAddr;
            orgP = origSrcPort;
            newA = newSrcAddr;
            newP = newSrcPort;
        } else {
            what = 2;
            orgA = origTrgAddr;
            orgP = origTrgPort;
            newA = newTrgAddr;
            newP = newTrgPort;
        }
        if (origSrcList != null) {
            what = 3;
            newA = newSrcAddr;
        }
        if (protocol >= 0) {
            what |= 4;
        }
        if (mask != null) {
            what |= 8;
        }
        String s;
        switch (what) {
            case 1:
                s = "source";
                break;
            case 2:
                s = "target";
                break;
            case 3:
                s = "srclist";
                break;
            case 5:
                s = "srcport";
                break;
            case 6:
                s = "trgport";
                break;
            case 9:
                s = "srcpref";
                break;
            case 10:
                s = "trgpref";
                break;
            default:
                s = "unknown";
                break;
        }
        if ((what & 4) != 0) {
            s = s + " " + protocol;
        }
        if (what == 3) {
            s = s + " " + origSrcList.listName;
        } else {
            if (origTrgIface == null) {
                s = s + " " + orgA;
            } else {
                s = s + " interface " + origTrgIface.name;
            }
        }
        if ((what & 4) != 0) {
            s = s + " " + orgP;
        }
        if (newSrcIface == null) {
            s = s + " " + newA;
        } else {
            s = s + " interface " + newSrcIface.name;
        }
        if ((what & 4) != 0) {
            s = s + " " + newP;
        }
        if ((what & 8) != 0) {
            s = s + " " + mask;
        }
        return s;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @return string
     */
    public List<String> usrString(String beg) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + this);
        String s = beg + "sequence " + sequence;
        l.add(s + " timeout " + timeout);
        l.add(s + " sessions " + maxSess);
        if (rangeMin > 0) {
            l.add(s + " randomize " + rangeMin + " " + rangeMax);
        }
        if (logTrans) {
            l.add(s + " log-translations");
        }
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if ((protocol >= 0) && (pck.IPprt != protocol)) {
            return false;
        }
        if ((origSrcPort >= 0) && (pck.UDPsrc != origSrcPort)) {
            return false;
        }
        if ((origTrgPort >= 0) && (pck.UDPtrg != origTrgPort)) {
            return false;
        }
        if (origSrcList != null) {
            if (!origSrcList.matches(false, false, pck)) {
                return false;
            }
        }
        if (mask != null) {
            addrIP adr = new addrIP();
            if (origSrcAddr != null) {
                adr.setAnd(pck.IPsrc, mask);
                if (origSrcAddr.compare(origSrcAddr, adr) != 0) {
                    return false;
                }
            }
            if (origTrgAddr != null) {
                adr.setAnd(pck.IPtrg, mask);
                if (origTrgAddr.compare(origTrgAddr, adr) != 0) {
                    return false;
                }
            }
            return true;
        }
        if (origSrcAddr != null) {
            if (!usableAddr(origSrcAddr)) {
                return false;
            }
            if (origSrcAddr.compare(origSrcAddr, pck.IPsrc) != 0) {
                return false;
            }
        }
        if (origTrgAddr != null) {
            if (!usableAddr(origTrgAddr)) {
                return false;
            }
            if (origTrgAddr.compare(origTrgAddr, pck.IPtrg) != 0) {
                return false;
            }
        }
        if (newSrcAddr != null) {
            if (!usableAddr(newSrcAddr)) {
                return false;
            }
        }
        return true;
    }

    private boolean usableAddr(addrIP addr) {
        if (addr.isIPv4()) {
            addrIPv4 adr = addr.toIPv4();
            if (adr.isFilled(0)) {
                return false;
            }
            if (!adr.isUnicast()) {
                return false;
            }
        } else {
            addrIPv6 adr = addr.toIPv6();
            if (adr.isFilled(0)) {
                return false;
            }
            if (!adr.isUnicast()) {
                return false;
            }
            if (adr.isLinkLocal()) {
                return false;
            }
        }
        return true;
    }

    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

    /**
     * create entry
     *
     * @param pck packet to use
     * @param icc icmp core
     * @return newly created entry
     */
    public tabNatTraN createEntry(packHolder pck, ipIcmp icc) {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = bits.getTime();
        n.created = n.lastUsed;
        n.timeout = timeout;
        n.protocol = pck.IPprt;
        n.origSrcAddr = pck.IPsrc.copyBytes();
        n.origTrgAddr = pck.IPtrg.copyBytes();
        n.origSrcPort = pck.UDPsrc;
        n.origTrgPort = pck.UDPtrg;
        n.newSrcAddr = pck.IPsrc.copyBytes();
        n.newTrgAddr = pck.IPtrg.copyBytes();
        n.newSrcPort = pck.UDPsrc;
        n.newTrgPort = pck.UDPtrg;
        if (mask == null) {
            if (newSrcAddr != null) {
                n.newSrcAddr = newSrcAddr.copyBytes();
            }
            if (newTrgAddr != null) {
                n.newTrgAddr = newTrgAddr.copyBytes();
            }
        } else {
            addrIP adr = new addrIP();
            if (newSrcAddr != null) {
                adr.setAnd(pck.IPsrc, maskNot);
                adr.setOr(adr, newSrcAddr);
                n.newSrcAddr = adr.copyBytes();
            }
            if (newTrgAddr != null) {
                adr.setAnd(pck.IPtrg, maskNot);
                adr.setOr(adr, newTrgAddr);
                n.newTrgAddr = adr.copyBytes();
            }
        }
        if (newSrcPort >= 0) {
            n.newSrcPort = newSrcPort;
        }
        if (newTrgPort >= 0) {
            n.newTrgPort = newTrgPort;
        }
        if (rangeMin < 1) {
            return n;
        }
        if (rangeMax < 1) {
            return n;
        }
        if (n.protocol == icc.getProtoNum()) {
            return n;
        }
        n.newSrcPort = bits.random(rangeMin, rangeMax);
        n.logEnd = logTrans;
        if (logTrans) {
            logger.info("creating translation " + n);
        }
        return n;
    }

}
