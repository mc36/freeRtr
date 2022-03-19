package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.ip.ipCor;
import net.freertr.ip.ipIcmp;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtDccp;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtLudp;
import net.freertr.prt.prtSctp;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * represents one sequenced listing
 *
 * @param <Te> type of one entry
 * @param <Ta> type of one address
 * @author matecsaba
 */
public class tabListing<Te extends tabListingEntry<Ta>, Ta extends addrType> {

    /**
     * name of prefix list
     */
    public String listName;

    /**
     * my ip handler
     */
    public ipCor myCor;

    /**
     * my icmp handler
     */
    public ipIcmp myIcmp;

    /**
     * my tcp handler
     */
    protected tabGen<Te> entries;

    /**
     * creates new listing
     */
    public tabListing() {
        if (debugger.tabListingEvnt) {
            logger.debug("create");
        }
        entries = new tabGen<Te>();
    }

    /**
     * get statistics
     *
     * @param wht what
     * @return statistics
     */
    public userFormat getStats(int wht) {
        String a = "";
        String b = "";
        if ((wht & 1) != 0) {
            b += "|2tx";
            a += "|byte|pack";
        }
        if ((wht & 2) != 0) {
            b += "|2rx";
            a += "|byte|pack";
        }
        if ((wht & 4) != 0) {
            b += "|2drop";
            a += "|byte|pack";
        }
        userFormat l = new userFormat("|", "seq" + a + "|last|timout|cfg", "1" + b + "|2timers|1");
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.sequence + ntry.getCounters(wht) + "|" + bits.timePast(ntry.lastMatch) + "|" + bits.timeDump(ntry.timeout / 1000) + "|" + ntry);
        }
        return l;
    }

    /**
     * copy core handlers
     *
     * @param src source
     */
    public void copyCores(tabListing<?, ?> src) {
        myCor = src.myCor;
        myIcmp = src.myIcmp;
    }

    public String toString() {
        return listName;
    }

    /**
     * clear all prefixes from list
     */
    public void clear() {
        if (debugger.tabListingEvnt) {
            logger.debug("clear");
        }
        entries.clear();
    }

    /**
     * clear counters
     */
    public void counterClear() {
        if (debugger.tabListingEvnt) {
            logger.debug("clear counters");
        }
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.cntr.clear();
            ntry.lastMatch = 0;
        }
    }

    /**
     * clear aged entries
     *
     * @return entries removed
     */
    public int purgeAged() {
        if (debugger.tabListingEvnt) {
            logger.debug("purge aged");
        }
        long tim = bits.getTime();
        int done = 0;
        for (int i = entries.size() - 1; i >= 0; i--) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.timeout < 1) {
                continue;
            }
            if ((tim - ntry.lastMatch) < ntry.timeout) {
                continue;
            }
            entries.del(ntry);
            done++;
        }
        if (done > 0) {
            reindex(0, 0);
        }
        return done;
    }

    /**
     * reindex this list
     *
     * @param beg initial number
     * @param inc increment number
     */
    public void reindex(int beg, int inc) {
        if (beg < 1) {
            beg = 10;
        }
        if (inc < 1) {
            inc = 10;
        }
        if (debugger.tabListingEvnt) {
            logger.debug("reindex beg=" + beg + " inc=" + inc);
        }
        for (int i = entries.size() - 1; i >= 0; i--) {
            Te prf = entries.get(i);
            prf.sequence = beg + (i * inc);
        }
    }

    /**
     * get next sequence number to give
     *
     * @return next sequence number to give
     */
    public int nextseq() {
        int i = entries.size();
        if (i < 1) {
            return 10;
        }
        Te ntry = entries.get(i - 1);
        if (ntry == null) {
            return 10;
        }
        return ntry.sequence + 10;
    }

    /**
     * add one table entry with preset values
     *
     * @param ntry entry to add
     * @return true if newly added, false if updated existing entry
     */
    public boolean add(Te ntry) {
        if (debugger.tabListingEvnt) {
            logger.debug("add " + ntry);
        }
        return entries.put(ntry) == null;
    }

    /**
     * delete one table entry
     *
     * @param ntry prefix to delete
     * @return false if deleted, true if not found
     */
    public boolean del(Te ntry) {
        if (debugger.tabListingEvnt) {
            logger.debug("del " + ntry);
        }
        return entries.del(ntry) == null;
    }

    /**
     * count entries in table
     *
     * @return number of entries
     */
    public int size() {
        return entries.size();
    }

    /**
     * read table entry
     *
     * @param idx sequence number 0..size-1
     * @return null if not found, the entry from table if found
     */
    public Te get(int idx) {
        return entries.get(idx);
    }

    /**
     * merge one list
     *
     * resequencing will happen
     *
     * @param l list
     * @param skip last entries to skip
     */
    public void mergeOne(tabListing<Te, Ta> l, int skip) {
        if (debugger.tabListingEvnt) {
            logger.debug("merge " + l.listName);
        }
        copyCores(l);
        listName = "merged from " + l.listName;
        for (int i = 0; i < (l.size() - skip); i++) {
            Te ntry = l.get(i);
            ntry.sequence = nextseq();
            entries.put(ntry);
        }
    }

    /**
     * merge two lists
     *
     * resequencing will happen
     *
     * @param l1 first
     * @param l2 second
     */
    public void mergeTwo(tabListing<Te, Ta> l1, tabListing<Te, Ta> l2) {
        if ((l1 == null) && (l2 == null)) {
            return;
        }
        if (l2 == null) {
            mergeOne(l1, 0);
            return;
        }
        if (l1 == null) {
            mergeOne(l2, 0);
            return;
        }
        mergeOne(l1, 1);
        mergeOne(l2, 0);
        listName = "merged from " + l1.listName + " and " + l2.listName;
    }

    /**
     * dump part of this table
     *
     * @param beg beginning to add to lines
     * @return string showing the table
     */
    public List<String> dump(String beg) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            List<String> res = ntry.usrString(beg);
            if (res.size() > 1) {
                res.add(beg + cmds.comment);
            }
            l.addAll(res);
        }
        return l;
    }

    /**
     * find one entry
     *
     * @param ntry entry to find
     * @return entry, null if not found
     */
    public Te find(Te ntry) {
        return entries.find(ntry);
    }

    /**
     * find prefix list entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network to test
     * @return entry, null if nothing found
     */
    public Te find(int afi, int asn, addrPrefix<Ta> net) {
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.matches(afi, asn, net)) {
                continue;
            }
            ntry.cntr.rx(new packHolder(false, false));
            ntry.lastMatch = bits.getTime();
            if (ntry.logMatch) {
                logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + net);
            }
            return ntry;
        }
        return null;
    }

    /**
     * find route table entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network to test
     * @return entry, null if nothing found
     */
    public Te find(int afi, int asn, tabRouteEntry<Ta> net) {
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.matches(afi, asn, net)) {
                continue;
            }
            ntry.cntr.rx(new packHolder(false, false));
            ntry.lastMatch = bits.getTime();
            if (ntry.logMatch) {
                logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + net);
            }
            return ntry;
        }
        return null;
    }

    /**
     * find packet
     *
     * @param pck packet to test
     * @return entry, null if nothing found
     */
    public Te find(packHolder pck) {
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.matches(pck)) {
                continue;
            }
            ntry.cntr.rx(pck);
            ntry.lastMatch = bits.getTime();
            if (ntry.logMatch) {
                logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + pck.ETHsrc + " " + pck.ETHtrg + " " + pck.IPprt + " " + pck.IPsrc + " " + pck.UDPsrc + " -> " + pck.IPtrg + " " + pck.UDPtrg);
            }
            return ntry;
        }
        return null;
    }

    /**
     * parse one packet
     *
     * @param l3 parse layer3 header
     * @param l4 parse layer4 header
     * @param chksiz check size
     * @param pck packet to parse
     */
    public void packParse(boolean l3, boolean l4, boolean chksiz, packHolder pck) {
        if (l3) {
            myCor.parseIPheader(pck, chksiz);
        }
        if (l4) {
            pck.getSkip(pck.IPsiz);
            tabQos.classifyLayer4(pck);
            pck.getSkip(-pck.IPsiz);
        }
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     */
    public void packUpdate(packHolder pck) {
        myCor.updateIPheader(pck, pck.IPsrc, pck.IPtrg, -1, -1, -1, -1, -1);
        pck.getSkip(pck.IPsiz);
        switch (pck.IPprt) {
            case prtTcp.protoNum:
                prtTcp.updateTCPheader(pck, pck.UDPsrc, pck.UDPtrg, -1, -1, -1);
                break;
            case prtUdp.protoNum:
                prtUdp.updateUDPheader(pck, pck.UDPsrc, pck.UDPtrg);
                break;
            case prtLudp.protoNum:
                prtLudp.updateLUDPheader(pck, pck.UDPsrc, pck.UDPtrg);
                break;
            case prtDccp.protoNum:
                prtDccp.updateDCCPheader(pck, pck.UDPsrc, pck.UDPtrg);
                break;
            case prtSctp.protoNum:
                prtSctp.updateSCTPheader(pck, pck.UDPsrc, pck.UDPtrg);
                break;
            default:
                if (pck.IPprt != myIcmp.getProtoNum()) {
                    break;
                }
                myIcmp.updateICMPheader(pck);
                break;
        }
        pck.getSkip(-pck.IPsiz);
    }

    /**
     * test one network against this prefix list
     *
     * @param afi address family
     * @param asn as number
     * @param net network to test
     * @return true if permitted, false if denied
     */
    public boolean matches(int afi, int asn, addrPrefix<Ta> net) {
        Te ntry = find(afi, asn, net);
        if (ntry == null) {
            return false;
        }
        return ntry.action == tabListingEntry.actionType.actPermit;
    }

    /**
     * test one network against this prefix list
     *
     * @param afi address family
     * @param asn as number
     * @param net network to test
     * @return true if permitted, false if denied
     */
    public boolean matches(int afi, int asn, tabRouteEntry<Ta> net) {
        Te ntry = find(afi, asn, net);
        if (ntry == null) {
            return false;
        }
        return ntry.action == tabListingEntry.actionType.actPermit;
    }

    /**
     * update one entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network number
     * @param copy copy before update
     * @return null if denied, copy otherwise
     */
    public tabRouteEntry<Ta> update(int afi, int asn, tabRouteEntry<Ta> net, boolean copy) {
        Te ntry = find(afi, asn, net);
        if (ntry == null) {
            return null;
        }
        if (ntry.action != tabListingEntry.actionType.actPermit) {
            return null;
        }
        if (copy) {
            net = net.copyBytes(tabRoute.addType.ecmp);
        }
        ntry.update(afi, asn, net);
        return net;
    }

    /**
     * test one packet against this access list
     *
     * @param l3 set true to parse layer3 header before
     * @param l4 set true to parse layer4 header before
     * @param pck packet to test
     * @return true if permitted, false if denied
     */
    public boolean matches(boolean l3, boolean l4, packHolder pck) {
        packParse(l3, l4, true, pck);
        Te ntry = find(pck);
        if (ntry == null) {
            return false;
        }
        return ntry.action == tabListingEntry.actionType.actPermit;
    }

    /**
     * test one connection against this access list
     *
     * @param conn connection to test
     * @return true if permitted, false if denied
     */
    public boolean matches(prtGenConn conn) {
        packHolder pck = new packHolder(false, false);
        pck.UDPsrc = conn.portRem;
        pck.UDPtrg = conn.portLoc;
        pck.IPsrc.setAddr(conn.peerAddr.copyBytes());
        pck.IPtrg.setAddr(conn.iface.addr);
        Te ntry = find(pck);
        if (ntry == null) {
            return false;
        }
        return ntry.action == tabListingEntry.actionType.actPermit;
    }

}
