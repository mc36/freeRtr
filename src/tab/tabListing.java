package tab;

import addr.addrPrefix;
import addr.addrType;
import ip.ipCor;
import ip.ipIcmp;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import prt.prtDccp;
import prt.prtGenConn;
import prt.prtSctp;
import prt.prtTcp;
import prt.prtUdp;
import prt.prtLudp;
import util.cmds;
import util.debugger;
import util.logger;

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
     * @return statistics
     */
    public List<String> getStats() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            l.addAll(ntry.usrString(" "));
            l.add("  match=" + ntry.getCounters());
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
        for (;;) {
            int i = entries.size();
            if (i < 1) {
                return 10;
            }
            Te ntry = entries.get(i - 1);
            if (ntry == null) {
                continue;
            }
            return ntry.sequence + 10;
        }
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
     * @param net network to test
     * @return entry, null if nothing found
     */
    public Te find(int afi, addrPrefix<Ta> net) {
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.matches(afi, net)) {
                ntry.countPack++;
                if (ntry.logMatch) {
                    logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + net);
                }
                return ntry;
            }
        }
        return null;
    }

    /**
     * find route table entry
     *
     * @param afi address family
     * @param net network to test
     * @return entry, null if nothing found
     */
    public Te find(int afi, tabRouteEntry<Ta> net) {
        for (int i = 0; i < entries.size(); i++) {
            Te ntry = entries.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.matches(afi, net)) {
                ntry.countPack++;
                if (ntry.logMatch) {
                    logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + net);
                }
                return ntry;
            }
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
            if (ntry.matches(pck)) {
                ntry.countPack += 1;
                ntry.countByte += pck.dataSize();
                if (ntry.logMatch) {
                    logger.info("list " + listName + " matched at sequence " + ntry.sequence + " on " + pck.ETHsrc + " " + pck.ETHtrg + " " + pck.IPprt + " " + pck.IPsrc + " " + pck.UDPsrc + " -> " + pck.IPtrg + " " + pck.UDPtrg);
                }
                return ntry;
            }
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
            pck.UDPsrc = 0;
            pck.UDPtrg = 0;
            pck.UDPsiz = 0;
            switch (pck.IPprt) {
                case prtTcp.protoNum:
                    prtTcp.parseTCPports(pck);
                    break;
                case prtUdp.protoNum:
                    prtUdp.parseUDPports(pck);
                    break;
                case prtLudp.protoNum:
                    prtLudp.parseLUDPports(pck);
                    break;
                case prtDccp.protoNum:
                    prtDccp.parseDCCPports(pck);
                    break;
                case prtSctp.protoNum:
                    prtSctp.parseSCTPports(pck);
                    break;
                default:
                    if (pck.IPprt != myIcmp.getProtoNum()) {
                        break;
                    }
                    myIcmp.parseICMPheader(pck);
                    pck.UDPtrg = pck.UDPsrc;
                    break;
            }
            pck.getSkip(-pck.IPsiz);
        }
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     */
    public void packUpdate(packHolder pck) {
        myCor.updateIPheader(pck, pck.IPsrc, pck.IPtrg, -1, -1, -1, -1);
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
     * @param net network to test
     * @return true if permitted, false if denied
     */
    public boolean matches(int afi, addrPrefix<Ta> net) {
        Te ntry = find(afi, net);
        if (ntry == null) {
            return false;
        }
        return ntry.action == tabPlcmapN.actionType.actPermit;
    }

    /**
     * update one entry
     *
     * @param afi address family
     * @param net network number
     * @param copy copy before update
     * @return null if denied, copy otherwise
     */
    public tabRouteEntry<Ta> update(int afi, tabRouteEntry<Ta> net, boolean copy) {
        Te ntry = find(afi, net);
        if (ntry == null) {
            return null;
        }
        if (ntry.action == tabPlcmapN.actionType.actDeny) {
            return null;
        }
        if (copy) {
            net = net.copyBytes();
        }
        ntry.update(afi, net);
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
        return ntry.action == tabPlcmapN.actionType.actPermit;
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
        return ntry.action == tabPlcmapN.actionType.actPermit;
    }

}
