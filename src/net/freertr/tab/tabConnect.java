package net.freertr.tab;

import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.user.userFormat;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * keeps track of connections or listeners
 *
 * @param <Ta> address it works with
 * @param <Td> this type will be kept in records
 * @author matecsaba
 */
public class tabConnect<Ta extends addrType, Td extends tabConnectLower> {

    /**
     * connections
     */
    private final tabGen<tabConnectEntry<Ta, Td>> conns;

    /**
     * name of table
     */
    public final String tabName;

    public String toString() {
        return tabName + " table";
    }

    /**
     * create one connection table
     *
     * @param newAdr a newly created address
     * @param nam name of table
     */
    public tabConnect(Ta newAdr, String nam) {
        if (debugger.tabConnectEvnt) {
            logger.debug("create");
        }
        conns = new tabGen<tabConnectEntry<Ta, Td>>();
        tabName = nam;
    }

    /**
     * add one entry to this table
     *
     * @param locInt local interface, null means all
     * @param remAdr remote address, null means all
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @param data data to store
     * @param name name of connection
     * @return false if successful, true if already found
     */
    @SuppressWarnings("unchecked")
    public boolean add(tabRouteIface locInt, Ta remAdr, int locPrt, int remPrt, Td data, String name) {
        if (data == null) {
            return true;
        }
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        if (remAdr != null) {
            ntry.peer = (Ta) remAdr.copyBytes();
        }
        ntry.local = locPrt;
        ntry.remote = remPrt;
        ntry.name = name;
        ntry.data = data;
        if (debugger.tabConnectEvnt) {
            logger.debug("adding " + ntry.dump());
        }
        conns.add(ntry);
        return false;
    }

    /**
     * read one entry
     *
     * @param locInt local interface, null means all
     * @param remAdr remote address, null means all
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @return stored value, null if not found
     */
    public Td get(tabRouteIface locInt, Ta remAdr, int locPrt, int remPrt) {
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = remAdr;
        ntry.local = locPrt;
        ntry.remote = remPrt;
        ntry = conns.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.hits++;
        return ntry.data;
    }

    /**
     * read one entry
     *
     * @param i index
     * @return stored value, null if not found
     */
    public Td get(int i) {
        tabConnectEntry<Ta, Td> ntry = conns.get(i);
        if (ntry == null) {
            return null;
        }
        return ntry.data;
    }

    /**
     * read one entry
     *
     * @param i index
     * @return value, null if not found
     */
    public tabConnectEntry<Ta, Td> read(int i) {
        return conns.get(i);
    }

    /**
     * get number of entries
     *
     * @return number of entries
     */
    public int size() {
        return conns.size();
    }

    /**
     * delete one entry
     *
     * @param locInt local interface, null means all
     * @param remAdr remote address, null means all
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @return deleted value, null if not found
     */
    public Td delNext(tabRouteIface locInt, Ta remAdr, int locPrt, int remPrt) {
        for (int i = 0; i < conns.size(); i++) {
            tabConnectEntry<Ta, Td> ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (locInt != null) {
                if (locInt != ntry.iface) {
                    continue;
                }
            }
            if (locPrt > 0) {
                if (locPrt != ntry.local) {
                    continue;
                }
            }
            if (remPrt > 0) {
                if (remPrt != ntry.remote) {
                    continue;
                }
            }
            if (remAdr != null) {
                if (ntry.peer == null) {
                    continue;
                }
                if (remAdr.compare(remAdr, ntry.peer) != 0) {
                    continue;
                }
            }
            ntry = conns.del(ntry);
            if (ntry == null) {
                continue;
            }
            return ntry.data;
        }
        return null;
    }

    /**
     * delete one entry
     *
     * @param locInt local interface, 0 means all
     * @param remAdr remote address, null means all
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @return deleted value, null if not found
     */
    public Td del(tabRouteIface locInt, Ta remAdr, int locPrt, int remPrt) {
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = remAdr;
        ntry.local = locPrt;
        ntry.remote = remPrt;
        if (debugger.tabConnectEvnt) {
            logger.debug("del " + ntry.dump());
        }
        ntry = conns.del(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry.data;
    }

    /**
     * dump part of this table
     *
     * @param l list to update
     * @param b beginning to use
     */
    public void dump(userFormat l, String b) {
        for (int i = 0; i < conns.size(); i++) {
            l.add(b + "|" + conns.get(i).dump());
        }
    }

    /**
     * count clients
     *
     * @param ifc interface
     * @param prt local port
     * @param adr remote address
     * @return number of clients
     */
    public int countClients(tabRouteIface ifc, int prt, Ta adr) {
        int res = 0;
        for (int i = 0; i < conns.size(); i++) {
            tabConnectEntry<Ta, Td> ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.iface != ifc) {
                continue;
            }
            if (ntry.local != prt) {
                continue;
            }
            if (adr == null) {
                res++;
                continue;
            }
            if (adr.compare(adr, ntry.peer) != 0) {
                continue;
            }
            res++;
        }
        return res;
    }

    /**
     * count clients
     *
     * @param ifc interface
     * @param prt local port
     * @param prf remote prefix
     * @return number of clients
     */
    public int countSubnet(tabRouteIface ifc, int prt, addrPrefix<Ta> prf) {
        int res = 0;
        for (int i = 0; i < conns.size(); i++) {
            tabConnectEntry<Ta, Td> ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.iface != ifc) {
                continue;
            }
            if (ntry.local != prt) {
                continue;
            }
            if (prf.matches(ntry.peer)) {
                res++;
            }
        }
        return res;
    }

}
