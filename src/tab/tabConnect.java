package tab;

import addr.addrPrefix;
import addr.addrType;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import user.userFormat;
import util.debugger;
import util.logger;

/**
 * keeps track of connections or listeners
 *
 * @param <Ta> address it works with
 * @param <Td> this type will be kept in records
 * @author matecsaba
 */
public class tabConnect<Ta extends addrType, Td extends tabConnectLower> {

    private final List<tabConnectEntry<Ta, Td>> conns;

    private final Ta empty;

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
        conns = new ArrayList<tabConnectEntry<Ta, Td>>();
        tabName = nam;
        empty = newAdr;
    }

    private addrPrefix<Ta> getPref(Ta adr, int msk) {
        if (adr == null) {
            return new addrPrefix<Ta>(empty, 0);
        }
        if (msk < 1) {
            msk = adr.maxBits();
        }
        return new addrPrefix<Ta>(adr, msk);
    }

    private int findChildPrt(tabConnectEntry<Ta, Td> ntry) {
        synchronized (conns) {
            for (int i = 0; i < conns.size(); i++) {
                if (ntry.greaterLIR(conns.get(i))) {
                    return i;
                }
            }
        }
        return -1;
    }

    private int findChildIfc(tabConnectEntry<Ta, Td> ntry) {
        synchronized (conns) {
            for (int i = 0; i < conns.size(); i++) {
                if (ntry.greaterILR(conns.get(i))) {
                    return i;
                }
            }
        }
        return -1;
    }

    private int findParent(tabConnectEntry<Ta, Td> ntry, boolean cntr) {
        synchronized (conns) {
            int i = Collections.binarySearch(conns, ntry, ntry);
            if (i >= 0) {
                return i;
            }
            if (!cntr) {
                return -1;
            }
            i = -i - 1;
            if (i < 0) {
                return -1;
            }
            if (i >= conns.size()) {
                return -1;
            }
            if (conns.get(i).greaterLIR(ntry)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * add one entry to this table
     *
     * @param locInt local interface, 0 means all
     * @param remAdr remote address, null means all
     * @param remMsk remote mask, 0 means host route
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @param data data to store
     * @param name name of connection
     * @param parent scan for parent if no exact match
     * @return false if successful, true if already found
     */
    public boolean add(int locInt, Ta remAdr, int remMsk, int locPrt, int remPrt, Td data, String name, boolean parent) {
        if (data == null) {
            return true;
        }
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = getPref(remAdr, remMsk);
        ntry.local = locPrt;
        ntry.remote = remPrt;
        ntry.name = name;
        ntry.data = data;
        if (debugger.tabConnectEvnt) {
            logger.debug("adding " + ntry.dump(null));
        }
        synchronized (conns) {
            if (findParent(ntry, parent) >= 0) {
                return true;
            }
            if (findChildPrt(ntry) >= 0) {
                return true;
            }
            conns.add(ntry);
            Collections.sort(conns, ntry);
        }
        return false;
    }

    /**
     * read one entry
     *
     * @param locInt local interface, 0 means all
     * @param remAdr remote address, null means all
     * @param remMsk remote mask, 0 means host route
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @param parent scan for parent if no exact match
     * @return stored value, null if not found
     */
    public Td get(int locInt, Ta remAdr, int remMsk, int locPrt, int remPrt, boolean parent) {
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = getPref(remAdr, remMsk);
        ntry.local = locPrt;
        ntry.remote = remPrt;
        synchronized (conns) {
            int i = findParent(ntry, parent);
            if (i < 0) {
                return null;
            }
            return conns.get(i).data;
        }
    }

    /**
     * read one entry
     *
     * @param i index
     * @return stored value, null if not found
     */
    public Td get(int i) {
        synchronized (conns) {
            if (i < 0) {
                return null;
            }
            if (i > conns.size()) {
                return null;
            }
            return conns.get(i).data;
        }
    }

    /**
     * get number of entries
     *
     * @return number of entries
     */
    public int size() {
        synchronized (conns) {
            return conns.size();
        }
    }

    /**
     * delete one entry
     *
     * @param locInt local interface, 0 means all
     * @param remAdr remote address, null means all
     * @param remMsk remote mask, 0 means host route
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @param parent scan for parent if no exact match
     * @return deleted value, null if not found
     */
    public Td del(int locInt, Ta remAdr, int remMsk, int locPrt, int remPrt, boolean parent) {
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = getPref(remAdr, remMsk);
        ntry.local = locPrt;
        ntry.remote = remPrt;
        if (debugger.tabConnectEvnt) {
            logger.debug("del " + ntry.dump(null));
        }
        synchronized (conns) {
            int i = findParent(ntry, parent);
            if (i < 0) {
                return null;
            }
            Td dat = conns.get(i).data;
            conns.remove(i);
            Collections.sort(conns, ntry);
            return dat;
        }
    }

    /**
     * delete one child of superb
     *
     * @param locInt local interface, 0 means all
     * @param remAdr remote address, null means all
     * @param remMsk remote mask, 0 means host route
     * @param locPrt local port, 0 means all
     * @param remPrt remote port, 0 means all
     * @return number of childs deleted
     */
    public Td delChild(int locInt, Ta remAdr, int remMsk, int locPrt, int remPrt) {
        tabConnectEntry<Ta, Td> ntry = new tabConnectEntry<Ta, Td>();
        ntry.iface = locInt;
        ntry.peer = getPref(remAdr, remMsk);
        ntry.local = locPrt;
        ntry.remote = remPrt;
        if (debugger.tabConnectEvnt) {
            logger.debug("delchild " + ntry.dump(null));
        }
        synchronized (conns) {
            int i = findParent(ntry, false);
            if (i < 0) {
                if (locPrt == 0) {
                    i = findChildIfc(ntry);
                } else {
                    i = findChildPrt(ntry);
                }
            }
            if (i < 0) {
                return null;
            }
            Td dat = conns.get(i).data;
            conns.remove(i);
            Collections.sort(conns, ntry);
            return dat;
        }
    }

    /**
     * dump part of this table
     *
     * @param f fowarder
     * @param l list to update
     * @param b beginning to use
     */
    public void dump(ipFwd f, userFormat l, String b) {
        synchronized (conns) {
            for (int i = 0; i < conns.size(); i++) {
                l.add(b + "|" + conns.get(i).dump(f));
            }
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
    public int countClients(int ifc, int prt, Ta adr) {
        int res = 0;
        synchronized (conns) {
            for (int i = 0; i < conns.size(); i++) {
                tabConnectEntry<Ta, Td> ntry = conns.get(i);
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
                if (adr.compare(adr, ntry.peer.network) != 0) {
                    continue;
                }
                res++;
            }
        }
        return res;
    }

}
