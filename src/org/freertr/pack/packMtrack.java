package org.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;

/**
 * our proprietary mtracker packet
 *
 * @author matecsaba
 */
public class packMtrack {

    /**
     * create instance
     */
    public packMtrack() {
    }

    /**
     * port number
     */
    public final static int port = 5499;

    /**
     * config request
     */
    public final static int typCfgReq = 1;

    /**
     * config reply
     */
    public final static int typCfgRep = 2;

    /**
     * config reply
     */
    public final static int typCfgEnd = 3;

    /**
     * report
     */
    public final static int typReport = 4;

    /**
     * last report
     */
    public final static int typLreport = 5;

    /**
     * maximum addresses
     */
    public final static int maxAddrs = 1024 / addrIP.size;

    /**
     * packet type
     */
    public int typ;

    /**
     * addresses
     */
    public List<addrIP> adrs = new ArrayList<addrIP>();

    /**
     * parse one packet
     *
     * @param pck packet to update
     */
    public void parsePacket(packHolder pck) {
        typ = pck.getByte(0); // type
        pck.getSkip(1);
        adrs.clear();
        for (;;) {
            if (pck.dataSize() < addrIP.size) {
                return;
            }
            addrIP a = new addrIP();
            pck.getAddr(a, 0); // address
            pck.getSkip(addrIP.size);
            adrs.add(a);
        }
    }

    /**
     * create one packet
     *
     * @param pck packet to update
     */
    public void createPacket(packHolder pck) {
        pck.clear();
        pck.putByte(0, typ); // type
        pck.putSkip(1);
        for (int i = 0; i < adrs.size(); i++) {
            pck.putAddr(0, adrs.get(i)); // address
            pck.putSkip(addrIP.size);
        }
        pck.merge2beg();
    }

    /**
     * update config address
     *
     * @param grp group
     * @param cfg config
     */
    public static void updateCfg(addrIP grp, addrIP cfg) {
        byte[] buf = cfg.getBytes();
        buf[12] = 0;
        buf[13] = 0;
        buf[14] = 0;
        buf[15] = 0;
        int i = cfg.getHashB() ^ grp.getHashB();
        buf[12] = (byte) i;
        buf[13] = (byte) (0xff - i);
    }

    /**
     * validate config address
     *
     * @param grp group
     * @param cfg config
     * @return false on success, true on error
     */
    public static boolean validateCfg(addrIP grp, addrIP cfg) {
        if ((grp == null) || (cfg == null)) {
            return true;
        }
        byte[] buf = cfg.getBytes();
        if ((buf[14] != 0) || (buf[15] != 0)) {
            return true;
        }
        int i = buf[12] & 0xff;
        if ((buf[13] & 0xff) != (0xff - i)) {
            return true;
        }
        buf[12] = 0;
        buf[13] = 0;
        return i != (cfg.getHashB() ^ grp.getHashB());
    }

}
