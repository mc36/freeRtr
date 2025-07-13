package org.freertr.ifc;

import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * qinqX protocol
 *
 * @author matecsaba
 */
public class ifcQinqX extends ifcVlan {

    /**
     * size of header
     */
    public final static int size = 6;

    /**
     * ethertype of these packets
     */
    public int type = 0xfa52;

    private ifcEthTyp lower;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            return true;
        }
        pck.ETHvlan = pck.msbGetD(2); // vlan tag
        pck.getSkip(size);
        if (debugger.ifcQinqXTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        if (debugger.ifcQinqXTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.msbPutW(0, type); // ether type
        pck.msbPutD(2, pck.ETHvlan); // vlan tag
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "qinqx on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "ethertype", "ethertype to use");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value in hex");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "ethertype " + bits.toHexW(type));
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("ethertype")) {
            type = bits.fromHex(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "qinqx", this);
        ethtyp.updateET(type, this);
        lower = ethtyp;
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delET(type);
        lower = ethtyp;
    }

    /**
     * create new multiplexer
     *
     * @param old old instance
     */
    public ifcQinqX(ifcQinqX old) {
        if (debugger.ifcQinqXTraf) {
            logger.debug("started");
        }
        if (old == null) {
            return;
        }
        type = old.type;
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public int remainingMtu() {
        return lower.getMTUsize() - size;
    }

}
