package serv;

import addr.addrIP;
import java.util.List;
import pack.packHolder;
import pack.packNtp;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.state;

/**
 * mpls oam (rfc4379) server
 *
 * @author matecsaba
 */
public class servMplsOam extends servGeneric implements prtServP {

    /**
     * port number
     */
    public static final int portNum = 3503;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server mplsoam .*! port " + portNum,
        "server mplsoam .*! protocol " + proto2string(protoAllDgrm),};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
    }

    public boolean srvCfgStr(cmds cmd) {
        return true;
    }

    public void srvHelp(userHelping l) {
    }

    public String srvName() {
        return "mplsoam";
    }

    public int srvPort() {
        return portNum;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = 10000;
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (pck.msbGetW(0) != 1) { // version
            return true;
        }
        if (pck.getByte(4) != 1) { // type
            return true;
        }
        switch (pck.getByte(5)) { // mode
            case 1: // silent
                return true;
            case 2: // udp
                break;
            case 3: // udp with alert
                break;
            case 4: // application
                return true;
            default:
                return true;
        }
        long hndl = pck.msbGetQ(8);
        long tim = pck.msbGetQ(16);
        pck.clear();
        pck.msbPutW(0, 1); // version
        pck.msbPutW(2, 0); // flags
        pck.msbPutW(4, 0x202); // type, mode
        pck.msbPutW(6, 0); // return code
        pck.msbPutQ(8, hndl);
        pck.msbPutQ(16, tim);
        pck.msbPutQ(24, packNtp.encode(bits.getTime()));
        pck.putSkip(32);
        pck.merge2beg();
        id.send2net(pck);
        id.setClosing();
        return false;
    }

}
