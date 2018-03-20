package ifc;

import addr.addrMac;
import addr.addrType;
import cfg.cfgIfc;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * ling aggregation control protocol (ieee 802.3ad) handler
 *
 * @author matecsaba
 */
public class ifcLacp implements ifcUp, Runnable {

    /**
     * ethertype
     */
    public final static int ethtyp = 0x8809;

    /**
     * system priority
     */
    public int sysPri = 0x8000;

    /**
     * system mac
     */
    public addrMac sysMac = new addrMac();

    /**
     * system key
     */
    public int sysKey = 1;

    /**
     * port priority
     */
    public int portPri = 0x8000;

    /**
     * port number
     */
    public int portNum = 1;

    private byte[] peer = new byte[18];

    private int heard;

    private cfgIfc cfg;

    private ifcDn lower = new ifcNull();

    private addrType hwadr;

    private counter cntr = new counter();

    private boolean need2run = true;

    /**
     * create new instance
     *
     * @param ifc interface to use
     */
    public ifcLacp(cfgIfc ifc) {
        cfg = ifc;
        new Thread(this).start();
    }

    /**
     * stop working
     */
    public void stopWork() {
        need2run = false;
    }

    public String toString() {
        return "lacp on " + lower;
    }

    private typLenVal getTlv() {
        return new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        if (pck.msbGetW(2) != 0x0101) {
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        pck.getSkip(4);
        if (debugger.ifcLacpEvnt) {
            logger.debug("received packet");
        }
        typLenVal tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp == 0) {
                break;
            }
            if (tlv.valTyp == 1) {
                bits.byteCopy(tlv.valDat, 0, peer, 0, peer.length);
            }
        }
        heard = 0;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        hwadr = lower.getHwAddr();
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    public void run() {
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(1000);
            heard++;
            if (heard > 60) {
                peer = new byte[18];
            }
            if (debugger.ifcLacpEvnt) {
                logger.debug("sending packet");
            }
            packHolder pck = new packHolder(true, true);
            typLenVal tlv = getTlv();
            pck.ETHtrg.fromString("0180:c200:0002");
            if (hwadr.getSize() == addrMac.size) {
                pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
            }
            pck.msbPutW(0, ethtyp); // ethertype
            pck.msbPutW(2, 0x0101); // lacp v1
            pck.putSkip(4);
            tlv.putBytes(pck, 1, getInfo());
            tlv.putBytes(pck, 2, peer);
            byte[] buf = new byte[14];
            bits.msbPutW(buf, 0, 0x8000);
            tlv.putBytes(pck, 3, buf);
            pck.merge2beg();
            pck.putFill(0, 0x34, 0);
            pck.putSkip(0x34);
            pck.merge2end();
            lower.sendPack(pck);
        }
    }

    private byte[] getInfo() {
        byte[] buf = new byte[18];
        bits.msbPutW(buf, 0, sysPri);
        sysMac.toBuffer(buf, 2);
        bits.msbPutW(buf, 8, sysKey);
        bits.msbPutW(buf, 10, portPri);
        bits.msbPutW(buf, 12, portNum);
        buf[14] = 0x3d; // state
        return buf;
    }

    private static void dumpInfo(List<String> l, byte[] buf) {
        l.add("system priority=" + bits.msbGetW(buf, 0));
        addrMac adr = new addrMac();
        adr.fromBuf(buf, 2);
        l.add("system address=" + adr);
        l.add("system key=" + bits.msbGetW(buf, 8));
        l.add("port priority=" + bits.msbGetW(buf, 10));
        l.add("port number=" + bits.msbGetW(buf, 12));
        l.add("port state=" + buf[14]);
    }

    /**
     * get show output
     *
     * @param detailed detailed listing
     * @return list of neighbors
     */
    public List<String> getShNeigh(boolean detailed) {
        List<String> l = new ArrayList<String>();
        if (!detailed) {
            addrMac adr = new addrMac();
            adr.fromBuf(peer, 2);
            l.add(cfg.name + "|" + bits.msbGetW(peer, 0) + "." + adr + "." + bits.msbGetW(peer, 8) + "|" + bits.msbGetW(peer, 10) + "." + bits.msbGetW(peer, 12) + "|" + peer[14]);
        } else {
            l.add("local:");
            dumpInfo(l, getInfo());
            l.add("remote:");
            dumpInfo(l, peer);
        }
        return l;
    }

    /**
     * get config
     *
     * @param c holder
     * @return text
     */
    public static String getCfg(ifcLacp c) {
        if (c == null) {
            return "";
        } else {
            return c.sysMac + " " + c.sysKey + " " + c.portNum;
        }
    }

}
