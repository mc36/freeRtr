package org.freertr.ifc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * unidirectional link detection (rfc5171) protocol
 *
 * @author matecsaba
 */
public class ifcUdld implements ifcUp {

    /**
     * snap org id
     */
    public final static int orgid = 0x00000c;

    /**
     * ethertype
     */
    public final static int ethtyp = 0x0111;

    /**
     * time between advertisements
     */
    public int advertiseInterval = 5000;

    /**
     * list of neighbors
     */
    public tabGen<ifcUdldNeigh> neighs = new tabGen<ifcUdldNeigh>();

    /**
     * keepalive
     */
    protected ifcUdldTxAdv keepTimer;

    private cfgIfc cfg;

    private ifcDn lower = new ifcNull();

    private addrType hwadr = new addrEmpty();

    private counter cntr = new counter();

    private int txSeq;

    /**
     * probe
     */
    public final static int opcProbe = 1;

    /**
     * echo
     */
    public final static int opcEcho = 2;

    /**
     * flush
     */
    public final static int opcFlush = 3;

    /**
     * using default timeout
     */
    public final static int flgDflt = 1;

    /**
     * resynchronization
     */
    public final static int flgSync = 2;

    /**
     * device id
     */
    public final static int ttypDevId = 1;

    /**
     * port id
     */
    public final static int ttypPrtId = 2;

    /**
     * echo
     */
    public final static int ttypEcho = 3;

    /**
     * message interval
     */
    public final static int ttypMsgInt = 4;

    /**
     * timeout interval
     */
    public final static int ttypTimOut = 5;

    /**
     * device name
     */
    public final static int ttypDevNam = 6;

    /**
     * sequence number
     */
    public final static int ttypSeqNum = 7;

    /**
     * decode opcode
     *
     * @param i opcode
     * @return decoded string
     */
    public static String opcode2string(int i) {
        switch (i) {
            case opcProbe:
                return "probe";
            case opcEcho:
                return "echo";
            case opcFlush:
                return "flush";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * create new instance
     *
     * @param ifc interface to use
     */
    public ifcUdld(cfgIfc ifc) {
        cfg = ifc;
        restartTimer(false);
    }

    public String toString() {
        return "udld on " + lower;
    }

    private encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 4, 4, 1, 0, 512, true);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        int ver = pck.getByte(0);
        if ((ver & 0xe0) != 0x20) {
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        // int flag = pck.getByte(1);
        ifcUdldNeigh nei = new ifcUdldNeigh();
        nei.peer = pck.ETHsrc.copyBytes();
        nei.serNum = "";
        nei.portId = "";
        pck.getSkip(4);
        encTlv tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case ttypDevId:
                    nei.serNum = tlv.getStr();
                    break;
                case ttypPrtId:
                    nei.portId = tlv.getStr();
                    break;
                case ttypEcho:
                    nei.bidir = findMyself(tlv);
                    break;
                case ttypMsgInt:
                    nei.msgInt = (tlv.valDat[0] & 0xff) * 1000;
                    break;
                case ttypTimOut:
                    nei.timOut = (tlv.valDat[0] & 0xff) * 1000;
                    break;
                case ttypDevNam:
                    nei.hostName = tlv.getStr();
                    break;
                case ttypSeqNum:
                    // rxSeq = bits.msbGetD(tlv.valDat, 0);
                    break;
            }
        }
        nei.created = bits.getTime();
        if (neighs.put(nei) == null) {
            logger.warn("neighbor " + nei.hostName + " up");
        }
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwadr = lower.getHwAddr();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (advertiseInterval < 1) {
            return;
        }
        keepTimer = new ifcUdldTxAdv(this);
        keepTimer.start();
    }

    private boolean findMyself(encTlv tlv) {
        int m = bits.msbGetD(tlv.valDat, 0);
        int p = 4;
        for (int i = 0; i < m; i++) {
            if (p >= tlv.valSiz) {
                break;
            }
            int o = bits.msbGetW(tlv.valDat, p);
            p += 2;
            String a = new String(tlv.valDat, p, o);
            p += o;
            o = bits.msbGetW(tlv.valDat, p);
            p += 2;
            String b = new String(tlv.valDat, p, o);
            p += o;
            if (!a.equals(cfgInit.hwIdNum)) {
                continue;
            }
            if (!b.equals(cfg.name)) {
                continue;
            }
            return true;
        }
        return false;
    }

    private static byte[] string2seen(String s) {
        byte[] b1 = s.getBytes();
        byte[] b2 = new byte[2];
        bits.msbPutW(b2, 0, b1.length);
        return bits.byteConcat(b2, b1);
    }

    private static byte[] neigh2seen(ifcUdldNeigh ntry) {
        return bits.byteConcat(string2seen(ntry.serNum), string2seen(ntry.portId));
    }

    private byte[] getSeenList() {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, neighs.size());
        for (int i = 0; i < neighs.size(); i++) {
            buf = bits.byteConcat(buf, neigh2seen(neighs.get(i)));
        }
        return buf;
    }

    /**
     * send advertisement
     */
    protected void sendAdvert() {
        boolean need2stop = false;
        long tim = bits.getTime();
        for (int i = neighs.size(); i >= 0; i--) {
            ifcUdldNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if ((nei.created + (nei.msgInt * 3)) > tim) {
                continue;
            }
            logger.error("neighbor " + nei.hostName + " down");
            neighs.del(nei);
            need2stop |= nei.bidir;
        }
        if (need2stop) {
            cfg.flapNow(1000);
            return;
        }
        txSeq++;
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        pck.ETHtrg.fromString("0100:0ccc:cccc");
        if (hwadr.getSize() == addrMac.size) {
            pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
        }
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putSkip(2);
        pck.merge2end();
        pck.putByte(0, 0x21); // version
        pck.putByte(1, 0); // flags
        pck.msbPutW(2, 0); // checksum
        pck.putSkip(4);
        tlv.putStr(pck, ttypDevId, cfgInit.hwIdNum);
        tlv.putStr(pck, ttypPrtId, cfg.name);
        tlv.putBytes(pck, ttypEcho, getSeenList());
        byte[] buf = new byte[1];
        buf[0] = (byte) (advertiseInterval / 1000);
        tlv.putBytes(pck, ttypMsgInt, buf);
        buf[0] = (byte) ((advertiseInterval * 3) / 1000);
        tlv.putBytes(pck, ttypTimOut, buf);
        tlv.putStr(pck, ttypDevNam, cfgAll.hostName);
        buf = new byte[4];
        bits.msbPutD(buf, 0, txSeq);
        if ((pck.headSize() & 1) != 0) {
            tlv.valDat[0] = 0;
            tlv.putBytes(pck, 0xffff, 1, tlv.valDat);
        }
        int i = pck.headSize();
        pck.lsbPutW(2 - i, 0xffff - pck.putIPsum(-i, i, 0));
        pck.merge2end();
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    /**
     * get show output
     *
     * @param detailed detailed listing
     * @return list of neighbors
     */
    public List<String> getShNeigh(boolean detailed) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            ifcUdldNeigh nei = neighs.get(i);
            if (!detailed) {
                l.add(cfg.name + "|" + nei);
            } else {
                nei.dump(l);
            }
        }
        return l;
    }

}

class ifcUdldTxAdv implements Runnable {

    private ifcUdld lower;

    public ifcUdldTxAdv(ifcUdld parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendAdvert();
                bits.sleep(lower.advertiseInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcUdldNeigh implements Comparable<ifcUdldNeigh> {

    public addrMac peer;

    public String hostName;

    public String portId;

    public String serNum;

    public int msgInt;

    public int timOut;

    public boolean bidir;

    public long created;

    public String toString() {
        return hostName + "|" + serNum + "|" + portId + "|" + (bidir ? "bidir" : "unidir");
    }

    public void dump(List<String> l) {
        l.add("");
        l.add("peer|" + peer);
        l.add("hostname|" + hostName);
        l.add("port id|" + portId);
        l.add("serial|" + serNum);
        l.add("message interval|" + msgInt);
        l.add("timeout value|" + timOut);
        l.add("bidir|" + bidir);
    }

    public int compareTo(ifcUdldNeigh o) {
        return hostName.compareTo(o.hostName);
    }

}
