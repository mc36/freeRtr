package ifc;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrType;
import cfg.cfgIfc;
import ip.ipIfc4;
import ip.ipIfc6;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import rtr.rtrBgpUtil;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * next hop resolution protocol (rfc2332) handler
 *
 * @author matecsaba
 */
public class ifcNhrp implements ifcUp {

    /**
     * ethertype of these packets
     */
    public final static int ethtyp = 0x2001;

    /**
     * resolution request
     */
    public final static int typResReq = 1;

    /**
     * resolution reply
     */
    public final static int typResRep = 2;

    /**
     * registration request
     */
    public final static int typRegReq = 3;

    /**
     * registration reply
     */
    public final static int typRegRep = 4;

    /**
     * purge request
     */
    public final static int typPurReq = 5;

    /**
     * purge reply
     */
    public final static int typPurRep = 6;

    /**
     * error indication
     */
    public final static int typError = 7;

    /**
     * ipv4 address of server
     */
    public addrIPv4 ip4;

    /**
     * ipv6 address of server
     */
    public addrIPv6 ip6;

    /**
     * time between advertisements
     */
    public int advertiseInterval = 30;

    private cfgIfc cfg;

    private ifcDn lower = new ifcNull();

    private addrType hwadr;

    private counter cntr = new counter();

    private Timer keepTimer;

    /**
     * convert type to string
     *
     * @param typ type
     * @return string
     */
    public static String type2string(int typ) {
        switch (typ) {
            case typResReq:
                return "resReq";
            case typResRep:
                return "resRep";
            case typRegReq:
                return "regReq";
            case typRegRep:
                return "regRep";
            case typPurReq:
                return "purReq";
            case typPurRep:
                return "purRep";
            case typError:
                return "error";
            default:
                return "unknown=" + typ;
        }
    }

    /**
     * create new instance
     *
     * @param ifc interface to use
     */
    public ifcNhrp(cfgIfc ifc) {
        cfg = ifc;
        restartTimer(false);
    }

    public String toString() {
        return "nhrp on " + lower;
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
        int i = pck.msbGetW(10); // packet length
        if (i < 20) {
            cntr.drop(pck, counter.reasons.badSiz);
            return;
        }
        if (pck.dataSize() < i) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        pck.setDataSize(i);
        if (pck.getIPsum(0, i, 0) != 0xffff) {
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        if (pck.getByte(16) != 1) { // version
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        int typ = pck.getByte(17); // type
        addrType tun;
        switch (pck.getByte(18)) { // nbma size
            case addrIPv4.size:
                tun = new addrIPv4();
                break;
            case addrIPv6.size:
                tun = new addrIPv6();
                break;
            default:
                cntr.drop(pck, counter.reasons.badVal);
                return;
        }
        pck.getSkip(20); // fixed header
        addrType src;
        addrType trg;
        switch (pck.getByte(0)) {
            case addrIPv4.size:
                src = new addrIPv4();
                trg = new addrIPv4();
                break;
            case addrIPv6.size:
                src = new addrIPv6();
                trg = new addrIPv6();
                break;
            default:
                cntr.drop(pck, counter.reasons.badVal);
                return;
        }
        int flg = pck.msbGetW(2); // flags
        int id = pck.msbGetD(4); // id
        pck.getSkip(8); // common header
        pck.getAddr(tun, 0); // source nbma addr
        pck.getSkip(tun.getSize());
        pck.getAddr(src, 0); // source proto addr
        pck.getSkip(src.getSize());
        pck.getAddr(trg, 0); // target proto addr
        pck.getSkip(trg.getSize());
        int cod = pck.getByte(0);
        int tim = pck.msbGetW(6);
        if (debugger.ifcNhrpEvnt) {
            logger.debug("rx typ=" + type2string(typ) + " cod=" + cod + " flg=" + flg + " id=" + id + " tim=" + tim + " tun=" + tun + " src=" + src + " trg=" + trg);
        }
        pck = null;
        switch (typ) {
            case typRegReq:
                pck = createPack(typRegRep, 0, id, 0, tim, tun, src, trg);
                break;
            case typPurReq:
                pck = createPack(typPurRep, 0, id, 0, tim, tun, src, trg);
                break;
            default:
                break;
        }
        if (pck == null) {
            return;
        }
        lower.sendPack(pck);
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
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (advertiseInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ifcNhrpTxAdv task = new ifcNhrpTxAdv(this);
        keepTimer.schedule(task, 500, advertiseInterval * 1000);
    }

    /**
     * send advertisement
     */
    protected void sendAdvert() {
        addrIP trg = cfg.tunTrg;
        if (trg == null) {
            return;
        }
        if (cfg.tunSrc == null) {
            return;
        }
        addrIP src = cfg.tunSrc.getLocAddr(trg);
        if (src == null) {
            return;
        }
        addrType asrc;
        if (src.isIPv4()) { // nbma
            asrc = src.toIPv4();
        } else {
            asrc = src.toIPv6();
        }
        packHolder pck = createPack(typRegReq, 0, bits.randomD(), 0x8000, advertiseInterval * 5, asrc, cfg.addr4, ip4);
        if (pck != null) {
            lower.sendPack(pck);
        }
        pck = createPack(typRegReq, 0, bits.randomD(), 0x8000, advertiseInterval * 5, asrc, cfg.addr6, ip6);
        if (pck != null) {
            lower.sendPack(pck);
        }
    }

    private packHolder createPack(int typ, int cod, int id, int flg, int tim, addrType nsrc, addrType tsrc, addrType ttrg) {
        if ((tsrc == null) || (ttrg == null)) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        pck.ETHtrg.fillBytes(0xff);
        if (hwadr.getSize() == addrMac.size) {
            pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
        }
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putSkip(2);
        pck.msbPutW(0, ((nsrc.getSize() == addrIPv4.size) ? rtrBgpUtil.afiIpv4 : rtrBgpUtil.afiIpv6) >>> 16);
        pck.msbPutW(2, (tsrc.getSize() == addrIPv4.size) ? ipIfc4.type : ipIfc6.type);
        pck.putFill(4, 5, 0); // snap
        pck.putByte(9, 255); // hop count
        pck.msbPutW(10, 0); // packet size
        pck.msbPutW(12, 0); // checksum
        pck.msbPutW(14, 0); // extension offset
        pck.putByte(16, 1); // version
        pck.putByte(17, typ); // type
        pck.putByte(18, nsrc.getSize()); // source address len
        pck.putByte(19, 0); // source subaddress len
        pck.putSkip(20); // fixed part
        pck.putByte(0, tsrc.getSize()); // source proto len
        pck.putByte(1, tsrc.getSize()); // target proto len
        pck.msbPutW(2, flg); // flags
        pck.msbPutD(4, id); // request id
        pck.putSkip(8);
        pck.putAddr(0, nsrc); // nbma
        pck.putSkip(nsrc.getSize());
        pck.putAddr(0, tsrc); // protocol source
        pck.putSkip(tsrc.getSize());
        pck.putAddr(0, ttrg); // protocol target
        pck.putSkip(ttrg.getSize());
        pck.putByte(0, cod); // code
        pck.putByte(1, tsrc.maxBits()); // prefix length
        pck.msbPutW(2, 0); // unused
        pck.msbPutW(4, cfg.ethtyp.getMTUsize()); // mtu
        pck.msbPutW(6, tim);// hold time
        pck.putByte(8, nsrc.getSize()); // source address len
        pck.putByte(9, 0); // source subaddress len
        pck.putByte(10, tsrc.getSize()); // source proto len
        pck.putByte(11, 0); // cie preference
        pck.putSkip(12);
        pck.putAddr(0, nsrc); // nbma
        pck.putSkip(nsrc.getSize());
        pck.putAddr(0, tsrc); // protocol source
        pck.putSkip(tsrc.getSize());
        pck.msbPutD(0, 0x80030000); // responder address
        pck.msbPutD(4, 0x80040000); // forward transit nhs record
        pck.msbPutD(8, 0x80050000); // reverse transit nhs record
        pck.msbPutD(12, 0); // end of list
        pck.putSkip(16);
        int i = pck.headSize() - 2;
        pck.putSkip(-i);
        pck.msbPutW(10, i); // packet size
        pck.msbPutW(14, i - 16); // extension offset
        pck.lsbPutW(12, 0xffff - pck.putIPsum(0, i, 0)); // checksum
        pck.putSkip(i);
        pck.merge2end();
        if (debugger.ifcNhrpEvnt) {
            logger.debug("tx typ=" + type2string(typ) + " cod=" + cod + " flg=" + flg + " id=" + id + " tim=" + tim + " tun=" + nsrc + " src=" + tsrc + " trg=" + ttrg);
        }
        return pck;
    }

}

class ifcNhrpTxAdv extends TimerTask {

    private ifcNhrp lower;

    public ifcNhrpTxAdv(ifcNhrp parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendAdvert();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
