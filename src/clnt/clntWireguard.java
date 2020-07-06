package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import cry.cryBase64;
import cry.cryEnchChacha20poly1305;
import cry.cryHashBlake2s;
import cry.cryHashHmac;
import cry.cryECcurve25519;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * wireguard protocol
 *
 * @author matecsaba
 */
public class clntWireguard implements Runnable, prtServP, ifcDn {

    /**
     * port number
     */
    public static final int port = 51902;

    private static final byte[] magicC = {
        (byte) 0x60, (byte) 0xe2, (byte) 0x6d, (byte) 0xae, (byte) 0xf3, (byte) 0x27, (byte) 0xef, (byte) 0xc0,
        (byte) 0x2e, (byte) 0xc3, (byte) 0x35, (byte) 0xe2, (byte) 0xa0, (byte) 0x25, (byte) 0xd2, (byte) 0xd0,
        (byte) 0x16, (byte) 0xeb, (byte) 0x42, (byte) 0x06, (byte) 0xf8, (byte) 0x72, (byte) 0x77, (byte) 0xf5,
        (byte) 0x2d, (byte) 0x38, (byte) 0xd1, (byte) 0x98, (byte) 0x8b, (byte) 0x78, (byte) 0xcd, (byte) 0x36};

    private static final byte[] magicH = {
        (byte) 0x22, (byte) 0x11, (byte) 0xb3, (byte) 0x61, (byte) 0x08, (byte) 0x1a, (byte) 0xc5, (byte) 0x66,
        (byte) 0x69, (byte) 0x12, (byte) 0x43, (byte) 0xdb, (byte) 0x45, (byte) 0x8a, (byte) 0xd5, (byte) 0x32,
        (byte) 0x2d, (byte) 0x9c, (byte) 0x6c, (byte) 0x66, (byte) 0x22, (byte) 0x93, (byte) 0xe8, (byte) 0xb7,
        (byte) 0x0e, (byte) 0xe1, (byte) 0x9c, (byte) 0x65, (byte) 0xba, (byte) 0x07, (byte) 0x9e, (byte) 0xf3};

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * source port
     */
    public int prtL;

    /**
     * target port
     */
    public int prtR;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * preshared secret
     */
    public String preshared;

    /**
     * counter
     */
    public counter cntr = new counter();

    private prtGenConn conn;

    private boolean working = true;

    private long seqRx;

    private long seqTx;

    private byte[] locPriv;

    private byte[] locPub;

    private byte[] remPub;

    private long lasTim;

    private int idxRx;

    private int idxTx;

    private byte[] keyRx;

    private byte[] keyTx;

    private byte[] dh1;

    private byte[] dh2;

    private byte[] dhr;

    private byte[] hi;

    private byte[] hr;

    private byte[] ci;

    private byte[] cr;

    public String toString() {
        return "wireguard to " + target;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
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
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1400;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        int i = preshared.indexOf("=") + 1;
        locPriv = cryBase64.decodeBytes(preshared.substring(0, i));
        remPub = cryBase64.decodeBytes(preshared.substring(i, preshared.length()));
        locPub = cryECcurve25519.calc(locPriv, null);
        addrIP trg = userTerminal.justResolv(target, 0);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        if (prtR == 0) {
            prtR = port;
        }
        if (prtL == 0) {
            prtL = prtR;
        }
        conn = udp.packetConnect(this, fwdIfc, prtL, trg, prtR, "wireguard", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendTTL = sendingTTL;
        for (;;) {
            if (conn == null) {
                return;
            }
            if (conn.txBytesFree() < 0) {
                return;
            }
            if (keyRx == null) {
                sendInit();
            } else {
                sendKeep();
            }
            bits.sleep(5000);
        }
    }

    private synchronized void clearState() {
        keyRx = null;
        keyTx = null;
        if (conn != null) {
            conn.setClosing();
        }
        conn = null;
        seqRx = 0;
        seqTx = 0;
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        return true;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * connection closed
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
     * send packet
     *
     * @param pck packet
     */
    public synchronized void sendPack(packHolder pck) {
        pck.merge2beg();
        if (conn == null) {
            return;
        }
        if (keyTx == null) {
            return;
        }
        cntr.tx(pck);
        if (ifcEther.stripEtherType(pck)) {
            return;
        }
        int i = pck.dataSize() % 16;
        i = 16 - i;
        pck.putFill(0, i, 0); // padding
        pck.putSkip(i);
        pck.merge2end();
        byte[] tmp = new byte[12];
        bits.lsbPutQ(tmp, 4, seqTx);
        cryEnchChacha20poly1305 en = new cryEnchChacha20poly1305();
        en.init(keyTx, tmp, true);
        i = pck.enchData(en, 0, pck.dataSize());
        if (i < 0) {
            return;
        }
        pck.setDataSize(i);
        pck.lsbPutD(0, 4); // data
        pck.msbPutD(4, idxRx);
        pck.lsbPutQ(8, seqTx);
        pck.putSkip(16);
        pck.merge2beg();
        conn.send2net(pck);
        seqTx++;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public synchronized boolean datagramRecv(prtGenConn id, packHolder pck) {
        int typ = pck.lsbGetD(0);
        switch (typ) {
            case 4: // data
                if (pck.dataSize() < 32) {
                    logger.info("get truncated data from " + target);
                    return false;
                }
                if (keyRx == null) {
                    logger.info("got unwanted data from " + target);
                    return false;
                }
                if (pck.msbGetD(4) != idxTx) {
                    logger.info("got invalid index from " + target);
                    return false;
                }
                byte[] tmp1 = new byte[12];
                pck.getCopy(tmp1, 4, 8, 8);
                pck.getSkip(16);
                seqRx = bits.lsbGetQ(tmp1, 0);
                cryEnchChacha20poly1305 en = new cryEnchChacha20poly1305();
                en.init(keyRx, tmp1, false);
                typ = pck.enchData(en, 0, pck.dataSize());
                if (typ < 0) {
                    logger.info("got invalid data from " + target);
                    return false;
                }
                pck.setDataSize(typ);
                if (typ < 1) {
                    if (debugger.clntWireguardTraf) {
                        logger.debug("rx keepalive");
                    }
                    return false;
                }
                typ = ifcEther.guessEtherType(pck);
                if (typ < 0) {
                    logger.info("got bad protocol from " + target);
                    cntr.drop(pck, counter.reasons.badProto);
                    return false;
                }
                pck.msbPutW(0, typ); // ethertype
                pck.putSkip(2);
                pck.merge2beg();
                cntr.rx(pck);
                upper.recvPack(pck);
                return false;
            case 1: // init
                if (pck.dataSize() < 148) {
                    logger.info("get truncated init from " + target);
                    return false;
                }
                tmp1 = new byte[32];
                pck.getCopy(tmp1, 0, 116, tmp1.length);
                pck.setDataSize(116);
                if (bits.byteComp(tmp1, 0, calcMac1(locPub, pck), 0, tmp1.length) != 0) {
                    logger.info("got invalid mac from " + target);
                    return false;
                }
                int ridx = pck.msbGetD(4);
                pck.getSkip(8);
                dhr = new byte[32];
                pck.getCopy(dhr, 0, 0, dhr.length);
                pck.getSkip(dhr.length);
                if (debugger.clntWireguardTraf) {
                    logger.debug("rx init e=" + bits.byteDump(dhr, 0, -1));
                }
                cryHashBlake2s h = new cryHashBlake2s(null, 32);
                h.init();
                h.update(magicH);
                h.update(locPub);
                hi = h.finish();
                byte[][] ks = calcKdf(magicC, dhr, 1);
                ci = ks[0];
                h.init();
                h.update(hi);
                h.update(dhr);
                hi = h.finish();
                ks = calcKdf(ci, cryECcurve25519.calc(locPriv, dhr), 2);
                ci = ks[0];
                tmp1 = new byte[48];
                pck.getCopy(tmp1, 0, 0, tmp1.length);
                pck.getSkip(tmp1.length);
                byte[] tmp2 = decAead(ks[1], tmp1, hi);
                if (tmp2 == null) {
                    logger.info("got bad static from " + target);
                    return false;
                }
                if (bits.byteComp(tmp2, 0, remPub, 0, remPub.length) != 0) {
                    logger.info("got bad static from " + target);
                    return false;
                }
                h.init();
                h.update(hi);
                h.update(tmp1);
                hi = h.finish();
                ks = calcKdf(ci, cryECcurve25519.calc(locPriv, remPub), 2);
                ci = ks[0];
                tmp1 = new byte[28];
                pck.getCopy(tmp1, 0, 0, tmp1.length);
                pck.getSkip(tmp1.length);
                tmp2 = decAead(ks[1], tmp1, hi);
                if (tmp2 == null) {
                    logger.info("got bad timestamp from " + target);
                    return false;
                }
                h.init();
                h.update(hi);
                h.update(tmp1);
                hi = h.finish();
                if (bits.msbGetQ(tmp2, 0) <= lasTim) {
                    logger.info("got replayed handshake from " + target);
                    return false;
                }
                idxRx = ridx;
                initDH();
                if (debugger.clntWireguardTraf) {
                    logger.debug("tx resp e=" + bits.byteDump(dh2, 0, -1));
                }
                ks = calcKdf(ci, dh2, 1);
                cr = ks[0];
                h.init();
                h.update(hi);
                h.update(dh2);
                hr = h.finish();
                ks = calcKdf(cr, cryECcurve25519.calc(dh1, dhr), 1);
                cr = ks[0];
                ks = calcKdf(cr, cryECcurve25519.calc(dh1, remPub), 1);
                cr = ks[0];
                ks = calcKdf(cr, new byte[32], 3);
                cr = ks[0];
                h.init();
                h.update(hr);
                h.update(ks[1]);
                hr = h.finish();
                tmp1 = encAead(ks[2], new byte[0], hr);
                h.init();
                h.update(hr);
                h.update(tmp1);
                hr = h.finish();
                pck.clear();
                pck.lsbPutD(0, 2); // resp
                pck.msbPutD(4, idxTx);
                pck.msbPutD(8, idxRx);
                pck.putSkip(12);
                pck.putCopy(dh2, 0, 0, dh2.length);
                pck.putSkip(dh2.length);
                pck.putCopy(tmp1, 0, 0, tmp1.length);
                pck.putSkip(tmp1.length);
                pck.merge2beg();
                tmp1 = calcMac1(remPub, pck);
                pck.putCopy(tmp1, 0, 0, tmp1.length);
                pck.putSkip(tmp1.length);
                pck.merge2end();
                conn.send2net(pck);
                initKeys(1);
                return false;
            case 2: // resp
                if (pck.dataSize() < 92) {
                    logger.info("get truncated resp from " + target);
                    return false;
                }
                tmp1 = new byte[32];
                pck.getCopy(tmp1, 0, 60, tmp1.length);
                pck.setDataSize(60);
                if (bits.byteComp(tmp1, 0, calcMac1(locPub, pck), 0, tmp1.length) != 0) {
                    logger.info("got invalid mac from " + target);
                    return false;
                }
                ridx = pck.msbGetD(4);
                if (pck.msbGetD(8) != idxTx) {
                    logger.info("got invalid index from " + target);
                    return false;
                }
                if (ridx == idxRx) {
                    logger.info("got replayed resp from " + target);
                    return false;
                }
                pck.getSkip(12);
                dhr = new byte[32];
                pck.getCopy(dhr, 0, 0, dhr.length);
                pck.getSkip(dhr.length);
                if (debugger.clntWireguardTraf) {
                    logger.debug("rx resp e=" + bits.byteDump(dhr, 0, -1));
                }
                ks = calcKdf(ci, dhr, 1);
                cr = ks[0];
                h = new cryHashBlake2s(null, 32);
                h.init();
                h.update(hi);
                h.update(dhr);
                hr = h.finish();
                ks = calcKdf(cr, cryECcurve25519.calc(dh1, dhr), 1);
                cr = ks[0];
                ks = calcKdf(cr, cryECcurve25519.calc(locPriv, dhr), 1);
                cr = ks[0];
                ks = calcKdf(cr, new byte[32], 3);
                cr = ks[0];
                h.init();
                h.update(hr);
                h.update(ks[1]);
                hr = h.finish();
                tmp2 = pck.getCopy();
                tmp1 = decAead(ks[2], tmp2, hr);
                if (tmp1 == null) {
                    logger.info("got invalid empty from " + target);
                    return false;
                }
                if (tmp1.length != 0) {
                    logger.info("got malformed empty from " + target);
                    return false;
                }
                h.init();
                h.update(hr);
                h.update(tmp2);
                hr = h.finish();
                idxRx = ridx;
                initKeys(0);
                sendKeep();
                return false;
            default:
                logger.info("got invalid type from " + target);
                return false;
        }
    }

    private synchronized void sendKeep() {
        packHolder pck = new packHolder(true, true);
        byte[] tmp = new byte[12];
        bits.lsbPutQ(tmp, 4, seqTx);
        cryEnchChacha20poly1305 en = new cryEnchChacha20poly1305();
        en.init(keyTx, tmp, true);
        int i = pck.enchData(en, 0, pck.dataSize());
        if (i < 0) {
            return;
        }
        pck.setDataSize(i);
        pck.lsbPutD(0, 4); // data
        pck.msbPutD(4, idxRx);
        pck.lsbPutQ(8, seqTx);
        pck.putSkip(16);
        pck.merge2beg();
        conn.send2net(pck);
        seqTx++;
        if (debugger.clntWireguardTraf) {
            logger.debug("tx keepalive");
        }
    }

    private synchronized void sendInit() {
        if (dh1 == null) {
            initDH();
        }
        packHolder pck = new packHolder(true, true);
        pck.lsbPutD(0, 1); // init
        pck.msbPutD(4, idxTx);
        pck.putSkip(8);
        pck.putCopy(dh2, 0, 0, dh2.length);
        pck.putSkip(dh2.length);
        cryHashBlake2s h = new cryHashBlake2s(null, 32);
        h.init();
        h.update(magicH);
        h.update(remPub);
        hi = h.finish();
        byte[][] ks = calcKdf(magicC, dh2, 1);
        ci = ks[0];
        h.init();
        h.update(hi);
        h.update(dh2);
        hi = h.finish();
        ks = calcKdf(ci, cryECcurve25519.calc(dh1, remPub), 2);
        ci = ks[0];
        byte[] tmp = encAead(ks[1], locPub, hi);
        pck.putCopy(tmp, 0, 0, tmp.length);
        pck.putSkip(tmp.length);
        h.init();
        h.update(hi);
        h.update(tmp);
        hi = h.finish();
        ks = calcKdf(ci, cryECcurve25519.calc(locPriv, remPub), 2);
        ci = ks[0];
        tmp = new byte[12];
        long tim = bits.getTime() + cfgAll.timeServerOffset;
        bits.msbPutQ(tmp, 0, (tim / 1000) + 0x400000000000000aL);
        bits.msbPutD(tmp, 8, (int) (tim % 1000));
        tmp = encAead(ks[1], tmp, hi);
        h.init();
        h.update(hi);
        h.update(tmp);
        hi = h.finish();
        pck.putCopy(tmp, 0, 0, tmp.length);
        pck.putSkip(tmp.length);
        pck.merge2beg();
        tmp = calcMac1(remPub, pck);
        pck.putCopy(tmp, 0, 0, tmp.length);
        pck.putSkip(tmp.length);
        pck.merge2end();
        conn.send2net(pck);
        if (debugger.clntWireguardTraf) {
            logger.debug("tx init e=" + bits.byteDump(dh2, 0, -1));
        }
    }

    private void initDH() {
        dh1 = cryECcurve25519.make();
        dh2 = cryECcurve25519.calc(dh1, null);
        idxTx = bits.randomD();
    }

    private void initKeys(int r) {
        byte[][] ks = calcKdf(cr, new byte[0], 2);
        keyRx = ks[1 ^ r];
        keyTx = ks[r];
        seqRx = 0;
        seqTx = 0;
        if (debugger.clntWireguardTraf) {
            logger.debug("keys r=" + r + " ri=" + idxRx + " ti=" + idxTx + " rk=" + bits.byteDump(keyRx, 0, -1) + " tk=" + bits.byteDump(keyTx, 0, -1));
        }
    }

    private static byte[] calcMac1(byte[] pub, packHolder pck) {
        cryHashBlake2s h = new cryHashBlake2s(null, 32);
        h.init();
        h.update("mac1----".getBytes());
        h.update(pub);
        byte[] r = h.finish();
        h = new cryHashBlake2s(r, 16);
        pck.hashData(h, 0, pck.dataSize());
        return bits.byteConcat(h.finish(), new byte[16]);
    }

    private static byte[][] calcKdf(byte[] key, byte[] msg, int rnd) {
        byte[][] res = new byte[rnd][];
        cryHashHmac h = new cryHashHmac(new cryHashBlake2s(null, 32), key);
        h.init();
        h.update(msg);
        byte[] psk = h.finish();
        byte[] old = new byte[0];
        for (int i = 0; i < rnd; i++) {
            h = new cryHashHmac(new cryHashBlake2s(null, 32), psk);
            h.init();
            h.update(old);
            h.update(i + 1);
            res[i] = h.finish();
            old = res[i];
        }
        return res;
    }

    private static byte[] encAead(byte[] key, byte[] msg, byte[] auth) {
        cryEnchChacha20poly1305 c = new cryEnchChacha20poly1305();
        c.init(key, new byte[12], true);
        c.authAdd(auth);
        return c.compute(msg);
    }

    private static byte[] decAead(byte[] key, byte[] msg, byte[] auth) {
        cryEnchChacha20poly1305 c = new cryEnchChacha20poly1305();
        c.init(key, new byte[12], false);
        c.authAdd(auth);
        return c.compute(msg);
    }

}
