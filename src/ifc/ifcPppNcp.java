package ifc;

import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.typLenVal;

/**
 * one network control protocol for ppp
 *
 * @author matecsaba
 */
public abstract class ifcPppNcp {

    /**
     * bitmap value of sawn events
     */
    private int sawBit;

    /**
     * confreqs seen
     */
    private int sawReq1;

    /**
     * confreqs seen
     */
    private int sawReq2;

    /**
     * confreqs seen
     */
    protected int sawReq3;

    /**
     * confnaks seen
     */
    private int sawNakC;

    /**
     * confnaks size
     */
    private int sawNakS;

    /**
     * last id used
     */
    private int lastTxIdnt;

    /**
     * transmitted confreq
     */
    public Object txReq;

    /**
     * to keep track of parent
     */
    public ifcPpp parent;

    /**
     * counter for this ncp
     */
    public counter cntr = new counter();

    /**
     * saw tx confreq
     */
    public final static int sawTxReq = 0x1;

    /**
     * saw rx confreq
     */
    public final static int sawRxReq = 0x2;

    /**
     * saw tx confack
     */
    public final static int sawTxAck = 0x4;

    /**
     * saw rx confack
     */
    public final static int sawRxAck = 0x8;

    /**
     * saw loc->rem data
     */
    public final static int sawLocNeed = 0x10;

    /**
     * saw rem->loc data
     */
    public final static int sawRemNeed = 0x20;

    /**
     * forced to open
     */
    public final static int sawFrcOpen = 0x40;

    /**
     * forced to close
     */
    public final static int sawFrcClsd = 0x80;

    /**
     * Configure-Request
     */
    public final static int codeConfReq = 1;

    /**
     * Configure-Ack
     */
    public final static int codeConfAck = 2;

    /**
     * Configure-Nak
     */
    public final static int codeConfNak = 3;

    /**
     * Configure-Reject
     */
    public final static int codeConfRej = 4;

    /**
     * Terminate-Request
     */
    public final static int codeTermReq = 5;

    /**
     * Terminate-Ack
     */
    public final static int codeTermAck = 6;

    /**
     * Code-Reject
     */
    public final static int codeCodeRej = 7;

    /**
     * Protocol-Reject
     */
    public final static int codeProtRej = 8;

    /**
     * Echo-Request
     */
    public final static int codeEchoReq = 9;

    /**
     * Echo-Reply
     */
    public final static int codeEchoRep = 10;

    /**
     * Discard-Request
     */
    public final static int codeDiscReq = 11;

    /**
     * clear this state
     */
    public void clearUpperState() {
        sawBit &= (sawFrcOpen | sawFrcClsd);
        if ((sawBit & sawFrcOpen) != 0) {
            sawBit |= sawLocNeed;
        }
        sawReq1 = 0;
        sawReq2 = 0;
        sawReq3 = 0;
        sawNakC = 0;
        sawNakS = -1;
        txReq = getOneConfig(true);
    }

    /**
     * force to open state
     *
     * @param b state to force
     */
    public void forceOpen(boolean b) {
        if (b) {
            sawBit |= sawFrcOpen;
        } else {
            sawBit &= ~sawFrcOpen;
        }
    }

    /**
     * force to closed state
     *
     * @param b true to force closed, false to not
     */
    public void forceClose(boolean b) {
        if (b) {
            sawBit |= sawFrcClsd;
        } else {
            sawBit &= ~sawFrcClsd;
        }
    }

    /**
     * check if forced to open
     *
     * @return true if forced, false if not
     */
    public boolean forced2open() {
        return (sawBit & sawFrcOpen) != 0;
    }

    /**
     * check if forced to closed
     *
     * @return true if forced, false if not
     */
    public boolean forced2close() {
        return (sawBit & sawFrcClsd) != 0;
    }

    /**
     * get force value
     *
     * @return force
     */
    public int getForceVal() {
        return sawBit & (sawFrcClsd | sawFrcOpen);
    }

    /**
     * set local need bit
     */
    public void setLocalNeed() {
        sawBit |= sawLocNeed;
    }

    /**
     * convert code to string
     *
     * @param i code value
     * @return string
     */
    public static String code2str(int i) {
        switch (i) {
            case codeConfReq:
                return "confReq";
            case codeConfAck:
                return "confAck";
            case codeConfNak:
                return "confNak";
            case codeConfRej:
                return "confRej";
            case codeTermReq:
                return "termReq";
            case codeTermAck:
                return "termAck";
            case codeCodeRej:
                return "codeRej";
            case codeProtRej:
                return "protRej";
            case codeEchoReq:
                return "echoReq";
            case codeEchoRep:
                return "echoRep";
            case codeDiscReq:
                return "discReq";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * check if ready for transmission
     *
     * @return true if yes, false if not
     */
    public boolean getReady() {
        int need = sawTxReq | sawRxReq | sawTxAck | sawRxAck;
        return (sawBit & need) == need;
    }

    /**
     * check if needed
     *
     * @return true if yes, false if not
     */
    public boolean getNeeded() {
        int need = sawLocNeed | sawRemNeed;
        return (sawBit & need) != 0;
    }

    /**
     * send one confreq
     */
    public void sendReq() {
        if (sawReq2++ > 32) {
            parent.clearState();
            return;
        }
        sawReq3++;
        sawBit |= sawTxReq;
        packHolder pck = new packHolder(true, true);
        lastTxIdnt = bits.randomB();
        pck.clear();
        pck.putStart();
        writeOptions(pck, txReq);
        parent.sendNcpCtrl(pck, getPPPctrl(), codeConfReq, lastTxIdnt);
        if (debugger.ifcPppEvnt) {
            logger.debug("options: " + txReq);
        }
    }

    /**
     * received one confreq
     *
     * @param pck packet containing request
     * @param code code
     * @param id identification
     */
    public void recvPck(packHolder pck, int code, int id) {
        if ((sawBit & sawFrcClsd) != 0) {
            return;
        }
        cntr.rx(pck);
        packHolder rej = new packHolder(true, true);
        rej.clear();
        rej.putStart();
        Object tmp1;
        switch (code) {
            case codeConfReq:
                rej = pck.copyBytes(true, true);
                sawBit |= sawRemNeed;
                Object tmp2 = readOptions(pck);
                if (tmp2 == null) {
                    parent.sendNcpCtrl(pck, getPPPctrl(), codeConfRej, id);
                    return;
                }
                sawBit |= sawRxReq;
                tmp1 = gotConfReq(tmp2, true);
                if (tmp1 != null) {
                    rej.clear();
                    rej.putStart();
                    sawNakC = 0;
                    writeOptions(rej, tmp1);
                    rej = copyOptions(rej, pck);
                    parent.sendNcpCtrl(rej, getPPPctrl(), codeConfRej, id);
                    if (debugger.ifcPppEvnt) {
                        logger.debug("options: " + tmp1);
                    }
                    break;
                }
                tmp1 = gotConfReq(tmp2, false);
                if (tmp1 != null) {
                    rej.clear();
                    rej.putStart();
                    sawNakC++;
                    writeOptions(rej, tmp1);
                    int siz = rej.headSize();
                    if (siz != sawNakS) {
                        sawNakC = 0;
                        sawNakS = siz;
                    }
                    if (sawNakC < parent.nakRetryLimit) {
                        parent.sendNcpCtrl(rej, getPPPctrl(), codeConfNak, id);
                    } else {
                        rej = copyOptions(rej, pck);
                        parent.sendNcpCtrl(rej, getPPPctrl(), codeConfRej, id);
                    }
                    if (debugger.ifcPppEvnt) {
                        logger.debug("options: " + tmp1);
                    }
                    break;
                }
                sawBit |= sawTxAck;
                parent.sendNcpCtrl(rej, getPPPctrl(), codeConfAck, id);
                if (debugger.ifcPppEvnt) {
                    logger.debug("options: " + tmp2);
                }
                if (sawReq1++ > 3) {
                    clearState();
                    break;
                }
                if (!getReady()) {
                    sawReq1 = 0;
                }
                break;
            case codeConfAck:
                if (id != lastTxIdnt) {
                    return;
                }
                sawReq2 = 0;
                tmp1 = readOptions(pck);
                if (tmp1 == null) {
                    return;
                }
                if (gotConfAck(tmp1)) {
                    break;
                }
                sawBit |= sawRxAck;
                break;
            case codeConfNak:
                if (id != lastTxIdnt) {
                    return;
                }
                sawReq2 = 0;
                tmp1 = readOptions(pck);
                if (tmp1 == null) {
                    return;
                }
                gotConfNak(tmp1);
                sendReq();
                break;
            case codeConfRej:
                if (id != lastTxIdnt) {
                    return;
                }
                sawReq2 = 0;
                tmp1 = readOptions(pck);
                if (tmp1 == null) {
                    return;
                }
                gotConfRej(tmp1);
                sendReq();
                break;
            case codeTermReq:
                pck.clear();
                pck.putStart();
                parent.sendNcpCtrl(pck, getPPPctrl(), codeTermAck, id);
                parent.clearState();
                break;
            default:
                if (!gotUnknownCode(pck, code, id)) {
                    break;
                }
                parent.sendNcpCtrl(pck, getPPPctrl(), codeCodeRej, id);
                break;
        }
    }

    /**
     * get tlv handler
     *
     * @return tlv
     */
    protected typLenVal getTlv() {
        return new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);
    }

    /**
     * read up options
     *
     * @param pck packet to parse
     * @return readed config values, null if reject needed
     */
    public Object readOptions(packHolder pck) {
        packHolder rej = new packHolder(true, true);
        Object config = getOneConfig(false);
        int siz = pck.dataSize();
        typLenVal tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (!readOption(config, tlv)) {
                continue;
            }
            tlv.putThis(rej);
        }
        pck.setBytesLeft(siz);
        if (debugger.ifcPppEvnt) {
            logger.debug("options: " + config);
        }
        if (rej.headSize() > 0) {
            rej.merge2beg();
            byte[] buf = rej.getCopy();
            pck.clear();
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            return null;
        }
        return config;
    }

    /**
     * copy option data
     *
     * @param lst generated options
     * @param src original options
     * @return list of options
     */
    public packHolder copyOptions(packHolder lst, packHolder src) {
        lst.merge2beg();
        packHolder res = new packHolder(true, true);
        int siz = src.dataSize();
        typLenVal tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(lst)) {
                break;
            }
            byte[] buf = tlv.copyBytes();
            int typ = tlv.valTyp;
            for (;;) {
                if (tlv.getBytes(src)) {
                    break;
                }
                if (tlv.valTyp != typ) {
                    continue;
                }
                buf = tlv.copyBytes();
                break;
            }
            tlv.putBytes(res, typ, buf);
            src.setBytesLeft(siz);
        }
        return res;
    }

    /**
     * get ppp subprotocol name
     *
     * @return value
     */
    public abstract String getPPPname();

    /**
     * get ppp control code
     *
     * @return value
     */
    public abstract int getPPPctrl();

    /**
     * get ppp data code
     *
     * @return value
     */
    public abstract int getPPPdata();

    /**
     * get ethertype
     *
     * @return value
     */
    public abstract int getPPPetyp();

    /**
     * clear state of ncp
     */
    public abstract void clearState();

    /**
     * get one configuration
     *
     * @param txReq true for tx config request, false if empty one
     * @return configuration returned
     */
    public abstract Object getOneConfig(boolean txReq);

    /**
     * read up one option
     *
     * @param config where to store configuration
     * @param tlv tlv to use
     * @return false if successful, true if reject needed
     */
    public abstract boolean readOption(Object config, typLenVal tlv);

    /**
     * write options
     *
     * @param pck packet to update
     * @param data options to use
     */
    public abstract void writeOptions(packHolder pck, Object data);

    /**
     * got config acknowledge
     *
     * @param data options got
     * @return false on success, true on error
     */
    public abstract boolean gotConfAck(Object data);

    /**
     * got config request
     *
     * @param data options got
     * @param rej report just rejects
     * @return options to send back as nak
     */
    public abstract Object gotConfReq(Object data, boolean rej);

    /**
     * got config nak
     *
     * @param data options got
     */
    public abstract void gotConfNak(Object data);

    /**
     * got config reject
     *
     * @param data options got
     */
    public abstract void gotConfRej(Object data);

    /**
     * handle unknown code
     *
     * @param pck packet containing request
     * @param code code
     * @param id identification
     * @return false if handled successfully, true to send code reject
     */
    public abstract boolean gotUnknownCode(packHolder pck, int code, int id);

}
