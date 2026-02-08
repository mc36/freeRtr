package org.freertr.serv;

import org.freertr.addr.addrMac;
import org.freertr.ifc.ifcEther;
import org.freertr.pack.packHolder;
import org.freertr.pack.packOpenflow;
import org.freertr.pipe.pipeSide;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * one openflow receiver
 *
 * @author matecsaba
 */
class servOpenflowRx implements Runnable {

    /**
     * true if the transmitter is working
     */
    public boolean working = true;

    private final pipeSide pipe;

    private final servOpenflow lower;

    public servOpenflowRx(pipeSide stream, servOpenflow parent) {
        pipe = stream;
        lower = parent;
        logger.startThread(this);
    }

    public void run() {
        try {
            doWord();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        lower.notif.wakeup();
    }

    private void doWord() {
        packHolder pckB = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = pipe;
        for (;;) {
            if (!working) {
                return;
            }
            if (pckO.recvPack(pckB)) {
                return;
            }
            if (debugger.servOpenflowRx) {
                logger.debug("rx " + pckO.dump(pckB));
            }
            switch (pckO.type) {
                case packOpenflow.typHello:
                    break;
                case packOpenflow.typEchoReq:
                    pckO.createEchoRep();
                    lower.sendPack(pckB, pckO);
                    break;
                case packOpenflow.typEchoRep:
                    break;
                case packOpenflow.typPortStat:
                    servOpenflowIfc1 ntry = new servOpenflowIfc1();
                    ntry.id = pckB.msbGetD(8);
                    ntry = lower.expIfc.find(ntry);
                    if (ntry == null) {
                        break;
                    }
                    if ((pckB.msbGetD(44) & 1) != 0) {
                        ntry.lastState = state.states.down;
                    } else {
                        ntry.lastState = state.states.up;
                    }
                    ntry.upper.setState(ntry.lastState);
                    break;
                case packOpenflow.typPackIn:
                    lower.cntr.rx(pckB);
                    ntry = new servOpenflowIfc1();
                    ntry.id = pckO.parsePckIn(pckB);
                    ntry = lower.expIfc.find(ntry);
                    if (ntry == null) {
                        lower.cntr.drop(pckB, counter.reasons.noIface);
                        break;
                    }
                    ifcEther.parseETHheader(pckB, false);
                    ntry.upper.recvPack(pckB);
                    break;
                case packOpenflow.typMultiRep:
                    switch (pckB.msbGetW(0)) {
                        case 4:
                            pckB.getSkip(8);
                            int i = pckB.msbGetD(0);
                            if (debugger.servOpenflowRx) {
                                logger.debug("port #" + i + " stats");
                            }
                            ntry = new servOpenflowIfc1();
                            ntry.id = i;
                            ntry = lower.expIfc.find(ntry);
                            if (ntry == null) {
                                break;
                            }
                            ntry.ifc.ethtyp.hwCntr.packRx = pckB.msbGetQ(8);
                            ntry.ifc.ethtyp.hwCntr.packTx = pckB.msbGetQ(16);
                            ntry.ifc.ethtyp.hwCntr.byteRx = pckB.msbGetQ(24);
                            ntry.ifc.ethtyp.hwCntr.byteTx = pckB.msbGetQ(32);
                            ntry.ifc.ethtyp.hwCntr.packDr = pckB.msbGetQ(40) + pckB.msbGetQ(48);
                            ntry.ifc.ethtyp.hwHstry.update(ntry.ifc.ethtyp.hwCntr, true);
                            if (ntry.ifc.ethtyp.hwSub == null) {
                                break;
                            }
                            ntry.ifc.ethtyp.hwCntr = ntry.ifc.ethtyp.hwCntr.minus(ntry.ifc.ethtyp.hwSub);
                            break;
                        case 13:
                            pckB.getSkip(8);
                            i = pckB.msbGetD(0);
                            addrMac mac = new addrMac();
                            pckB.getAddr(mac, 8);
                            if (debugger.servOpenflowRx) {
                                logger.debug("port #" + i + " mac=" + mac);
                            }
                            break;
                    }
                    break;
                case packOpenflow.typFeatRep:
                    if (debugger.servOpenflowRx) {
                        logger.debug("datapath=" + pckB.msbGetQ(0) + " buffers=" + pckB.msbGetD(8) + " tables=" + pckB.getByte(12));
                    }
                    break;
                default:
                    logger.info("got invalid message type=" + pckO.type);
                    break;
            }
        }
    }

}
