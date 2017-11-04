package sec;

import pack.packTls;
import pack.packTlsHndshk;
import pipe.pipeLine;
import pipe.pipeSide;
import util.bits;
import util.debugger;
import util.logger;
import cfg.cfgAll;
import cry.cryCertificate;
import cry.cryKeyDSA;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;

/**
 * transport layer security (rfc5246) protocol
 *
 * @author matecsaba
 */
public class secTls implements Runnable {

    /**
     * lower layer session to use for encrypted communication
     */
    public final pipeSide lower;

    /**
     * user side of cleartext pipeline
     */
    public final pipeSide userC;

    /**
     * mode of operation, true=client, false=server
     */
    protected boolean client;

    /**
     * my side of cleartext pipeline
     */
    protected final pipeSide userS;

    /**
     * cleartext pipeline
     */
    protected final pipeLine userP;

    /**
     * rsa key
     */
    protected cryKeyRSA keyrsa;

    /**
     * dss key
     */
    protected cryKeyDSA keydsa;

    /**
     * ecdss key
     */
    protected cryKeyECDSA keyecdsa;

    /**
     * rsa certificate
     */
    protected cryCertificate certrsa;

    /**
     * dss certificate
     */
    protected cryCertificate certdsa;

    /**
     * ecdss certificate
     */
    protected cryCertificate certecdsa;

    /**
     * forced version
     */
    public int forcedVer = -1;

    private boolean datagram;

    /**
     * start tls on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     * @param dtls datagram mode
     */
    public secTls(pipeSide session, pipeLine pipe, boolean dtls) {
        lower = session;
        lower.timeout = 120 * 1000;
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.timeout = 120 * 1000;
        userS.timeout = userC.timeout;
        datagram = dtls;
    }

    /**
     * get user side pipeline
     *
     * @return cleartext pipeline
     */
    public pipeSide getPipe() {
        return userC;
    }

    /**
     * start client connection
     */
    public void startClient() {
        client = true;
        workerStart();
    }

    /**
     * start server connection
     *
     * @param rsaK rsa key
     * @param dsaK dss key
     * @param ecdsaK ecdss key
     * @param rsaC rsa certificate
     * @param dsaC dss certificate
     * @param ecdsaC ecdss certificate
     */
    public void startServer(cryKeyRSA rsaK, cryKeyDSA dsaK, cryKeyECDSA ecdsaK, cryCertificate rsaC, cryCertificate dsaC, cryCertificate ecdsaC) {
        client = false;
        keyrsa = rsaK;
        keydsa = dsaK;
        keyecdsa = ecdsaK;
        certrsa = rsaC;
        certdsa = dsaC;
        certecdsa = ecdsaC;
        if (certrsa == null) {
            certrsa = cryCertificate.createSelfSigned(keyrsa, cfgAll.hostName, 365);
        }
        if (certdsa == null) {
            certdsa = cryCertificate.createSelfSigned(keydsa, cfgAll.hostName, 365);
        }
        if (certecdsa == null) {
            certecdsa = cryCertificate.createSelfSigned(keyecdsa, cfgAll.hostName, 365);
        }
        workerStart();
    }

    private void workerStart() {
        new Thread(this).start();
    }

    public void run() {
        try {
            packTls p;
            if (client) {
                p = workerClient();
            } else {
                p = workerServer();
            }
            if (p != null) {
                workerThreads(p);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.setClose();
        userP.setClose();
        if (debugger.secTlsTraf) {
            logger.debug("main stopped");
        }
    }

    /**
     * receiver worker
     *
     * @param p packet to use
     */
    protected void workerRx(packTls p) {
        if (debugger.secTlsTraf) {
            logger.debug("rx started");
        }
        for (;;) {
            p.packRecv();
            if (p.pckTyp != packTls.typeAppDat) {
                return;
            }
            userS.morePut(p.pckDat.getCopy(), 0, p.pckDat.dataSize());
        }
    }

    /**
     * sender worker
     *
     * @param p packet to use
     */
    protected void workerTx(packTls p) {
        if (debugger.secTlsTraf) {
            logger.debug("tx started");
        }
        for (;;) {
            p.pipe.timeout = userS.timeout;
            byte[] buf = new byte[1024];
            int len;
            if (datagram) {
                len = userS.blockingGet(buf, 0, buf.length);
                if (len < 0) {
                    return;
                }
            } else {
                int i = userS.blockingGet(buf, 0, 1);
                if (i < 0) {
                    return;
                }
                len = userS.nonBlockGet(buf, i, buf.length - i);
                if (len < 0) {
                    len = 0;
                }
                len += i;
            }
            p.pckDat.clear();
            p.pckDat.putCopy(buf, 0, 0, len);
            p.pckDat.putSkip(len);
            p.pckTyp = packTls.typeAppDat;
            p.packSend();
        }
    }

    private void workerThreads(packTls p) {
        userS.setReady();
        new secTlsRx(this, p.copyBytes());
        new secTlsTx(this, p.copyBytes());
        for (;;) {
            bits.sleep(1000);
            if (userS.isClosed() != 0) {
                return;
            }
        }
    }

    /**
     * stop threads
     */
    protected void workerStop() {
        userP.setClose();
        lower.setClose();
    }

    private packTls workerClient() {
        if (debugger.secTlsTraf) {
            logger.debug("starting");
        }
        packTls p = new packTls(datagram);
        p.pipe = lower;
        if (forcedVer > 0) {
            p.verMax = forcedVer;
            p.verMin = forcedVer;
        }
        packTlsHndshk ph = new packTlsHndshk(p, datagram);
        ph.clntHelloFill();
        ph.clntHelloCreate();
        ph.headerCreate();
        p.packSend();
        p.verCurr = -1;
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (!ph.vrfyHelloParse()) {
            ph.clearXchgHash();
            ph.clntHelloFill();
            ph.clntHelloCreate();
            ph.headerCreate();
            p.packSend();
            p.verCurr = -1;
            p.packRecv();
            if (ph.headerParse()) {
                return null;
            }
        }
        if (ph.servHelloParse()) {
            return null;
        }
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.certDatParse()) {
            return null;
        }
        if (ph.servKexNeeded()) {
            p.packRecv();
            if (ph.headerParse()) {
                return null;
            }
            if (ph.servKexParse()) {
                return null;
            }
        }
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.servDoneParse()) {
            return null;
        }
        ph.clntKexFill();
        ph.clntKexCreate();
        ph.headerCreate();
        p.packSend();
        if (ph.calcKeys(true)) {
            return null;
        }
        ph.chgCipherCreate();
        p.packSend();
        ph.finishedFill(true);
        ph.finishedCreate();
        ph.headerCreate();
        p.encryptEna();
        p.packSend();
        p.encryptDis();
        p.packRecv();
        if (ph.chgCipherParse()) {
            return null;
        }
        p.encryptEna();
        ph.finishedFill(false);
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.finishedParse()) {
            return null;
        }
        return p;
    }

    private packTls workerServer() {
        if (debugger.secTlsTraf) {
            logger.debug("starting");
        }
        packTls p = new packTls(datagram);
        p.pipe = lower;
        if (forcedVer > 0) {
            p.verMax = forcedVer;
            p.verMin = forcedVer;
        }
        packTlsHndshk ph = new packTlsHndshk(p, datagram);
        ph.keyrsa = keyrsa;
        ph.keydsa = keydsa;
        ph.certrsa = certrsa;
        ph.certdsa = certdsa;
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.clntHelloParse()) {
            return null;
        }
        if (datagram) {
            ph.vrfyHelloFill();
            ph.vrfyHelloCreate();
            ph.headerCreate();
            p.packSend();
            ph.clearXchgHash();
            p.verCurr = -1;
            p.packRecv();
            if (ph.headerParse()) {
                return null;
            }
            if (ph.clntHelloParse()) {
                return null;
            }
        }
        if (ph.servHelloFill()) {
            return null;
        }
        ph.servHelloCreate();
        ph.headerCreate();
        p.packSend();
        ph.certDatFill();
        ph.certDatCreate();
        ph.headerCreate();
        p.packSend();
        if (ph.servKexNeeded()) {
            ph.servKexFill();
            ph.servKexCreate();
            ph.headerCreate();
            p.packSend();
        }
        ph.servDoneCreate();
        ph.headerCreate();
        p.packSend();
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.clntKexParse()) {
            return null;
        }
        if (ph.calcKeys(false)) {
            return null;
        }
        p.packRecv();
        if (ph.chgCipherParse()) {
            return null;
        }
        p.encryptEna();
        ph.finishedFill(true);
        p.packRecv();
        if (ph.headerParse()) {
            return null;
        }
        if (ph.finishedParse()) {
            return null;
        }
        p.encryptDis();
        ph.chgCipherCreate();
        p.packSend();
        ph.finishedFill(false);
        ph.finishedCreate();
        ph.headerCreate();
        p.encryptEna();
        p.packSend();
        return p;
    }

}

class secTlsRx implements Runnable {

    private secTls lower;

    private packTls pack;

    public secTlsRx(secTls parent, packTls p) {
        lower = parent;
        pack = p;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerRx(pack);
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secTlsTraf) {
            logger.debug("rx stopped");
        }
    }

}

class secTlsTx implements Runnable {

    private final secTls lower;

    private final packTls pack;

    public secTlsTx(secTls parent, packTls p) {
        lower = parent;
        pack = p;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerTx(pack);
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secTlsTraf) {
            logger.debug("tx stopped");
        }
    }

}
