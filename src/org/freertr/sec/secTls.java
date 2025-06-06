package org.freertr.sec;

import org.freertr.cfg.cfgAll;
import org.freertr.cry.cryCertificate;
import org.freertr.cry.cryKeyDSA;
import org.freertr.cry.cryKeyECDSA;
import org.freertr.cry.cryKeyMLDSA;
import org.freertr.cry.cryKeyRSA;
import org.freertr.pack.packTls;
import org.freertr.pack.packTlsHndshk;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

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
     * mldss key
     */
    protected cryKeyMLDSA keymldsa;

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
     * mldss certificate
     */
    protected cryCertificate certmldsa;

    /**
     * client pubkey
     */
    protected byte[] clntPubkey;

    /**
     * minimum version
     */
    public int minVer = -1;

    /**
     * maximum version
     */
    public int maxVer = -1;

    /**
     * server name
     */
    public String serverName = null;

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
        lower.setTime(120 * 1000);
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.setTime(120 * 1000);
        userS.setTime(120 * 1000);
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
     *
     * @param pubkey pubkey
     */
    public void startClient(byte[] pubkey) {
        client = true;
        clntPubkey = pubkey;
        workerStart();
    }

    /**
     * start server connection
     *
     * @param rsaK rsa key
     * @param dsaK dss key
     * @param ecdsaK ecdss key
     * @param mldsaK mldss key
     * @param rsaC rsa certificate
     * @param dsaC dss certificate
     * @param ecdsaC ecdss certificate
     * @param mldsaC mldss certificate
     */
    public void startServer(cryKeyRSA rsaK, cryKeyDSA dsaK, cryKeyECDSA ecdsaK, cryKeyMLDSA mldsaK, cryCertificate rsaC, cryCertificate dsaC, cryCertificate ecdsaC, cryCertificate mldsaC) {
        client = false;
        keyrsa = rsaK;
        keydsa = dsaK;
        keyecdsa = ecdsaK;
        keymldsa = mldsaK;
        certrsa = rsaC;
        certdsa = dsaC;
        certecdsa = ecdsaC;
        certmldsa = mldsaC;
        if ((certrsa == null) && (keyrsa != null)) {
            certrsa = cryCertificate.createSelfSigned(keyrsa, cfgAll.getFqdn(), 365);
        }
        if ((certdsa == null) && (keydsa != null)) {
            certdsa = cryCertificate.createSelfSigned(keydsa, cfgAll.getFqdn(), 365);
        }
        if ((certecdsa == null) && (keyecdsa != null)) {
            certecdsa = cryCertificate.createSelfSigned(keyecdsa, cfgAll.getFqdn(), 365);
        }
        if ((certmldsa == null) && (keymldsa != null)) {
            certmldsa = cryCertificate.createSelfSigned(keymldsa, cfgAll.getFqdn(), 365);
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
            workerThreads(p);
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
            if (p.aeadMode) {
                if (p.apackRecv()) {
                    return;
                }
            } else {
                p.packRecv();
            }
            if (p.pckTyp == packTls.typeHandshk) {
                continue;
            }
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
            p.pipe.setTime(userS.getTime());
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
            if (p.aeadMode) {
                if (p.apackSend()) {
                    return;
                }
            } else {
                p.packSend();
            }
        }
    }

    private void workerThreads(packTls p) {
        if (p == null) {
            return;
        }
        if (minVer >= 0) {
            if (minVer > p.verCurr) {
                return;
            }
        }
        if (maxVer >= 0) {
            if (maxVer < p.verCurr) {
                return;
            }
        }
        minVer = p.verCurr;
        maxVer = p.verCurr;
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

    private void setupCommon(packTls p) {
        p.pipe = lower;
        if (minVer >= 0) {
            p.verMin = minVer;
        }
        if (maxVer >= 0) {
            p.verMax = maxVer;
        }
    }

    private packTls verifyCerts(packTls p, packTlsHndshk ph) {
        if (p == null) {
            return null;
        }
        if (clntPubkey == null) {
            return p;
        }
        if (debugger.secTlsTraf) {
            logger.debug("verify trust");
        }
        if (ph.certUsed == null) {
            return null;
        }
        cryCertificate crt = new cryCertificate();
        if (crt.asn1ReadBuf(clntPubkey)) {
            return null;
        }
        if (cryCertificate.testClientCert(ph.certUsed, crt)) {
            return null;
        }
        return p;
    }

    private packTls workerClient() {
        if (debugger.secTlsTraf) {
            logger.debug("starting");
        }
        packTls p = new packTls(datagram);
        setupCommon(p);
        packTlsHndshk ph = new packTlsHndshk(p, datagram);
        ph.servNam = serverName;
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
        if (ph.minVer < 0x304) {
            p.packRecv();
            if (ph.headerParse()) {
                return null;
            }
            if (ph.certLstParse()) {
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
            if (ph.calcKeysDh(true)) {
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
            return verifyCerts(p, ph);
        }
        if (ph.clntHelloFillEc()) {
            return null;
        }
        ph.retriedCH = true;
        ph.clntHelloFill();
        ph.clntHelloCreate();
        ph.headerCreate();
        p.packSend();
        p.verCurr = -1;
        p.packRecv();
        if (!ph.chgCipherParse()) {
            p.packRecv();
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.servHelloParse()) {
            return null;
        }
        if (!ph.chgCipherParse()) {
            p.packRecv();
        }
        if (ph.calcKeysEc(true)) {
            return null;
        }
        if (ph.calcKeysHs(true)) {
            return null;
        }
        if (p.apackRecv()) {
            return null;
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.encrExtParse()) {
            return null;
        }
        if (p.apackRecv()) {
            return null;
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.certDatParse()) {
            return null;
        }
        ph.certVrfFill();
        if (p.apackRecv()) {
            return null;
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.certVrfParse()) {
            return null;
        }
        ph.finishedFill(true);
        if (p.apackRecv()) {
            return null;
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.finishedParse()) {
            return null;
        }
        ph.calcKeysAp();
        ph.finishedFill(false);
        ph.finishedCreate();
        ph.headerCreate();
        p.apackSend();
        ph.applyKeys(true, true);
        p.verCurr = ph.minVer;
        return verifyCerts(p, ph);
    }

    private packTls workerServer() {
        if (debugger.secTlsTraf) {
            logger.debug("starting");
        }
        packTls p = new packTls(datagram);
        setupCommon(p);
        packTlsHndshk ph = new packTlsHndshk(p, datagram);
        ph.keyrsa = keyrsa;
        ph.keydsa = keydsa;
        ph.keyecdsa = keyecdsa;
        ph.keymldsa = keymldsa;
        ph.certrsa = certrsa;
        ph.certdsa = certdsa;
        ph.certecdsa = certecdsa;
        ph.certmldsa = certmldsa;
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
        if (ph.maxVer < 0x304) {
            if (ph.servHelloFill()) {
                return null;
            }
            ph.servHelloCreate();
            ph.headerCreate();
            p.packSend();
            ph.certDatFill();
            ph.certLstCreate();
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
            if (ph.calcKeysDh(false)) {
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
        if (ph.servHelloRetrying()) {
            ph.retriedCH = true;
            if (ph.servHelloFill()) {
                return null;
            }
            ph.servHelloFillRetry();
            ph.servHelloCreate();
            ph.headerCreate();
            p.packSend();
            p.verCurr = -1;
            p.packRecv();
            if (!ph.chgCipherParse()) {
                p.packRecv();
            }
            if (ph.headerParse()) {
                return null;
            }
            if (ph.clntHelloParse()) {
                return null;
            }
        }
        if (ph.servHelloFillEc()) {
            return null;
        }
        if (ph.calcKeysEc(false)) {
            return null;
        }
        if (ph.servHelloFill()) {
            return null;
        }
        ph.servHelloCreate();
        ph.headerCreate();
        p.packSend();
        if (!ph.chgCipherParse()) {
            p.packRecv();
        }
        if (ph.calcKeysHs(false)) {
            return null;
        }
        ph.encrExtFill();
        ph.encrExtCreate();
        ph.headerCreate();
        p.apackSend();
        ph.certDatFill();
        ph.certDatCreate();
        ph.headerCreate();
        p.apackSend();
        ph.certVrfFill();
        if (ph.certVrfCreate()) {
            return null;
        }
        ph.headerCreate();
        p.apackSend();
        ph.finishedFill(true);
        ph.finishedCreate();
        ph.headerCreate();
        p.apackSend();
        ph.calcKeysAp();
        if (!ph.retriedCH) {
            p.encryptDis();
            p.packRecv();
            if (ph.chgCipherParse()) {
                return null;
            }
        }
        ph.finishedFill(false);
        if (p.apackRecv()) {
            return null;
        }
        if (ph.headerParse()) {
            return null;
        }
        if (ph.finishedParse()) {
            return null;
        }
        ph.applyKeys(false, true);
        p.verCurr = ph.minVer;
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
