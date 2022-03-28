package net.freertr.sec;

import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyECDSA;
import net.freertr.cry.cryKeyGeneric;
import net.freertr.cry.cryKeyRSA;
import net.freertr.pack.packSsh;
import net.freertr.pack.packSshAuth;
import net.freertr.pack.packSshChan;
import net.freertr.pack.packSshInit;
import net.freertr.pack.packSshKex;
import net.freertr.pack.packSshSign;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userReader;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * secure shell (rfc4251) protocol
 *
 * @author matecsaba
 */
public class secSsh implements Runnable {

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
     * server authentication list
     */
    protected authGeneric servAuth;

    /**
     * authenticated user
     */
    public authResult servUser;

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
     * client pubkey
     */
    protected byte[] clntPubkey;

    /**
     * client username
     */
    protected String clntUser;

    /**
     * client password
     */
    protected String clntPass;

    /**
     * remote channel id
     */
    protected int chanRem;

    /**
     * local channel id
     */
    protected int chanLoc;

    /**
     * bytes received in channel
     */
    protected int chanBytes;

    /**
     * default port number
     */
    public static final int portNum = 22;

    private packSsh pckRx;

    private packSsh pckTx;

    /**
     * start ssh on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     */
    public secSsh(pipeSide session, pipeLine pipe) {
        lower = session;
        lower.setTime(120 * 1000);
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.setTime(120 * 1000);
        userS.setTime(120 * 1000);
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
     * @param user username
     * @param pass password
     */
    public void startClient(byte[] pubkey, String user, String pass) {
        client = true;
        clntUser = user;
        clntPass = pass;
        clntPubkey = pubkey;
        workerStart();
    }

    /**
     * start server connection
     *
     * @param auth authentication list
     * @param rsa rsa key
     * @param dsa dss key
     * @param ecdsa ecdss key
     */
    public void startServer(authGeneric auth, cryKeyRSA rsa, cryKeyDSA dsa, cryKeyECDSA ecdsa) {
        client = false;
        servAuth = auth;
        keyrsa = rsa;
        keydsa = dsa;
        keyecdsa = ecdsa;
        workerStart();
    }

    private void workerStart() {
        new Thread(this).start();
    }

    public void run() {
        try {
            if (client) {
                workerClient();
            } else {
                workerServer();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.setClose();
        userP.setClose();
        if (debugger.secSshTraf) {
            logger.debug("main stopped");
        }
    }

    private int serverAuther(packSsh p, packSshAuth pa) {
        if (pa.method.equals(packSsh.authenNone)) {
            servUser = servAuth.authUserNone(pa.username);
            if (servUser == null) {
                return 2;
            }
            if (servUser.result == authResult.authSuccessful) {
                return 1;
            }
            return 2;
        }
        if (pa.method.equals(packSsh.authenPass)) {
            servUser = servAuth.authUserPass(pa.username, pa.password);
            if (servUser == null) {
                return 0;
            }
            if (servUser.result == authResult.authSuccessful) {
                return 1;
            }
            return 0;
        }
        if (!pa.method.equals(packSsh.authenPkey)) {
            return 0;
        }
        packSshSign sgn = new packSshSign(pa.password);
        if (pa.pkeySign == null) {
            cryKeyGeneric vrf = sgn.getKeyVerifier();
            if (vrf == null) {
                return 2;
            }
            if (vrf.sshReader(pa.pkeyBlob)) {
                return 2;
            }
            servUser = servAuth.authUserPkey(vrf, pa.username);
            if (servUser == null) {
                return 2;
            }
            if (servUser.result != authResult.authSuccessful) {
                return 2;
            }
            pa.authPkeyCreate();
            p.packSend();
            doPackRecv(p);
            if (pa.authReqParse()) {
                return 0;
            }
        }
        cryKeyGeneric vrf = sgn.getKeyVerifier();
        if (vrf == null) {
            return 0;
        }
        cryHashGeneric hsh = sgn.getKeyHashAlgo();
        if (hsh == null) {
            return 0;
        }
        String alg = sgn.getKeyHashAlgn();
        if (alg == null) {
            return 0;
        }
        if (vrf.sshReader(pa.pkeyBlob)) {
            return 0;
        }
        servUser = servAuth.authUserPkey(vrf, hsh, alg, pa.getAuthen2signed(), pa.username, pa.pkeySign);
        if (servUser == null) {
            return 0;
        }
        if (servUser.result == authResult.authSuccessful) {
            return 1;
        }
        return 0;
    }

    private void workerThreads(packSsh p) {
        userS.setReady();
        pckRx = p.copyBytes();
        pckTx = p.copyBytes();
        new secSshRx(this);
        new secSshTx(this);
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

    private void doPackRecv(packSsh p) {
        for (;;) {
            p.packRecv();
            switch (p.pckTyp) {
                case packSsh.typeChanWin:
                    packSshChan pc = new packSshChan(p);
                    if (pc.chanWindowParse()) {
                        return;
                    }
                    break;
                case packSsh.typeExtInfo:
                    packSshInit pi = new packSshInit(p);
                    if (pi.extensInfoParse()) {
                        break;
                    }
                    break;
                case packSsh.typeGlobReq:
                case packSsh.typeDebug:
                case packSsh.typeAuthBann:
                case packSsh.typeIgnore:
                case packSsh.typeUnimp:
                    break;
                default:
                    return;
            }
        }
    }

    /**
     * receiver worker
     */
    protected void workerRx() {
        packSshChan pc = new packSshChan(pckRx);
        for (;;) {
            pckRx.packRecv();
            switch (pckRx.pckTyp) {
                case packSsh.typeChanData:
                case packSsh.typeChanExtDat:
                    if (pc.chanDataParse()) {
                        return;
                    }
                    userS.morePut(pc.buf, 0, pc.buf.length);
                    chanBytes += pc.buf.length;
                    break;
                case packSsh.typeChanOpen:
                    if (pc.chanOpenParse()) {
                        return;
                    }
                    int tmp = pc.chanRem;
                    pc = new packSshChan(pckTx);
                    pc.chanRem = tmp;
                    pc.openFailCreate();
                    pckTx.packSend();
                    pc = new packSshChan(pckRx);
                    break;
                case packSsh.typeChanReq:
                    if (pc.chanReqParse()) {
                        return;
                    }
                    if (!processChanReq(pckRx, pc)) {
                        if (!pc.needReply) {
                            break;
                        }
                        pc = new packSshChan(pckTx);
                        pc.chanRem = chanRem;
                        pc.chanFailCreate();
                        pckTx.packSend();
                        pc = new packSshChan(pckRx);
                        break;
                    }
                    if (!pc.needReply) {
                        break;
                    }
                    pc = new packSshChan(pckTx);
                    pc.chanRem = chanRem;
                    pc.chanSuccCreate();
                    pckTx.packSend();
                    pc = new packSshChan(pckRx);
                    break;
                case packSsh.typeChanWin:
                    if (pc.chanWindowParse()) {
                        return;
                    }
                    break;
                case packSsh.typeDebug:
                case packSsh.typeIgnore:
                case packSsh.typeUnimp:
                case packSsh.typeGlobReq:
                    break;
                default:
                    return;
            }
        }
    }

    /**
     * sender worker
     */
    protected void workerTx() {
        packSshChan pc = new packSshChan(pckTx);
        for (;;) {
            pckTx.pipe.setTime(userS.getTime());
            if (chanBytes > 1024) {
                pc.chanRem = chanRem;
                pc.window = chanBytes;
                pc.chanWindowCreate();
                pckTx.packSend();
                chanBytes -= pc.window;
            }
            byte[] buf = new byte[1024];
            int i = userS.blockingGet(buf, 0, 1);
            if (i < 0) {
                return;
            }
            int o = userS.nonBlockGet(buf, i, buf.length - i);
            if (o < 0) {
                o = 0;
            }
            pc.buf = new byte[o + i];
            if (pc.buf.length < 1) {
                continue;
            }
            for (i = 0; i < pc.buf.length; i++) {
                pc.buf[i] = buf[i];
            }
            pc.chanRem = chanRem;
            pc.chanDataCreate();
            pckTx.packSend();
        }
    }

    private boolean processChanReq(packSsh p, packSshChan pc) {
        if (pc.type.equals(packSshChan.reqFlwCtr)) {
            return true;
        }
        if (pc.type.equals(packSshChan.reqSignal)) {
            return true;
        }
        if (pc.type.equals(packSshChan.reqWindow)) {
            userReader.setTermWdt(userS, p.pckDat.msbGetD(0));
            userReader.setTermLen(userS, p.pckDat.msbGetD(4));
            return true;
        }
        return false;
    }

    private void workerServer() {
        if (debugger.secSshTraf) {
            logger.debug("starting");
        }
        packSsh p = new packSsh();
        p.pipe = lower;
        packSshInit pi = new packSshInit(p);
        pi.exchangeVersion();
        packSshKex pg = new packSshKex(p);
        pg.hashStr(pi.remoteVersion);
        pg.hashStr(packSshInit.getLocalVersion());
        pg.hashSwap();
        pi.kexInitFill();
        pi.kexInitCreate(false);
        pg.hashPck();
        p.packSend();
        doPackRecv(p);
        pg.hashSwap();
        pg.hashPck();
        pg.hashMerge();
        if (pi.kexInitParse(false)) {
            return;
        }
        if (pi.kexInitChoose(pi, null)) {
            return;
        }
        pg.difHel = pi.getDHgroup();
        pg.hasher = pi.getDHhash();
        cryKeyGeneric key = pi.kexKeys.getKeySigner(keydsa, keyrsa);
        pg.cert = key.sshWriter();
        pg.hashInt(pg.cert.length);
        pg.hashBuf(pg.cert);
        if (pg.difHel == null) {
            doPackRecv(p);
            pg.hashPay();
            if (pg.gexReqParse()) {
                return;
            }
            pg.gexGroupFill();
            pg.gexGroupCreate();
            p.packSend();
            pg.hashBig(pg.difHel.modulus);
            pg.hashBig(pg.difHel.group);
            doPackRecv(p);
            if (pg.gexInitParse()) {
                return;
            }
            pg.difHel.servXchg();
            pg.difHel.servKey();
            pg.hashCalc();
            pg.gexReplyFill(pi.kexKeys.getKeyHashAlgo(), pi.kexKeys.getKeyHashAlgn(), key);
            pg.gexReplyCreate();
            p.packSend();
        } else {
            doPackRecv(p);
            if (pg.kexInitParse()) {
                return;
            }
            pg.difHel.servXchg();
            pg.difHel.servKey();
            pg.hashCalc();
            pg.gexReplyFill(pi.kexKeys.getKeyHashAlgo(), pi.kexKeys.getKeyHashAlgn(), key);
            pg.kexReplyCreate();
            p.packSend();
        }
        if (pi.newKeysExchange()) {
            return;
        }
        pg.encSetup(pi, false);
        if (pi.kexExts) {
            pi.extensInfoCreate();
            p.packSend();
        }
        packSshAuth pa = new packSshAuth(p, pg);
        doPackRecv(p);
        if (pa.servReqParse()) {
            return;
        }
        if (!pa.service.equals(packSsh.serviceAuth)) {
            return;
        }
        pa.servActpCreate(packSsh.serviceAuth);
        p.packSend();
        for (int retry = 0;; retry++) {
            if (retry > 5) {
                return;
            }
            doPackRecv(p);
            if (pa.authReqParse()) {
                continue;
            }
            int res = serverAuther(p, pa);
            if (res == 1) {
                break;
            }
            if (res == 0) {
                bits.sleep(3000 + bits.random(1, 1000));
            } else {
                retry--;
            }
            pa.authFailCreate();
            p.packSend();
        }
        userS.settingsPut(pipeSetting.authed, servUser);
        pa.authSuccCreate();
        p.packSend();
        doPackRecv(p);
        packSshChan pc = new packSshChan(p);
        if (pc.chanOpenParse()) {
            return;
        }
        chanRem = pc.chanRem;
        pc.openDoneCreate();
        chanLoc = pc.chanLoc;
        p.packSend();
        for (;;) {
            doPackRecv(p);
            if (pc.chanReqParse()) {
                return;
            }
            pc.chanRem = chanRem;
            if (processChanReq(p, pc)) {
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                continue;
            }
            if (pc.type.equals(packSshChan.reqShell)) {
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                break;
            }
            if (pc.type.equals(packSshChan.reqExec)) {
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                break;
            }
            if (pc.type.equals(packSshChan.reqSubsys)) {
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                break;
            }
            if (pc.type.equals(packSshChan.reqPtyReq)) {
                p.stringRead();
                userReader.setTermWdt(userS, p.pckDat.msbGetD(0));
                userReader.setTermLen(userS, p.pckDat.msbGetD(4));
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                continue;
            }
            if (pc.type.equals(packSshChan.reqEnv)) {
                if (pc.needReply) {
                    pc.chanSuccCreate();
                    p.packSend();
                }
                continue;
            }
            if (pc.needReply) {
                pc.chanFailCreate();
                p.packSend();
            }
        }
        workerThreads(p);
    }

    private void workerClient() {
        if (debugger.secSshTraf) {
            logger.debug("starting");
        }
        packSsh p = new packSsh();
        p.pipe = lower;
        packSshKex pg = new packSshKex(p);
        packSshInit pi = new packSshInit(p);
        pi.exchangeVersion();
        pg.hashStr(packSshInit.getLocalVersion());
        pg.hashStr(pi.remoteVersion);
        pi.kexInitFill();
        pi.kexInitCreate(true);
        pg.hashPck();
        p.packSend();
        doPackRecv(p);
        pg.hashPck();
        pg.hashSwap();
        if (pi.kexInitParse(true)) {
            return;
        }
        if (pi.kexInitChoose(null, pi)) {
            return;
        }
        pg.difHel = pi.getDHgroup();
        pg.hasher = pi.getDHhash();
        if (pg.difHel == null) {
            pg.gexReqFill();
            pg.gexReqCreate();
            pg.hashPay();
            p.packSend();
            doPackRecv(p);
            if (pg.gexGroupParse()) {
                return;
            }
            pg.hashBig(pg.difHel.modulus);
            pg.hashBig(pg.difHel.group);
            pg.gexInitFill();
            pg.gexInitCreate();
            p.packSend();
            doPackRecv(p);
            if (pg.gexReplyParse()) {
                return;
            }
        } else {
            pg.gexInitFill();
            pg.kexInitCreate();
            p.packSend();
            doPackRecv(p);
            if (pg.kexReplyParse()) {
                return;
            }
        }
        pg.difHel.clntKey();
        pg.hashSwap();
        pg.hashInt(pg.cert.length);
        pg.hashBuf(pg.cert);
        pg.hashMerge();
        pg.hashCalc();
        if (clntPubkey != null) {
            if (clntPubkey.length != pg.cert.length) {
                return;
            }
            if (bits.byteComp(clntPubkey, 0, pg.cert, 0, clntPubkey.length) != 0) {
                return;
            }
        }
        cryKeyGeneric key = pi.kexKeys.getKeyVerifier();
        if (key.sshReader(pg.cert)) {
            return;
        }
        if (key.sshVerify(pi.kexKeys.getKeyHashAlgo(), pi.kexKeys.getKeyHashAlgn(), pg.hashVal, pg.sign)) {
            return;
        }
        if (pi.newKeysExchange()) {
            return;
        }
        pg.encSetup(pi, true);
        packSshAuth pa = new packSshAuth(p, pg);
        pa.servReqCreate(packSsh.serviceAuth);
        p.packSend();
        doPackRecv(p);
        if (pa.servAcptParse()) {
            return;
        }
        pa.username = clntUser;
        pa.password = clntPass;
        pa.authReqCreate(packSsh.serviceConn);
        p.packSend();
        doPackRecv(p);
        if (pa.authSuccParse()) {
            return;
        }
        packSshChan pc = new packSshChan(p);
        pc.chanOpenCreate();
        chanLoc = pc.chanRem;
        p.packSend();
        doPackRecv(p);
        if (pc.openDoneParse()) {
            return;
        }
        chanRem = pc.chanLoc;
        pc.chanRem = chanRem;
        pc.chanReqCreatePty();
        p.packSend();
        doPackRecv(p);
        if (pc.chanSuccParse()) {
            return;
        }
        pc.chanRem = chanRem;
        pc.chanReqCreateShll();
        p.packSend();
        doPackRecv(p);
        if (pc.chanSuccParse()) {
            return;
        }
        workerThreads(p);
    }

}

class secSshRx implements Runnable {

    private secSsh lower;

    public secSshRx(secSsh parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerRx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secSshTraf) {
            logger.debug("rx stopped");
        }
    }

}

class secSshTx implements Runnable {

    private secSsh lower;

    public secSshTx(secSsh parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerTx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secSshTraf) {
            logger.debug("tx stopped");
        }
    }

}
