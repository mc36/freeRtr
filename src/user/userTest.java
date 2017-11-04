package user;

import addr.addrIP;
import addr.addrPrefix;
import auth.authConstant;
import auth.authLocal;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgIfc;
import clnt.clntSmtp;
import clnt.clntSnmp;
import clnt.clntSpeed;
import cry.cryAsn1;
import cry.cryBase64;
import cry.cryCertificate;
import cry.cryEncrCBCaes;
import cry.cryEncrCBCblowfish;
import cry.cryEncrCBCdes;
import cry.cryEncrCBCdes3;
import cry.cryEncrCBCrc2;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryHashHmac;
import cry.cryHashMd2;
import cry.cryHashMd5;
import cry.cryHashSha1;
import cry.cryHashSha256;
import cry.cryHashSha512;
import cry.cryKeyDH;
import cry.cryKeyDSA;
import cry.cryKeyECDH;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;
import cry.cryOtp;
import java.util.List;
import pack.packDnsZone;
import pack.packDnsRec;
import pack.packHolder;
import pack.packTls;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeSide;
import pipe.pipeWindow;
import sec.secSsh;
import sec.secTls;
import tab.tabRoute;
import util.bits;
import util.cmds;
import util.extMrkLng;
import util.logger;
import util.uniResLoc;

/**
 * process test commands
 *
 * @author matecsaba
 */
public class userTest {

    /**
     * command to use
     */
    public cmds cmd;

    /**
     * pipeline to use
     */
    public pipeSide pip;

    /**
     * reader to use
     */
    public userReader rdr;

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public String doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.test, false);
        if (alias != null) {
            return alias.getCommand(cmd);
        }
        if (a.equals("dns")) {
            a = cmd.word();
            packDnsZone zon = new packDnsZone(a);
            for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                cfgIfc ifc = cfgAll.ifaces.get(i);
                if (ifc.cloned) {
                    continue;
                }
                if (ifc.addr4 != null) {
                    packDnsRec rec = new packDnsRec();
                    rec.clss = packDnsRec.classIN;
                    rec.addr = new addrIP();
                    rec.name = ifc.name + "." + a;
                    rec.addr.fromIPv4addr(ifc.addr4);
                    rec.typ = packDnsRec.typeA;
                    zon.addBin(rec);
                }
                if (ifc.addr6 != null) {
                    packDnsRec rec = new packDnsRec();
                    rec.clss = packDnsRec.classIN;
                    rec.addr = new addrIP();
                    rec.name = ifc.name + "." + a;
                    rec.addr.fromIPv6addr(ifc.addr6);
                    rec.typ = packDnsRec.typeAAAA;
                    zon.addBin(rec);
                }
            }
            List<String> lst = zon.saveZone("");
            for (int i = 0; i < lst.size(); i++) {
                cmd.pipe.linePut(lst.get(i));
            }
            return null;
        }
        if (a.equals("speed")) {
            rdr.keyFlush();
            clntSpeed.smllClnt(cmd);
            rdr.keyFlush();
            return null;
        }
        if (a.equals("gc")) {
            System.gc();
            return null;
        }
        if (a.equals("snmp")) {
            a = cmd.word();
            clntSnmp sn = new clntSnmp();
            sn.cons = new pipeProgress(cmd.pipe);
            sn.host = cmd.word();
            sn.community = cmd.word();
            sn.oid = cmd.word();
            if (a.equals("get")) {
                sn.doGet();
                return null;
            }
            if (a.equals("next")) {
                sn.doNext();
                return null;
            }
            return null;
        }
        if (a.equals("smtp")) {
            clntSmtp sm = new clntSmtp(cmd.pipe);
            a = cmd.word();
            sm.rcpt.add(a);
            sm.putHead("test@" + cfgAll.hostName, a, "test message");
            a = cmd.getRemaining().trim();
            if (a.length() < 1) {
                a = "right now it worked fine";
            }
            sm.putText(bits.str2lst(a));
            sm.putFinish();
            cmd.error("res=" + sm.doSend(1));
            sm.cleanUp();
            return null;
        }
        if (a.equals("window")) {
            pipeSide pipWin = pipeWindow.create(80, 25,
                    userFonts1.font8x16data, userFonts1.colorData);
            logger.pipeStart(pipWin);
            userLine lin = new userLine();
            lin.execTimeOut = 0;
            lin.createHandler(pipWin, "window", true);
            return null;
        }
        if (a.equals("otppass")) {
            byte[] buf = cmd.word().getBytes();
            long t = (bits.getTime() + cfgAll.timeServerOffset) / 1000;
            a = cryOtp.calcTotp(buf, t, 8, new cryHashSha1());
            cmd.error("seed=" + bits.byteDump(buf, 0, buf.length));
            cmd.error("time=" + t);
            cmd.error("key=" + a);
            return null;
        }
        if (a.equals("password")) {
            a = cmd.getRemaining();
            cmd.error("original = '" + a + "'");
            a = authLocal.passwdDecode(a);
            cmd.error(" decoded = '" + a + "'");
            a = authLocal.passwdEncode(a);
            cmd.error(" encoded = '" + a + "'");
            return null;
        }
        if (a.equals("asn1parser")) {
            packHolder pck = new packHolder(true, true);
            if (pck.convertFromK12("|0   |" + cmd.getRemaining())) {
                cmd.error("error in packet");
                return null;
            }
            cmd.error("data: " + pck.dump());
            doShow(cryAsn1.dumpPack("", pck));
            return null;
        }
        if (a.equals("base64")) {
            a = cmd.getRemaining();
            cmd.error("data: " + a);
            byte[] b = cryBase64.decodeBytes(a);
            if (b == null) {
                cmd.error("error in data");
                return null;
            }
            cmd.error("decoded: " + bits.byteDump(b, 0, -1));
            a = cryBase64.encodeBytes(b);
            cmd.error("encoded: " + a);
            return null;
        }
        if (a.equals("xml")) {
            extMrkLng xml = extMrkLng.parseOne(cmd.getRemaining());
            cmd.error("orig: " + xml.orig);
            cmd.error("done: " + xml.toXMLstr());
            doShow(xml.show());
            return null;
        }
        if (a.equals("url")) {
            uniResLoc url = uniResLoc.parseOne(cmd.getRemaining());
            doShow(url.show());
            return null;
        }
        if (a.equals("addr")) {
            addrIP adr = new addrIP();
            a = cmd.word();
            boolean b = adr.fromString(a);
            String s = "" + adr;
            cmd.error("result=" + b + " equal=" + s.equals(a));
            cmd.error(" original=" + a);
            cmd.error("converted=" + s);
            return null;
        }
        if (a.equals("vm")) {
            a = cmd.word();
            userVM.doWork(pip, true, "", a, cmd.getRemaining());
            return null;
        }
        if (a.equals("logging")) {
            logger.logLev i = logger.string2level(cmd.word());
            switch (i) {
                case msgDebg:
                    logger.debug(cmd.getRemaining());
                    break;
                case msgEror:
                    logger.error(cmd.getRemaining());
                    break;
                case msgInfo:
                    logger.info(cmd.getRemaining());
                    break;
                case msgWarn:
                    logger.warn(cmd.getRemaining());
                    break;
                default:
                    break;
            }
            return null;
        }
        if (a.equals("routing")) {
            cmd.error("performing test");
            tabRoute<addrIP> tab1 = new tabRoute<addrIP>("test1");
            long rnd1 = 10000;
            long rnd2 = 50000;
            addrIP adr = new addrIP();
            long beg = bits.getTime();
            for (int i = (int) rnd1; i >= 0; i--) {
                byte[] buf = new byte[addrIP.size];
                bits.msbPutD(buf, 0, i);
                adr.fromBuf(buf, 0);
                tab1.add(3, new addrPrefix<addrIP>(adr, 32), adr);
            }
            beg = (rnd1 * 1000) / (bits.getTime() - beg);
            cmd.error(beg + " adds/sec");
            beg = bits.getTime();
            for (int i = 0; i < rnd2; i++) {
                tab1.route(adr);
            }
            beg = (rnd2 * 1000) / (bits.getTime() - beg);
            cmd.error(beg + " lookups/sec");
            return null;
        }
        if (a.equals("pipeline")) {
            cmd.error("performing test");
            pipeLine p = new pipeLine(65536, true);
            pipeSide p1 = p.getSide();
            pipeSide p2 = p.getSide();
            doTestPipe("conn", p1, p2, 1024);
            return null;
        }
        if (a.equals("digsig")) {
            boolean showKeys = cmd.word().equals("keys");
            cmd.error("performing test");
            final int keysiz = 512;
            cryKeyRSA krsa = new cryKeyRSA();
            cryKeyDSA kdsa = new cryKeyDSA();
            cryKeyECDSA kecdsa = new cryKeyECDSA();
            cryKeyDH kdh = new cryKeyDH();
            cryKeyECDH kecdh = new cryKeyECDH();
            final String init = "freedom4ever";
            byte[] buf = init.getBytes();
            krsa.keyMake(keysiz);
            buf = krsa.doEncrypt(buf);
            buf = krsa.doDecrypt(buf);
            cmd.error("rsa: " + krsa.keyVerify() + " " + krsa.keySize() + " " + !init.equals(new String(buf)));
            kdsa.keyMake(keysiz);
            kdsa.doSigning(buf);
            cmd.error("dsa: " + kdsa.keyVerify() + " " + kdsa.keySize() + " " + kdsa.doVerify(buf));
            kecdsa.keyMake(keysiz);
            kecdsa.doSigning(buf);
            cmd.error("ecdsa: " + kecdsa.keyVerify() + " " + kecdsa.keySize() + " " + kecdsa.doVerify(buf));
            kdh.keyMake(keysiz);
            kdh.clntXchg();
            kdh.servXchg();
            kdh.clntKey();
            kdh.servKey();
            cmd.error("dh: " + kdh.keyVerify() + " " + kdh.keySize());
            kecdh.keyMake(keysiz);
            kecdh.clntXchg();
            kecdh.servXchg();
            kecdh.clntKey();
            kecdh.servKey();
            cmd.error("ecdh: " + kecdh.keyVerify() + " " + kecdh.keySize());
            String sdsa = cryCertificate.createSelfSigned(kdsa, "test", 3650).pemWriteStr();
            String secdsa = cryCertificate.createSelfSigned(kecdsa, "test", 3650).pemWriteStr();
            String srsa = cryCertificate.createSelfSigned(krsa, "test", 3650).pemWriteStr();
            cryCertificate cdsa = new cryCertificate();
            cryCertificate cecdsa = new cryCertificate();
            cryCertificate crsa = new cryCertificate();
            cmd.error("pemio: " + cdsa.pemReadStr(sdsa) + " " + cecdsa.pemReadStr(secdsa) + " " + crsa.pemReadStr(srsa));
            cmd.error("cert: " + cryCertificate.testClientCert(cdsa, cdsa) + " " + cryCertificate.testClientCert(cecdsa, cecdsa) + " " + cryCertificate.testClientCert(crsa, crsa));
            cmd.error("dsa: " + cdsa);
            cmd.error("ecdsa: " + cecdsa);
            cmd.error("rsa: " + crsa);
            if (!showKeys) {
                return null;
            }
            cmd.error("dsa: " + kdsa.pemWriteStr(true) + " " + kdsa.pemWriteStr(false));
            cmd.error("ecdsa: " + kecdsa.pemWriteStr(true) + " " + kecdsa.pemWriteStr(false));
            cmd.error("rsa: " + krsa.pemWriteStr(true) + " " + krsa.pemWriteStr(false));
            cmd.error("dh: " + kdh.pemWriteStr(false));
            cmd.error("ecdh: " + kecdh.pemWriteStr(false));
            cmd.error("dcrt: " + cdsa.pemWriteStr());
            cmd.error("edcrt: " + cecdsa.pemWriteStr());
            cmd.error("rcrt: " + crsa.pemWriteStr());
            return null;
        }
        if (a.equals("crypto")) {
            doTestEncr(new cryEncrCBCaes());
            doTestEncr(new cryEncrCBCblowfish());
            doTestEncr(new cryEncrCBCdes3());
            doTestEncr(new cryEncrCBCdes());
            doTestEncr(new cryEncrCBCrc2());
            doTestHash(new cryHashMd2());
            doTestHash(new cryHashMd5());
            doTestHash(new cryHashSha1());
            doTestHash(new cryHashSha256());
            doTestHash(new cryHashSha512());
            doTestHash(new cryHashHmac(new cryHashMd2(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashMd5(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha1(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha256(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha512(), new byte[16]));
            return null;
        }
        if (a.equals("ssh")) {
            cmd.error("performing test");
            cryKeyRSA rsa = new cryKeyRSA();
            cryKeyDSA dss = new cryKeyDSA();
            cryKeyECDSA ecdss = new cryKeyECDSA();
            final int keysiz = 256;
            rsa.keyMake(keysiz);
            dss.keyMake(keysiz);
            ecdss.keyMake(keysiz);
            pipeLine conn = new pipeLine(65536, false);
            secSsh srvH = new secSsh(conn.getSide(), new pipeLine(65536, false));
            secSsh clnH = new secSsh(conn.getSide(), new pipeLine(65536, false));
            srvH.startServer(new authConstant(true), rsa, dss, ecdss);
            clnH.startClient("c", "c");
            doTestPipe("ssh", srvH.getPipe(), clnH.getPipe(), 1024);
            conn.setClose();
            return null;
        }
        if (a.equals("dtls")) {
            doTestTls(true, -1);
            doTestTls(true, 0x302);
            doTestTls(true, 0x303);
            return null;
        }
        if (a.equals("tls")) {
            cmd.error("performing test");
            doTestTls(false, -1);
            doTestTls(false, 0x300);
            doTestTls(false, 0x301);
            doTestTls(false, 0x302);
            doTestTls(false, 0x303);
            return null;
        }
        if (a.equals("screen")) {
            rdr.keyFlush();
            userScreenTest t = new userScreenTest(new userScreen(pip, rdr.width, rdr.height));
            t.doStart();
            t.doCommand(cmd);
            t.doFinish();
            rdr.keyFlush();
            return null;
        }
        if (a.equals("gomoku")) {
            rdr.keyFlush();
            userGomoku t = new userGomoku(new userScreen(pip, rdr.width, rdr.height));
            t.doStart();
            t.doGame();
            t.doFinish();
            rdr.keyFlush();
            return null;
        }
        if (a.equals("tetris")) {
            rdr.keyFlush();
            userTetris t = new userTetris(new userScreen(pip, rdr.width, rdr.height));
            t.doStart();
            t.doGame();
            t.doFinish();
            rdr.keyFlush();
            return null;
        }
        if (a.equals("minesweep")) {
            rdr.keyFlush();
            userMinesweep t = new userMinesweep(new userScreen(pip, rdr.width, rdr.height));
            t.doStart();
            t.doGame();
            t.doFinish();
            rdr.keyFlush();
            return null;
        }
        if (a.equals("verfile")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doRelease();
            return null;
        }
        if (a.equals("vercore")) {
            userUpgrade u = new userUpgrade(cmd);
            u.doVerCore();
            return null;
        }
        if (a.equals("hwdet")) {
            userHwdet h = new userHwdet();
            h.doer(cmd);
            return null;
        }
        if (a.equals("image")) {
            userImage i = new userImage();
            i.doer(cmd);
            return null;
        }
        if (a.equals("tester")) {
            userTester t = new userTester();
            t.doer(cmd);
            return null;
        }
        if (a.equals("template")) {
            userTemplate t = new userTemplate();
            t.doer(cmd);
            return null;
        }
        cmd.badCmd();
        return null;
    }

    private void doShow(List<String> l) {
        for (int i = 0; i < l.size(); i++) {
            cmd.error(l.get(i));
        }
    }

    private void doTestPipe(String tst, pipeSide p1, pipeSide p2, int bl) {
        cmd.pipe.strPut(tst);
        p1.setReady();
        p2.setReady();
        p1.wait4ready(0);
        p2.wait4ready(0);
        p1.timeout = 5000;
        p2.timeout = 5000;
        byte[] b1 = new byte[bl];
        byte[] b2 = new byte[b1.length];
        int goodT = 0;
        int goodR = 0;
        int tried = 0;
        long tim = bits.getTime();
        long cur;
        for (;;) {
            cur = bits.getTime();
            if ((cur - tim) > 5000) {
                break;
            }
            tried++;
            if (p1.blockingPut(b1, 0, b1.length) != b1.length) {
                continue;
            }
            goodT++;
            if (p2.moreGet(b2, 0, b2.length) != b2.length) {
                continue;
            }
            goodR++;
        }
        tim = cur - tim;
        finSpeedTest(tried + "/" + (tried - goodT) + "/" + (tried - goodR),
                b1.length, goodR, tim);
    }

    private void finSpeedTest(String beg, int blk, long rnd, long tim) {
        long xmit = rnd * blk;
        cmd.pipe.linePut(": " + beg + " rnd, " + xmit + " bytes, "
                + (tim / 1000) + " sec, " + ((rnd * 1000) / tim) + " pps, "
                + bits.bandwidth((xmit * 8000) / tim));
    }

    private void doTestTls(boolean dtls, int ver) {
        cryKeyRSA rsa = new cryKeyRSA();
        cryKeyDSA dss = new cryKeyDSA();
        cryKeyECDSA ecdss = new cryKeyECDSA();
        final int keysiz = 512;
        rsa.keyMake(keysiz);
        dss.keyMake(keysiz);
        ecdss.keyMake(keysiz);
        pipeLine conn = new pipeLine(65536, dtls);
        secTls srvH = new secTls(conn.getSide(), new pipeLine(65536, dtls), dtls);
        secTls clnH = new secTls(conn.getSide(), new pipeLine(65536, dtls), dtls);
        srvH.forcedVer = ver;
        clnH.forcedVer = ver;
        srvH.startServer(rsa, dss, ecdss, null, null, null);
        clnH.startClient();
        doTestPipe(packTls.version2string(dtls, ver), srvH.getPipe(), clnH.getPipe(), 1024);
        conn.setClose();
    }

    private void doTestEncr(cryEncrGeneric alg) {
        cmd.pipe.strPut(alg.getName());
        byte[] key = new byte[alg.getKeySize()];
        byte[] iv = new byte[alg.getBlockSize()];
        alg.init(key, iv, true);
        byte[] b1 = new byte[1024];
        int goodR = 0;
        long tim = bits.getTime();
        long cur;
        for (;;) {
            cur = bits.getTime();
            if ((cur - tim) > 3000) {
                break;
            }
            goodR++;
            alg.update(b1, 0, b1.length);
        }
        tim = cur - tim;
        finSpeedTest("" + goodR, b1.length, goodR, tim);
    }

    private void doTestHash(cryHashGeneric alg) {
        cmd.pipe.strPut(alg.getName());
        byte[] b1 = new byte[1024];
        int goodR = 0;
        long tim = bits.getTime();
        long cur;
        for (;;) {
            cur = bits.getTime();
            if ((cur - tim) > 3000) {
                break;
            }
            goodR++;
            alg.init();
            alg.update(b1);
            alg.finish();
        }
        tim = cur - tim;
        finSpeedTest("" + goodR, b1.length, goodR, tim);
    }

}
