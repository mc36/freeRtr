package user;

import addr.addrClns;
import addr.addrEui;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrIpx;
import addr.addrIsis;
import addr.addrMac;
import addr.addrPrefix;
import addr.addrType;
import auth.authConstant;
import auth.authLocal;
import cfg.cfgAceslst;
import cfg.cfgAlias;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgInit;
import cfg.cfgSensor;
import cfg.cfgTrnsltn;
import cry.cryAsn1;
import cry.cryBase64;
import cry.cryCertificate;
import cry.cryEncrCBCaes;
import cry.cryEncrCBCblowfish;
import cry.cryEncrCBCdes;
import cry.cryEncrCBCdes3;
import cry.cryEncrCBCrc2;
import cry.cryEncrChacha20;
import cry.cryEncrGeneric;
import cry.cryEncrRc4;
import cry.cryHashBlake2s;
import cry.cryHashCrc16;
import cry.cryHashCrc32;
import cry.cryHashCrc32c;
import cry.cryHashFcs16;
import cry.cryHashGeneric;
import cry.cryHashHec8;
import cry.cryHashHmac;
import cry.cryHashMd2;
import cry.cryHashMd5;
import cry.cryHashSha1;
import cry.cryHashSha2224;
import cry.cryHashSha2256;
import cry.cryHashSha2384;
import cry.cryHashSha2512;
import cry.cryHashSha3224;
import cry.cryHashSha3256;
import cry.cryHashSha3384;
import cry.cryHashSha3512;
import cry.cryKeyDH;
import cry.cryKeyDSA;
import cry.cryKeyECDH;
import cry.cryKeyECDSA;
import cry.cryKeyRSA;
import cry.cryOtp;
import java.util.ArrayList;
import java.util.List;
import pack.packDnsRec;
import pack.packDnsZone;
import pack.packHolder;
import pack.packTls;
import pipe.pipeLine;
import pipe.pipeSide;
import pipe.pipeWindow;
import sec.secSsh;
import sec.secTls;
import tab.tabAceslstN;
import tab.tabListing;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.extMrkLng;
import util.jasOn;
import util.logger;
import util.protoBuf;
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
        if (a.equals("yangconfig")) {
            String src = cmd.word();
            cmd.error("reading " + src);
            List<String> l = bits.txt2buf(src);
            if (l == null) {
                cmd.error("error reading");
                return null;
            }
            String trg = cmd.word();
            l = userNetconf.makeYang(l, 1, l.size() - 1);
            a = trg + src + ".yang";
            cmd.error("writing " + a);
            if (bits.buf2txt(true, l, a)) {
                cmd.error("error writing");
                return null;
            }
            cmd.error("done");
            return null;
        }
        if (a.equals("yangsensor")) {
            String src = cmd.word();
            cmd.error("reading " + src);
            List<String> l = bits.txt2buf(src);
            if (l == null) {
                cmd.error("error reading");
                return null;
            }
            String trg = cmd.word();
            int pos;
            for (pos = 0; pos < l.size(); pos++) {
                a = l.get(pos);
                if (!a.startsWith("sensor ")) {
                    continue;
                }
                pos++;
                int i = a.indexOf(" ");
                cfgSensor tl = new cfgSensor(a.substring(i, a.length()).trim());
                for (; pos < l.size(); pos++) {
                    a = l.get(pos);
                    if (a.equals(".")) {
                        break;
                    }
                    tl.doCfgStr(new cmds("tl", a));
                }
                List<String> res = tl.getYang();
                a = trg + src + "-" + tl.name + ".yang";
                cmd.error("writing " + a);
                if (bits.buf2txt(true, res, a)) {
                    cmd.error("error writing");
                    return null;
                }
            }
            cmd.error("done");
            return null;
        }
        if (a.equals("acl")) {
            cfgAceslst cfg1 = cfgAll.aclsFind(cmd.word(), false);
            cfgAceslst cfg2 = cfgAll.aclsFind(cmd.word(), false);
            tabListing<tabAceslstN<addrIP>, addrIP> acl1 = null;
            tabListing<tabAceslstN<addrIP>, addrIP> acl2 = null;
            if (cfg1 != null) {
                acl1 = tabAceslstN.unrollAcl(cfg1.aceslst);
            }
            if (cfg2 != null) {
                acl2 = tabAceslstN.unrollAcl(cfg2.aceslst);
            }
            tabListing<tabAceslstN<addrIP>, addrIP> res = new tabListing<tabAceslstN<addrIP>, addrIP>();
            res.mergeTwo(acl1, acl2);
            List<String> lst = res.dump("");
            for (int i = 0; i < lst.size(); i++) {
                cmd.pipe.linePut(lst.get(i));
            }
            return null;
        }
        if (a.equals("dns")) {
            a = cmd.word();
            packDnsZone zon = new packDnsZone(a);
            for (int i = 0; i < cfgAll.ifaces.size(); i++) {
                cfgIfc ifc = cfgAll.ifaces.get(i);
                if (ifc.cloned != null) {
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
        if (a.equals("gc")) {
            System.gc();
            return null;
        }
        if (a.equals("window")) {
            int x = bits.str2num(cmd.word());
            int y = bits.str2num(cmd.word());
            if (x < 1) {
                x = 80;
            }
            if (y < 1) {
                y = 25;
            }
            pipeSide pipWin = pipeWindow.createOne(x, y, userFonts1.fontDefault(), userFonts1.colorData);
            if (pipWin == null) {
                cmd.error("failed");
                return null;
            }
            userLine lin = new userLine();
            lin.execTimeOut = 0;
            lin.execWidth = x - 1;
            lin.execHeight = y - 1;
            lin.createHandler(pipWin, "window", 2);
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
        if (a.equals("json")) {
            jasOn json = jasOn.parseOne(cmd.getRemaining());
            cmd.error("orig: " + json.orig);
            cmd.error("done: " + json.toJSONstr());
            doShow(json.show());
            return null;
        }
        if (a.equals("protobuf")) {
            byte[] buf = cryAsn1.hex2bytes(cmd);
            packHolder pck = new packHolder(true, true);
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            pck.merge2beg();
            cmd.error("orig: " + pck.dump());
            protoBuf pb = protoBuf.parseOne(pck);
            cmd.error("done: " + pb.toPacket().dump());
            doShow(pb.show());
            return null;
        }
        if (a.equals("url")) {
            uniResLoc url = uniResLoc.parseOne(cmd.getRemaining());
            doShow(url.show());
            return null;
        }
        if (a.equals("prefix")) {
            a = cmd.word();
            addrPrefix<addrIP> prf = addrPrefix.str2ip(a);
            if (prf == null) {
                cmd.error("bad prefix");
                return null;
            }
            cmd.error("  network=" + prf.network);
            cmd.error("broadcast=" + prf.broadcast);
            cmd.error("  netmask=" + prf.mask);
            cmd.error("  masklen=" + prf.maskLen);
            cmd.error(" wildcard=" + prf.wildcard);
            cmd.error("   string=" + addrPrefix.ip2str(prf));
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
        if (a.equals("translation")) {
            cfgTrnsltn t = cfgAll.trnsltnFind(cmd.word(), false);
            if (t == null) {
                return null;
            }
            a = cmd.getRemaining();
            cmd.error("  original=" + a);
            String s = t.doTranslate(a);
            cmd.error("translated=" + s);
            return null;
        }
        if (a.equals("logging")) {
            a = cmd.word();
            if (a.equals("traceback")) {
                logger.traceback(new Exception(cmd.getRemaining()));
                return null;
            }
            logger.logLev i = logger.string2level(a);
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
            int add = 100000;
            int fnd = 10000;
            int idx = 111;
            boolean dmp = false;
            int msk = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("dump")) {
                    dmp = true;
                    continue;
                }
                if (a.equals("fwd")) {
                    msk |= 1;
                    continue;
                }
                if (a.equals("bwd")) {
                    msk |= 2;
                    continue;
                }
                if (a.equals("rnd")) {
                    msk |= 4;
                    continue;
                }
                if (a.equals("raw")) {
                    msk |= 8;
                    continue;
                }
                if (a.equals("opt")) {
                    msk |= 16;
                    continue;
                }
                if (a.equals("all")) {
                    msk |= 0xfff;
                    continue;
                }
                if (a.equals("find")) {
                    fnd = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("add")) {
                    add = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("idx")) {
                    idx = bits.str2num(cmd.word());
                    continue;
                }
            }
            doTestRouting(msk, new addrClns(), new tabRoute<addrClns>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrIsis(), new tabRoute<addrIsis>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrEui(), new tabRoute<addrEui>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrMac(), new tabRoute<addrMac>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrIpx(), new tabRoute<addrIpx>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrIPv4(), new tabRoute<addrIPv4>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrIPv6(), new tabRoute<addrIPv6>("test"), add, fnd, idx, dmp);
            doTestRouting(msk, new addrIP(), new tabRoute<addrIP>("test"), add, fnd, idx, dmp);
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
            int keysiz = 384;
            boolean showKeys = false;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("keys")) {
                    showKeys = true;
                    continue;
                }
                if (a.equals("len")) {
                    keysiz = bits.str2num(cmd.word());
                    continue;
                }
            }
            cmd.error("performing test");
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
            if (showKeys) {
                cmd.error("rsa: " + krsa.pemWriteStr(true) + " " + krsa.pemWriteStr(false));
            }
            kdsa.keyMake(keysiz);
            kdsa.doSigning(buf);
            cmd.error("dsa: " + kdsa.keyVerify() + " " + kdsa.keySize() + " " + kdsa.doVerify(buf));
            if (showKeys) {
                cmd.error("dsa: " + kdsa.pemWriteStr(true) + " " + kdsa.pemWriteStr(false));
            }
            kecdsa.keyMake(keysiz);
            kecdsa.doSigning(buf);
            cmd.error("ecdsa: " + kecdsa.keyVerify() + " " + kecdsa.keySize() + " " + kecdsa.doVerify(buf));
            if (showKeys) {
                cmd.error("ecdsa: " + kecdsa.pemWriteStr(true) + " " + kecdsa.pemWriteStr(false));
            }
            kdh.keyMake(keysiz);
            kdh.clntXchg();
            kdh.servXchg();
            kdh.clntKey();
            kdh.servKey();
            cmd.error("dh: " + kdh.keyVerify() + " " + kdh.keySize());
            if (showKeys) {
                cmd.error("dh: " + kdh.pemWriteStr(false));
            }
            kecdh.keyMake(keysiz);
            kecdh.clntXchg();
            kecdh.servXchg();
            kecdh.clntKey();
            kecdh.servKey();
            cmd.error("ecdh: " + kecdh.keyVerify() + " " + kecdh.keySize());
            if (showKeys) {
                cmd.error("ecdh: " + kecdh.pemWriteStr(false));
            }
            String sdsa = cryCertificate.createSelfSigned(kdsa, "test", 3650).pemWriteStr();
            String secdsa = cryCertificate.createSelfSigned(kecdsa, "test", 3650).pemWriteStr();
            String srsa = cryCertificate.createSelfSigned(krsa, "test", 3650).pemWriteStr();
            cryCertificate cdsa = new cryCertificate();
            cryCertificate cecdsa = new cryCertificate();
            cryCertificate crsa = new cryCertificate();
            cmd.error("pemio: " + cdsa.pemReadStr(sdsa) + " " + cecdsa.pemReadStr(secdsa) + " " + crsa.pemReadStr(srsa));
            cmd.error("cert: " + cryCertificate.testClientCert(cdsa, cdsa) + " " + cryCertificate.testClientCert(cecdsa, cecdsa) + " " + cryCertificate.testClientCert(crsa, crsa));
            if (showKeys) {
                cmd.error("dcrt: " + cdsa.pemWriteStr());
                cmd.error("edcrt: " + cecdsa.pemWriteStr());
                cmd.error("rcrt: " + crsa.pemWriteStr());
            }
            cmd.error("dsa: " + cdsa);
            cmd.error("ecdsa: " + cecdsa);
            cmd.error("rsa: " + crsa);
            return null;
        }
        if (a.equals("crypto")) {
            doTestEncr(new cryEncrCBCaes());
            doTestEncr(new cryEncrCBCblowfish());
            doTestEncr(new cryEncrCBCdes3());
            doTestEncr(new cryEncrCBCdes());
            doTestEncr(new cryEncrCBCrc2());
            doTestEncr(new cryEncrRc4());
            doTestEncr(new cryEncrChacha20());
            doTestHash(new cryHashHec8());
            doTestHash(new cryHashFcs16());
            doTestHash(new cryHashCrc16());
            doTestHash(new cryHashCrc32());
            doTestHash(new cryHashCrc32c());
            doTestHash(new cryHashMd2());
            doTestHash(new cryHashMd5());
            doTestHash(new cryHashSha1());
            doTestHash(new cryHashBlake2s(null, 32));
            doTestHash(new cryHashSha2224());
            doTestHash(new cryHashSha2256());
            doTestHash(new cryHashSha2384());
            doTestHash(new cryHashSha2512());
            doTestHash(new cryHashSha3224());
            doTestHash(new cryHashSha3256());
            doTestHash(new cryHashSha3384());
            doTestHash(new cryHashSha3512());
            doTestHash(new cryHashHmac(new cryHashMd2(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashMd5(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha1(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashBlake2s(null, 32), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha2224(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha2256(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha2384(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha2512(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha3224(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha3256(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha3384(), new byte[16]));
            doTestHash(new cryHashHmac(new cryHashSha3512(), new byte[16]));
            return null;
        }
        if (a.equals("ssh")) {
            cmd.error("performing test");
            cryKeyRSA rsa = new cryKeyRSA();
            cryKeyDSA dss = new cryKeyDSA();
            cryKeyECDSA ecdss = new cryKeyECDSA();
            final int keysiz = 512;
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
        if (a.equals("hwcfg")) {
            cfgInit.executeHWcommands(bits.str2lst(cmd.getRemaining()), new ArrayList<String>(), new ArrayList<String>());
            return null;
        }
        if (a.equals("hwext")) {
            userHwext h = new userHwext();
            h.doer(cmd);
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

    private <T extends addrType> void doTestRoutingChk(tabRoute<T> tab, boolean dmp) {
        int i = tab.checkConsistency();
        if (i >= 0) {
            cmd.pipe.strPut(",err#" + i + " ");
        }
        if (!dmp) {
            return;
        }
        cmd.pipe.strPut("," + tab.getTableInfo() + "  ");
    }

    private <T extends addrType> void doTestRoutingGet(tabRoute<T> tab, int idx, int add, boolean dmp, String typ) {
        for (int i = 0; i < tab.size(); i++) {
            tab.get(i).best.time = i;
        }
        long beg = bits.getTime();
        long rnd = 0;
        for (int cur = 0; cur < (add / tab.size()); cur++) {
            for (int i = 0; i < tab.size(); i++) {
                tab.get(i).best.time = rnd;
                rnd++;
            }
        }
        beg = (rnd * 1000) / (bits.getTime() - beg);
        cmd.pipe.strPut("  " + beg + " " + typ + "g");
        int siz = tab.size();
        tabRouteEntry<T> rou = tab.get(idx);
        addrPrefix<T> prf = null;
        T adr = null;
        if (rou != null) {
            prf = rou.prefix;
            adr = prf.network;
        }
        if (dmp) {
            cmd.pipe.strPut("  rou=" + rou + ",prf=" + prf + ",adr=" + adr);
        }
        beg = bits.getTime();
        rnd = 0;
        for (int cur = 0; cur < (add / tab.size()); cur++) {
            for (int i = 0; i < tab.size(); i++) {
                if (prf == null) {
                    tab.find(tab.get(bits.random(0, siz))).best.time = rnd;
                } else {
                    tab.find(prf).best.time = rnd;
                }
                rnd++;
            }
        }
        beg = (rnd * 1000) / (bits.getTime() - beg);
        cmd.pipe.strPut("  " + beg + " " + typ + "f");
        beg = bits.getTime();
        rnd = 0;
        for (int cur = 0; cur < (add / tab.size()); cur++) {
            for (int i = 0; i < tab.size(); i++) {
                if (adr == null) {
                    tab.route(tab.get(bits.random(0, siz)).prefix.network).best.time = rnd;
                } else {
                    tab.route(adr).best.time = rnd;
                }
                rnd++;
            }
        }
        beg = (rnd * 1000) / (bits.getTime() - beg);
        cmd.pipe.strPut("  " + beg + " " + typ + "l");
    }

    private <T extends addrType> void doTestRoutingAdd(tabRoute<T> tab, T adr, int fill, int add, userTestIfc mod, boolean dmp, String typ) {
        tab.clear();
        byte[] buf = new byte[256];
        long beg = bits.getTime();
        for (int i = 0; i < add; i++) {
            bits.msbPutD(buf, 0, mod.forAdd(i));
            adr.fromBuf(buf, 0);
            tab.add(tabRoute.addType.always, new addrPrefix<T>(adr, fill), adr);
        }
        beg = (add * 1000) / (bits.getTime() - beg);
        cmd.pipe.strPut("  " + beg + " " + typ + "a");
        doTestRoutingChk(tab, dmp);
        beg = bits.getTime();
        add = tab.size();
        for (int i = add - 1; i >= 0; i--) {
            tab.del(tab.get(mod.forDel(i + 1)));
        }
        beg = (add * 1000) / (bits.getTime() - beg);
        cmd.pipe.strPut("  " + beg + " " + typ + "d");
        doTestRoutingChk(tab, dmp);
    }

    private <T extends addrType> void doTestRouting(int msk, T adr, tabRoute<T> tab, int add, int fnd, int idx, boolean dmp) {
        int fill = adr.getSize() * 8;
        cmd.pipe.strPut("testing " + adr.getClass().getName() + "-" + fill + ":");
        byte[] buf = new byte[256];
        if ((msk & 1) != 0) {
            doTestRoutingAdd(tab, adr, fill, add, new userTestFwd(), dmp, "f");
        }
        if ((msk & 2) != 0) {
            doTestRoutingAdd(tab, adr, fill, add, new userTestBwd(), dmp, "b");
        }
        if ((msk & 4) != 0) {
            doTestRoutingAdd(tab, adr, fill, add, new userTestRnd(), dmp, "r");
        }
        if ((msk & 24) == 0) {
            cmd.pipe.linePut("");
            return;
        }
        tab.clear();
        fill = fill - 8;
        for (int i = 0; i < fnd; i++) {
            bits.msbPutD(buf, 0, i << 8);
            adr.fromBuf(buf, 0);
            tab.add(tabRoute.addType.always, new addrPrefix<T>(adr, fill), adr);
        }
        if ((msk & 8) != 0) {
            doTestRoutingChk(tab, dmp);
            doTestRoutingGet(tab, idx, add, dmp, "r");
            doTestRoutingChk(tab, dmp);
        }
        tab.optimize4lookup();
        if ((msk & 16) != 0) {
            doTestRoutingChk(tab, dmp);
            doTestRoutingGet(tab, idx, add, dmp, "o");
            doTestRoutingChk(tab, dmp);
        }
        cmd.pipe.linePut("");
    }

    private void doTestPipe(String tst, pipeSide p1, pipeSide p2, int bl) {
        cmd.pipe.strPut(tst);
        p1.setReady();
        p2.setReady();
        p1.setTime(5000);
        p2.setTime(5000);
        p1.wait4ready(5000);
        p2.wait4ready(5000);
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
            for (int i = 0; i < 3000; i++) {
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

interface userTestIfc {

    public int forAdd(int rnd);

    public int forDel(int len);

}

class userTestFwd implements userTestIfc {

    public int forAdd(int rnd) {
        return rnd;
    }

    public int forDel(int len) {
        return len - 1;
    }

}

class userTestBwd implements userTestIfc {

    public int forAdd(int rnd) {
        return -rnd;
    }

    public int forDel(int len) {
        return 0;
    }

}

class userTestRnd implements userTestIfc {

    public int forAdd(int rnd) {
        return bits.randomD();
    }

    public int forDel(int len) {
        return bits.random(0, len);
    }

}
