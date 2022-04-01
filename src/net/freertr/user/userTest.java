package net.freertr.user;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrClns;
import net.freertr.addr.addrEui;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrIpx;
import net.freertr.addr.addrIsis;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.auth.authConstant;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgSensor;
import net.freertr.cfg.cfgTrnsltn;
import net.freertr.cry.cryAsn1;
import net.freertr.cry.cryBase64;
import net.freertr.cry.cryCertificate;
import net.freertr.cry.cryEncrCBCaes;
import net.freertr.cry.cryEncrCBCblowfish;
import net.freertr.cry.cryEncrCBCdes;
import net.freertr.cry.cryEncrCBCdes3;
import net.freertr.cry.cryEncrCBCrc2;
import net.freertr.cry.cryEncrCFBaes;
import net.freertr.cry.cryEncrCFBblowfish;
import net.freertr.cry.cryEncrCFBdes;
import net.freertr.cry.cryEncrCFBdes3;
import net.freertr.cry.cryEncrCFBrc2;
import net.freertr.cry.cryEncrCTRaes;
import net.freertr.cry.cryEncrCTRblowfish;
import net.freertr.cry.cryEncrCTRdes;
import net.freertr.cry.cryEncrCTRdes3;
import net.freertr.cry.cryEncrCTRrc2;
import net.freertr.cry.cryEncrCTSaes;
import net.freertr.cry.cryEncrCTSblowfish;
import net.freertr.cry.cryEncrCTSdes;
import net.freertr.cry.cryEncrCTSdes3;
import net.freertr.cry.cryEncrCTSrc2;
import net.freertr.cry.cryEncrChacha20;
import net.freertr.cry.cryEncrECBaes;
import net.freertr.cry.cryEncrECBblowfish;
import net.freertr.cry.cryEncrECBdes;
import net.freertr.cry.cryEncrECBdes3;
import net.freertr.cry.cryEncrECBrc2;
import net.freertr.cry.cryEncrGCMaes;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryEncrNone;
import net.freertr.cry.cryEncrOFBaes;
import net.freertr.cry.cryEncrOFBblowfish;
import net.freertr.cry.cryEncrOFBdes;
import net.freertr.cry.cryEncrOFBdes3;
import net.freertr.cry.cryEncrOFBrc2;
import net.freertr.cry.cryEncrRc4;
import net.freertr.cry.cryHashBlake2s;
import net.freertr.cry.cryHashCrc16;
import net.freertr.cry.cryHashCrc32;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashCrc8;
import net.freertr.cry.cryHashFcs16;
import net.freertr.cry.cryHashHmac;
import net.freertr.cry.cryHashMd2;
import net.freertr.cry.cryHashMd5;
import net.freertr.cry.cryHashNone;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2224;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2384;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryHashSha3224;
import net.freertr.cry.cryHashSha3256;
import net.freertr.cry.cryHashSha3384;
import net.freertr.cry.cryHashSha3512;
import net.freertr.cry.cryKeyDH;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyECDH;
import net.freertr.cry.cryKeyECDSA;
import net.freertr.cry.cryKeyRSA;
import net.freertr.cry.cryOtp;
import net.freertr.pack.packDnsRec;
import net.freertr.pack.packDnsRes;
import net.freertr.pack.packDnsZone;
import net.freertr.pack.packHolder;
import net.freertr.pack.packTls;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.pipe.pipeWindow;
import net.freertr.sec.secSsh;
import net.freertr.sec.secTls;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.extMrkLng;
import net.freertr.util.jasOn;
import net.freertr.util.logger;
import net.freertr.util.protoBuf;
import net.freertr.util.uniResLoc;

/**
 * process test commands
 *
 * @author matecsaba
 */
public class userTest {

    /**
     * create instance
     */
    public userTest() {
    }

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
    public cfgAlias doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.test, false);
        if (alias != null) {
            return alias;
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
                    packDnsRes res = new packDnsRes();
                    rec.clss = packDnsRec.classIN;
                    rec.name = ifc.name + "." + a;
                    res.addr = new addrIP();
                    res.addr.fromIPv4addr(ifc.addr4);
                    rec.res.add(res);
                    rec.typ = packDnsRec.typeA;
                    zon.addBin(rec);
                }
                if (ifc.addr6 != null) {
                    packDnsRec rec = new packDnsRec();
                    packDnsRes res = new packDnsRes();
                    rec.clss = packDnsRec.classIN;
                    rec.name = ifc.name + "." + a;
                    res.addr = new addrIP();
                    res.addr.fromIPv6addr(ifc.addr6);
                    rec.res.add(res);
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
            lin.createHandler(pipWin, "window", 2);
            return null;
        }
        if (a.equals("otppass")) {
            byte[] buf = cmd.word().getBytes();
            long t = (bits.getTime() + cfgAll.timeServerOffset) / 1000;
            a = cryOtp.calcTotp(buf, t, cryOtp.timeInt, 8, new cryHashSha1());
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
            a = authLocal.passwdEncode(a, false);
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
            int pmsiz = 1024;
            int ecsiz = 128;
            int times = 1;
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
                    pmsiz = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("eclen")) {
                    ecsiz = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("times")) {
                    times = bits.str2num(cmd.word());
                    continue;
                }
            }
            cmd.error("performing test");
            cryKeyRSA krsa = new cryKeyRSA();
            cryKeyDSA kdsa = new cryKeyDSA();
            cryKeyECDSA kecdsa = new cryKeyECDSA();
            cryKeyDH kdh = new cryKeyDH();
            cryKeyECDH kecdh = new cryKeyECDH();
            final String init = "tesging";
            boolean ok = false;
            krsa.keyMake(pmsiz);
            long tim = bits.getTime();
            for (int i = 0; i < times; i++) {
                byte[] buf = init.getBytes();
                buf = krsa.doEncrypt(buf);
                buf = krsa.doDecrypt(buf);
                ok |= !init.equals(new String(buf));
            }
            cmd.error("rsa: " + krsa.keyVerify() + " " + krsa.keySize() + " " + ok + " in " + (bits.getTime() - tim) + "ms");
            if (showKeys) {
                cmd.error("rsa: " + krsa.pemWriteStr(true) + " " + krsa.pemWriteStr(false));
            }
            kdsa.keyMake(pmsiz);
            ok = false;
            tim = bits.getTime();
            for (int i = 0; i < times; i++) {
                byte[] buf = init.getBytes();
                kdsa.doSigning(buf);
                ok |= kdsa.doVerify(buf);
            }
            cmd.error("dsa: " + kdsa.keyVerify() + " " + kdsa.keySize() + " " + ok + " in " + (bits.getTime() - tim) + "ms");
            if (showKeys) {
                cmd.error("dsa: " + kdsa.pemWriteStr(true) + " " + kdsa.pemWriteStr(false));
            }
            kecdsa.keyMake(ecsiz);
            ok = false;
            tim = bits.getTime();
            for (int i = 0; i < times; i++) {
                byte[] buf = init.getBytes();
                kecdsa.doSigning(buf);
                ok |= kecdsa.doVerify(buf);
            }
            cmd.error("ecdsa: " + kecdsa.keyVerify() + " " + kecdsa.keySize() + " " + ok + " in " + (bits.getTime() - tim) + "ms");
            if (showKeys) {
                cmd.error("ecdsa: " + kecdsa.pemWriteStr(true) + " " + kecdsa.pemWriteStr(false));
            }
            kdh = cryKeyDH.findGroup(pmsiz);
            tim = bits.getTime();
            for (int i = 0; i < times; i++) {
                kdh.clntXchg();
                kdh.servXchg();
                kdh.clntKey();
                kdh.servKey();
            }
            cmd.error("dh: " + kdh.keyVerify() + " " + kdh.keySize() + " in " + (bits.getTime() - tim) + "ms");
            if (showKeys) {
                cmd.error("dh: " + kdh.pemWriteStr(false));
            }
            kecdh.keyMake(ecsiz);
            tim = bits.getTime();
            for (int i = 0; i < times; i++) {
                kecdh.clntXchg();
                kecdh.servXchg();
                kecdh.clntKey();
                kecdh.servKey();
            }
            cmd.error("ecdh: " + kecdh.keyVerify() + " " + kecdh.keySize() + " in " + (bits.getTime() - tim) + "ms");
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
            doTestEncr(new cryEncrCBCaes(), 128, "128cbc");
            doTestEncr(new cryEncrCBCaes(), 192, "192cbc");
            doTestEncr(new cryEncrCBCaes(), 256, "256cbc");
            doTestEncr(new cryEncrCBCblowfish(), 0, "-cbc");
            doTestEncr(new cryEncrCBCdes3(), 0, "-cbc");
            doTestEncr(new cryEncrCBCdes(), 0, "-cbc");
            doTestEncr(new cryEncrCBCrc2(), 0, "-cbc");
            doTestEncr(new cryEncrCFBaes(), 128, "128cfb");
            doTestEncr(new cryEncrCFBaes(), 192, "192cfb");
            doTestEncr(new cryEncrCFBaes(), 256, "256cfb");
            doTestEncr(new cryEncrCFBblowfish(), 0, "-cfb");
            doTestEncr(new cryEncrCFBdes3(), 0, "-cfb");
            doTestEncr(new cryEncrCFBdes(), 0, "-cfb");
            doTestEncr(new cryEncrCFBrc2(), 0, "-cfb");
            doTestEncr(new cryEncrCTRaes(), 128, "128ctr");
            doTestEncr(new cryEncrCTRaes(), 192, "192ctr");
            doTestEncr(new cryEncrCTRaes(), 256, "256ctr");
            doTestEncr(new cryEncrCTRblowfish(), 0, "-ctr");
            doTestEncr(new cryEncrCTRdes3(), 0, "-ctr");
            doTestEncr(new cryEncrCTRdes(), 0, "-ctr");
            doTestEncr(new cryEncrCTRrc2(), 0, "-ctr");
            doTestEncr(new cryEncrCTSaes(), 128, "128cts");
            doTestEncr(new cryEncrCTSaes(), 192, "192cts");
            doTestEncr(new cryEncrCTSaes(), 256, "256cts");
            doTestEncr(new cryEncrCTSblowfish(), 0, "-cts");
            doTestEncr(new cryEncrCTSdes3(), 0, "-cts");
            doTestEncr(new cryEncrCTSdes(), 0, "-cts");
            doTestEncr(new cryEncrCTSrc2(), 0, "-cts");
            doTestEncr(new cryEncrECBaes(), 128, "128ecb");
            doTestEncr(new cryEncrECBaes(), 192, "192ecb");
            doTestEncr(new cryEncrECBaes(), 256, "256ecb");
            doTestEncr(new cryEncrECBblowfish(), 0, "-ecb");
            doTestEncr(new cryEncrECBdes3(), 0, "-ecb");
            doTestEncr(new cryEncrECBdes(), 0, "-ecb");
            doTestEncr(new cryEncrECBrc2(), 0, "-ecb");
            doTestEncr(new cryEncrOFBaes(), 128, "128ofb");
            doTestEncr(new cryEncrOFBaes(), 192, "192ofb");
            doTestEncr(new cryEncrOFBaes(), 256, "256ofb");
            doTestEncr(new cryEncrOFBblowfish(), 0, "-ofb");
            doTestEncr(new cryEncrOFBdes3(), 0, "-ofb");
            doTestEncr(new cryEncrOFBdes(), 0, "-ofb");
            doTestEncr(new cryEncrOFBrc2(), 0, "-ofb");
            doTestEncr(new cryEncrGCMaes(), 128, "128gcm");
            doTestEncr(new cryEncrGCMaes(), 192, "192gcm");
            doTestEncr(new cryEncrGCMaes(), 256, "256gcm");
            doTestEncr(new cryEncrRc4(), 0, "");
            doTestEncr(new cryEncrChacha20(), 0, "");
            doTestEncr(new cryEncrNone(), 0, "");
            doTestHash(new cryHashCrc8(cryHashCrc8.polyCrc8c));
            doTestHash(new cryHashFcs16());
            doTestHash(new cryHashCrc16(cryHashCrc16.polyCrc16c));
            doTestHash(new cryHashCrc32(cryHashCrc32.polyCrc32c));
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
            doTestHash(new cryHashNone());
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
            rsa.keyMake(1024);
            dss.keyMake(512);
            ecdss.keyMake(128);
            pipeLine conn = new pipeLine(65536, false);
            secSsh srvH = new secSsh(conn.getSide(), new pipeLine(65536, false));
            secSsh clnH = new secSsh(conn.getSide(), new pipeLine(65536, false));
            srvH.startServer(new authConstant(true), rsa, dss, ecdss);
            clnH.startClient(null, "c", "c");
            doTestPipe("ssh", srvH.getPipe(), clnH.getPipe(), 1024);
            conn.setClose();
            return null;
        }
        if (a.equals("dtls")) {
            a = cmd.word();
            if (a.length() > 0) {
                String s = cmd.word();
                doTestTls(true, bits.str2num(a), bits.str2num(s));
                return null;
            }
            doTestTls(true, 0, 4);
            doTestTls(true, 3, 3);
            doTestTls(true, 4, 4);
            return null;
        }
        if (a.equals("tls")) {
            a = cmd.word();
            if (a.length() > 0) {
                String s = cmd.word();
                doTestTls(false, bits.str2num(a), bits.str2num(s));
                return null;
            }
            doTestTls(false, 0, 4);
            doTestTls(false, 0, 0);
            doTestTls(false, 1, 1);
            doTestTls(false, 2, 2);
            doTestTls(false, 3, 3);
            doTestTls(false, 4, 4);
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
            cfgInit.executeHWcommands(bits.str2lst(cmd.getRemaining()), new ArrayList<String>(), new ArrayList<String>(), new ArrayList<String>());
            return null;
        }
        if (a.equals("hwext")) {
            userHwext h = new userHwext();
            h.doer(cmd);
            return null;
        }
        if (a.equals("hwpop")) {
            userHwpop h = new userHwpop();
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
            t.doTesting(cmd);
            return null;
        }
        if (a.equals("changelog")) {
            userTester t = new userTester();
            t.doChanges(cmd);
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
        cmd.pipe.strPut("," + tab.tableInfo() + "  ");
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

    private void doTestTls(boolean dtls, int min, int max) {
        cryKeyRSA rsa = new cryKeyRSA();
        cryKeyDSA dss = new cryKeyDSA();
        cryKeyECDSA ecdss = new cryKeyECDSA();
        rsa.keyMake(1024);
        dss.keyMake(512);
        ecdss.keyMake(128);
        pipeLine conn = new pipeLine(65536, dtls);
        secTls srvH = new secTls(conn.getSide(), new pipeLine(65536, dtls), dtls);
        secTls clnH = new secTls(conn.getSide(), new pipeLine(65536, dtls), dtls);
        srvH.minVer = 0x300 + min;
        srvH.maxVer = 0x300 + max;
        clnH.minVer = srvH.minVer;
        clnH.maxVer = srvH.maxVer;
        srvH.startServer(rsa, dss, ecdss, null, null, null);
        clnH.startClient(null);
        pipeSide pip = srvH.getPipe();
        pip.wait4ready(5000);
        doTestPipe(packTls.version2string(dtls, srvH.minVer), pip, clnH.getPipe(), 1024);
        conn.setClose();
    }

    private void doTestEncr(cryEncrGeneric alg, int kes, String add) {
        cmd.pipe.strPut(alg.getName() + add);
        kes /= 8;
        if (kes < 1) {
            kes = alg.getKeySize();
        }
        byte[] key = new byte[kes];
        byte[] iv = new byte[alg.getBlockSize()];
        alg.init(key, iv, true);
        byte[] b1 = new byte[1280 + 256];
        int goodR = 0;
        long tim = bits.getTime();
        long cur;
        for (;;) {
            cur = bits.getTime();
            if ((cur - tim) > 3000) {
                break;
            }
            goodR++;
            alg.update(b1, 0, b1.length - 256);
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
