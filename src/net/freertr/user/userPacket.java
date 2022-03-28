package net.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authResult;
import net.freertr.cfg.cfgAlias;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgAuther;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgPlymp;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntModem;
import net.freertr.clnt.clntNrpe;
import net.freertr.clnt.clntNtp;
import net.freertr.clnt.clntPcep;
import net.freertr.clnt.clntProxy;
import net.freertr.clnt.clntSmtp;
import net.freertr.clnt.clntSnmp;
import net.freertr.clnt.clntSpeed;
import net.freertr.clnt.clntVconf;
import net.freertr.clnt.clntVoice;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcEther;
import net.freertr.ip.ipCor;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipIfc4;
import net.freertr.ip.ipIfc6;
import net.freertr.pack.packHolder;
import net.freertr.pack.packNrpe;
import net.freertr.pack.packWol;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSide;
import net.freertr.pipe.pipeTerm;
import net.freertr.prt.prtDccp;
import net.freertr.prt.prtLudp;
import net.freertr.prt.prtSctp;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.rtr.rtrBgp;
import net.freertr.rtr.rtrBgpMon;
import net.freertr.rtr.rtrBgpMrt;
import net.freertr.rtr.rtrBgpNeigh;
import net.freertr.rtr.rtrBgpSpeak;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.sec.secWebsock;
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabHop;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.uniResLoc;

/**
 * process packet commands
 *
 * @author matecsaba
 */
public class userPacket {

    /**
     * create instance
     */
    public userPacket() {
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

    private boolean need2stop() {
        if (cmd.pipe.isClosed() != 0) {
            return true;
        }
        int i = cmd.pipe.ready2rx();
        if (i != 0) {
            cmd.pipe.moreSkip(i);
            return true;
        }
        return false;
    }

    private static int readMrt(packHolder pck, RandomAccessFile f) {
        pck.clear();
        byte[] buf = new byte[12];
        try {
            if (f.read(buf, 0, buf.length) != buf.length) {
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        pck.INTtime = bits.msbGetD(buf, 0);
        pck.INTtime *= 1000;
        int typ = bits.msbGetW(buf, 4);
        int cls = bits.msbGetW(buf, 6);
        int i = bits.msbGetD(buf, 8);
        if (i < 0) {
            return 2;
        }
        if (i > packHolder.maxHead) {
            return 2;
        }
        buf = new byte[i];
        try {
            if (f.read(buf, 0, buf.length) != buf.length) {
                return 1;
            }
        } catch (Exception e) {
            return 1;
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        if (typ != rtrBgpMrt.typBgp) {
            return 2;
        }
        boolean xchg = false;
        switch (cls) {
            case rtrBgpMrt.typLoc16:
                pck.getSkip(4);
                xchg = true;
                break;
            case rtrBgpMrt.typRem16:
                pck.getSkip(4);
                break;
            case rtrBgpMrt.typLoc32:
                xchg = true;
                pck.getSkip(8);
                break;
            case rtrBgpMrt.typRem32:
                pck.getSkip(8);
                break;
            default:
                return 2;
        }
        typ = pck.msbGetW(2);
        pck.getSkip(4);
        switch (typ) {
            case 1:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                pck.IPtrg.fromIPv4addr(a4);
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                pck.IPsrc.fromIPv4addr(a4);
                break;
            case 2:
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                pck.IPtrg.fromIPv6addr(a6);
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                pck.IPsrc.fromIPv6addr(a6);
                break;
            default:
                return 2;
        }
        if (pck.msbGetD(0) != -1) {
            try {
                f.seek(f.getFilePointer() - buf.length);
            } catch (Exception e) {
            }
            return 2;
        }
        if (xchg) {
            return 0;
        }
        addrIP adr = new addrIP();
        adr.setAddr(pck.IPsrc);
        pck.IPsrc.setAddr(pck.IPtrg);
        pck.IPtrg.setAddr(adr);
        return 0;
    }

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.pckt, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("mrt2pcap")) {
            RandomAccessFile fs;
            RandomAccessFile ft;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
                a = cmd.word();
                cmd.error("opening target " + a);
                ft = new RandomAccessFile(new File(a), "rw");
                ft.setLength(0);
                byte[] buf = packHolder.getPcapHeader(1);
                ft.write(buf, 0, buf.length);
            } catch (Exception e) {
                return null;
            }
            packHolder pck = new packHolder(true, true);
            cmd.error("converting");
            ipCor4 ic4 = new ipCor4();
            ipCor6 ic6 = new ipCor6();
            tabGen<userPacketSeq> seqs = new tabGen<userPacketSeq>();
            int pk = 0;
            for (;;) {
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                userPacketSeq seq = new userPacketSeq(pck);
                userPacketSeq old = seqs.add(seq);
                if (old != null) {
                    seq = old;
                } else {
                    seq.seq = bits.randomD();
                }
                pck.UDPsrc = rtrBgp.port;
                pck.UDPtrg = rtrBgp.port;
                pck.TCPwin = 8192;
                pck.TCPseq = seq.seq;
                seq.seq += pck.dataSize();
                prtTcp.createTCPheader(pck, null);
                if (pck.IPtrg.isIPv4()) {
                    ic4.createIPheader(pck);
                    i = ipIfc4.type;
                } else {
                    ic6.createIPheader(pck);
                    i = ipIfc6.type;
                }
                pck.msbPutW(0, i);
                pck.putSkip(2);
                pck.merge2beg();
                byte[] buf = pck.convertToPcap(pck.INTtime, true);
                pk++;
                try {
                    ft.write(buf, 0, buf.length);
                } catch (Exception e) {
                }
            }
            try {
                ft.close();
                fs.close();
            } catch (Exception e) {
            }
            cmd.error(pk + " packets (" + seqs.size() + " streams) converted");
            return null;
        }
        if (a.equals("mrtfilter")) {
            tabRouteAttr.routeType rt = cfgRtr.name2num(cmd.word());
            if (rt == null) {
                cmd.error("invalid routing protocol");
                return null;
            }
            cfgRtr rp = cfgAll.rtrFind(rt, bits.str2num(cmd.word()), false);
            if (rp == null) {
                cmd.error("bad process number");
                return null;
            }
            if (rp.bgp == null) {
                cmd.error("not a bgp process");
                return null;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            rtrBgpNeigh nei = rp.bgp.findPeer(adr);
            if (nei == null) {
                cmd.error("no such peer");
                return null;
            }
            RandomAccessFile fs;
            RandomAccessFile ft;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
                a = cmd.word();
                cmd.error("opening target " + a);
                ft = new RandomAccessFile(new File(a), "rw");
            } catch (Exception e) {
                return null;
            }
            addrIP sip = new addrIP();
            sip.fromString(cmd.word());
            addrIP tip = new addrIP();
            tip.fromString(cmd.word());
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            int mat = 0;
            int snt = 0;
            int tot = 0;
            packHolder pck = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            for (;;) {
                long fp;
                try {
                    fp = fs.getFilePointer();
                } catch (Exception e) {
                    return null;
                }
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                int typ = pck.getByte(rtrBgpSpeak.sizeU - 1);
                pck.getSkip(rtrBgpSpeak.sizeU);
                tot++;
                if (sip.compare(sip, pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compare(tip, pck.IPtrg) != 0) {
                    continue;
                }
                if (need2stop()) {
                    break;
                }
                nei.conn.currChg = 0;
                switch (typ) {
                    case rtrBgpUtil.msgUpdate:
                        nei.conn.parseUpdate(pck, hlp);
                        break;
                    case rtrBgpUtil.msgOpen:
                        nei.conn.parseOpen(pck);
                        nei.conn.currChg++;
                        break;
                    default:
                        continue;
                }
                snt++;
                if (nei.conn.currChg < 1) {
                    cmd.pipe.strPut(".");
                    continue;
                }
                cmd.pipe.strPut("!");
                mat++;
                try {
                    long cp = fs.getFilePointer();
                    byte[] buf = new byte[(int) (cp - fp)];
                    fs.seek(fp);
                    fs.read(buf);
                    ft.write(buf);
                } catch (Exception e) {
                    return null;
                }
            }
            try {
                fs.close();
                ft.close();
            } catch (Exception e) {
            }
            cmd.error("sent " + snt + " of " + tot + " updates, " + mat + " accepted");
            return null;
        }
        if (a.equals("mrt2self")) {
            tabRouteAttr.routeType rt = cfgRtr.name2num(cmd.word());
            if (rt == null) {
                cmd.error("invalid routing protocol");
                return null;
            }
            cfgRtr rp = cfgAll.rtrFind(rt, bits.str2num(cmd.word()), false);
            if (rp == null) {
                cmd.error("bad process number");
                return null;
            }
            if (rp.bgp == null) {
                cmd.error("not a bgp process");
                return null;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            rtrBgpNeigh nei = rp.bgp.findPeer(adr);
            if (nei == null) {
                cmd.error("no such peer");
                return null;
            }
            RandomAccessFile fs;
            try {
                a = cmd.word();
                cmd.error("opening " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
                return null;
            }
            addrIP sip = new addrIP();
            sip.fromString(cmd.word());
            addrIP tip = new addrIP();
            tip.fromString(cmd.word());
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            int snt = 0;
            int tot = 0;
            packHolder pck = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            for (;;) {
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                int typ = pck.getByte(rtrBgpSpeak.sizeU - 1);
                pck.getSkip(rtrBgpSpeak.sizeU);
                tot++;
                if (sip.compare(sip, pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compare(tip, pck.IPtrg) != 0) {
                    continue;
                }
                if (need2stop()) {
                    break;
                }
                switch (typ) {
                    case rtrBgpUtil.msgUpdate:
                        nei.conn.parseUpdate(pck, hlp);
                        break;
                    case rtrBgpUtil.msgOpen:
                        nei.conn.parseOpen(pck);
                        break;
                    default:
                        continue;
                }
                snt++;
                cmd.pipe.strPut(".");
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            cmd.error("sent " + snt + " of " + tot + " updates");
            return null;
        }
        if (a.equals("mrt2bmp")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return null;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return null;
            }
            int prt = bits.str2num(cmd.word());
            RandomAccessFile fs;
            try {
                a = cmd.word();
                cmd.error("opening " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
                return null;
            }
            addrIP sip = new addrIP();
            sip.fromString(cmd.word());
            addrIP tip = new addrIP();
            tip.fromString(cmd.word());
            pipeSide strm = null;
            cmd.error("connecting " + trg + " " + prt);
            clntProxy prx = clntProxy.makeTemp(vrf, ifc);
            strm = prx.doConnect(servGeneric.protoTcp, trg, prt, "mrt2bmp");
            if (strm == null) {
                cmd.error("failed");
                try {
                    fs.close();
                } catch (Exception e) {
                }
                return null;
            }
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            packHolder pck = new packHolder(true, true);
            int snt = 0;
            int tot = 0;
            for (;;) {
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                tot++;
                if (sip.compare(sip, pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compare(tip, pck.IPtrg) != 0) {
                    continue;
                }
                rtrBgpMon.createHeader(pck, pck.INTtime, false, rtrBgpMon.typMon, sip, 1, tip.toIPv4());
                pck.pipeSend(strm, 0, pck.dataSize(), 3);
                cmd.pipe.strPut(".");
                if (need2stop()) {
                    break;
                }
                snt++;
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            cmd.error("sent " + snt + " of " + tot + " updates");
            strm.setClose();
            cmd.error("finished");
            return null;
        }
        if (a.equals("mrt2bgp")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return null;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return null;
            }
            int las = bits.str2num(cmd.word());
            RandomAccessFile fs;
            try {
                a = cmd.word();
                cmd.error("opening " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
                return null;
            }
            addrIP sip = new addrIP();
            sip.fromString(cmd.word());
            addrIP tip = new addrIP();
            tip.fromString(cmd.word());
            int safi;
            if (trg.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            a = cmd.word();
            if (a.length() > 0) {
                safi = bits.str2num(a);
            }
            pipeSide strm = null;
            for (;;) {
                cmd.error("connecting " + trg);
                clntProxy prx = clntProxy.makeTemp(vrf, ifc);
                strm = prx.doConnect(servGeneric.protoTcp, trg, rtrBgp.port, "mrt2bgp");
                if (strm != null) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                try {
                    fs.close();
                } catch (Exception e) {
                }
                return null;
            }
            cmd.error("sending safi=" + rtrBgpUtil.safi2string(safi) + " as=" + las + " open");
            rtrBgpNeigh nei = new rtrBgpNeigh(null);
            nei.localAs = las;
            nei.addrFams = safi;
            rtrBgpSpeak spk = new rtrBgpSpeak(null, nei, strm);
            packHolder pck = new packHolder(true, true);
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, nei.localAs);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capaMultiProto, buf);
            pck.merge2beg();
            pck.putByte(0, rtrBgpUtil.version);
            pck.msbPutW(1, rtrBgpUtil.asNum16bit(nei.localAs));
            pck.msbPutW(3, nei.holdTimer / 1000);
            buf = ifc.addr4.getBytes();
            pck.putCopy(buf, 0, 5, buf.length);
            pck.putByte(9, pck.dataSize());
            pck.putSkip(10);
            pck.merge2beg();
            spk.packSend(pck, rtrBgpUtil.msgOpen);
            spk.sendKeepAlive();
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            int snt = 0;
            int tot = 0;
            for (;;) {
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                if (pck.getByte(rtrBgpSpeak.sizeU - 1) != rtrBgpUtil.msgUpdate) {
                    continue;
                }
                pck.getSkip(rtrBgpSpeak.sizeU);
                tot++;
                if (sip.compare(sip, pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compare(tip, pck.IPtrg) != 0) {
                    continue;
                }
                spk.packSend(pck, rtrBgpUtil.msgUpdate);
                cmd.pipe.strPut(".");
                if (need2stop()) {
                    break;
                }
                snt++;
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            cmd.error("sent " + snt + " of " + tot + " updates");
            cmd.error("waiting");
            for (int o = 1000;; o++) {
                if (o > 30) {
                    cmd.pipe.strPut(".");
                    spk.sendKeepAlive();
                    o = 0;
                }
                int i = strm.ready2rx();
                if (i > 0) {
                    strm.moreSkip(i);
                }
                i = strm.ready2tx();
                if (i < 0) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return null;
        }
        if (a.equals("bgpattr")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return null;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return null;
            }
            int las = bits.str2num(cmd.word());
            addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
            if (prf == null) {
                return null;
            }
            cfgRoump rmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rmp == null) {
                return null;
            }
            List<Integer> attr = new ArrayList<Integer>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                attr.add(bits.fromHex(a));
            }
            pipeSide strm = null;
            for (;;) {
                cmd.error("connecting " + trg);
                clntProxy prx = clntProxy.makeTemp(vrf, ifc);
                strm = prx.doConnect(servGeneric.protoTcp, trg, rtrBgp.port, "bgpgen");
                if (strm != null) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                return null;
            }
            cmd.error("sending open");
            rtrBgpNeigh nei = new rtrBgpNeigh(null);
            nei.localAs = las;
            int safi;
            if (prf.network.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            nei.addrFams = safi;
            rtrBgpSpeak spk = new rtrBgpSpeak(null, nei, strm);
            packHolder pck = new packHolder(true, true);
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, nei.localAs);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capaMultiProto, buf);
            pck.merge2beg();
            pck.putByte(0, rtrBgpUtil.version);
            pck.msbPutW(1, rtrBgpUtil.asNum16bit(nei.localAs));
            pck.msbPutW(3, nei.holdTimer / 1000);
            buf = ifc.addr4.getBytes();
            pck.putCopy(buf, 0, 5, buf.length);
            pck.putByte(9, pck.dataSize());
            pck.putSkip(10);
            pck.merge2beg();
            spk.packSend(pck, rtrBgpUtil.msgOpen);
            spk.sendKeepAlive();
            buf = new byte[attr.size()];
            for (int i = 0; i < buf.length; i++) {
                buf[i] = (byte) (attr.get(i) & 0xff);
            }
            cmd.error("sending " + prf + " network with attrib " + bits.byteDump(buf, 0, -1));
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = prf.copyBytes();
            rmp.roumap.update(rtrBgpUtil.sfiUnicast, 0, ntry, false);
            ntry.best.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
            packHolder tmp = new packHolder(true, true);
            cmd.error("sending update");
            pck.clear();
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            lst.add(ntry);
            rtrBgpUtil.createReachable(pck, tmp, safi, false, true, lst, buf);
            spk.packSend(pck, rtrBgpUtil.msgUpdate);
            cmd.error("waiting");
            for (int o = 1000;; o++) {
                if (o > 30) {
                    cmd.pipe.strPut(".");
                    spk.sendKeepAlive();
                    o = 0;
                }
                int i = strm.ready2rx();
                if (i > 0) {
                    strm.moreSkip(i);
                }
                i = strm.ready2tx();
                if (i < 0) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return null;
        }
        if (a.equals("bgpgen")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return null;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return null;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return null;
            }
            int las = bits.str2num(cmd.word());
            addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
            if (prf == null) {
                return null;
            }
            cfgRoump rmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rmp == null) {
                return null;
            }
            int num = bits.str2num(cmd.word());
            pipeSide strm = null;
            for (;;) {
                cmd.error("connecting " + trg);
                clntProxy prx = clntProxy.makeTemp(vrf, ifc);
                strm = prx.doConnect(servGeneric.protoTcp, trg, rtrBgp.port, "bgpgen");
                if (strm != null) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                return null;
            }
            cmd.error("sending open");
            rtrBgpNeigh nei = new rtrBgpNeigh(null);
            nei.localAs = las;
            int safi;
            if (prf.network.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            nei.addrFams = safi;
            rtrBgpSpeak spk = new rtrBgpSpeak(null, nei, strm);
            packHolder pck = new packHolder(true, true);
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, nei.localAs);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, false, rtrBgpUtil.capaMultiProto, buf);
            pck.merge2beg();
            pck.putByte(0, rtrBgpUtil.version);
            pck.msbPutW(1, rtrBgpUtil.asNum16bit(nei.localAs));
            pck.msbPutW(3, nei.holdTimer / 1000);
            buf = ifc.addr4.getBytes();
            pck.putCopy(buf, 0, 5, buf.length);
            pck.putByte(9, pck.dataSize());
            pck.putSkip(10);
            pck.merge2beg();
            spk.packSend(pck, rtrBgpUtil.msgOpen);
            spk.sendKeepAlive();
            cmd.error("sending " + num + " random " + prf + " networks");
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            ntry.prefix = prf.copyBytes();
            rmp.roumap.update(rtrBgpUtil.sfiUnicast, 0, ntry, false);
            ntry.best.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
            packHolder tmp = new packHolder(true, true);
            for (int o = 0; o < num; o++) {
                int i = strm.ready2rx();
                if (i > 0) {
                    strm.moreSkip(i);
                }
                i = strm.ready2tx();
                if (i < 0) {
                    break;
                }
                if (i < 4096) {
                    bits.sleep(1000);
                }
                addrIP adr = new addrIP();
                adr.fillRandom();
                adr.setAnd(adr, prf.wildcard);
                adr.setOr(adr, prf.network);
                ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                pck.clear();
                lst.clear();
                lst.add(ntry);
                rtrBgpUtil.createReachable(pck, tmp, safi, false, true, lst, null);
                spk.packSend(pck, rtrBgpUtil.msgUpdate);
                cmd.pipe.strPut(".");
                if (need2stop()) {
                    break;
                }
            }
            cmd.error("sent " + num + " networks");
            cmd.error("waiting");
            for (int o = 1000;; o++) {
                if (o > 30) {
                    cmd.pipe.strPut(".");
                    spk.sendKeepAlive();
                    o = 0;
                }
                int i = strm.ready2rx();
                if (i > 0) {
                    strm.moreSkip(i);
                }
                i = strm.ready2tx();
                if (i < 0) {
                    break;
                }
                bits.sleep(1000);
                if (need2stop()) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return null;
        }
        if (a.equals("udpflood")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return null;
            }
            addrIP sa = new addrIP();
            sa.fromString(cmd.word());
            int sp = bits.str2num(cmd.word());
            addrPrefix<addrIP> ta = addrPrefix.str2ip(cmd.word());
            tabIntMatcher tp = new tabIntMatcher();
            tp.fromString(cmd.word());
            tabIntMatcher sz = new tabIntMatcher();
            sz.fromString(cmd.word());
            cfgPlymp plc = cfgAll.plmpFind(cmd.word(), false);
            if (plc == null) {
                cmd.error("no such policy map");
                return null;
            }
            tabQos qos = tabQos.convertPolicy(plc.plcmap);
            ipFwd fwd = vrf.getFwd(ta.network);
            ipFwdIface fwi = ipFwdTab.findSendingIface(fwd, ta.network);
            if (fwi == null) {
                cmd.error("no outgoing interface");
                return null;
            }
            cmd.error("flooding " + sa + " " + sp + " -> " + ta + " " + tp + " on " + fwd.vrfName);
            pipeProgress prg = new pipeProgress(cmd.pipe);
            long cnt = 0;
            packHolder pck = new packHolder(true, true);
            addrIP adr = new addrIP();
            int ofs = adr.getSize() - 4;
            for (;;) {
                prg.setCurr(cnt);
                if (need2stop()) {
                    break;
                }
                pck.clear();
                pck.putSkip(bits.random(sz.rangeMin, sz.rangeMax + 1));
                pck.merge2beg();
                if (qos.checkPacket(bits.getTime(), pck)) {
                    continue;
                }
                bits.msbPutD(adr.getBytes(), ofs, bits.randomD());
                adr.setAnd(adr, ta.wildcard);
                adr.setOr(adr, ta.network);
                pck.IPsrc.setAddr(sa);
                pck.UDPsrc = sp;
                pck.IPtrg.setAddr(adr);
                pck.UDPtrg = bits.random(tp.rangeMin, tp.rangeMax + 1);
                pck.TCPflg = 0;
                prtUdp.createUDPheader(pck);
                pck.merge2beg();
                fwd.protoPack(fwi, null, pck);
                cnt++;
            }
            return null;
        }
        if (a.equals("flood")) {
            a = cmd.word();
            cfgVrf vrf = null;
            cfgIfc ifc = null;
            if (a.equals("vrf")) {
                vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return null;
                }
            } else {
                ifc = cfgAll.ifcFind(cmd.word(), false);
                if (ifc == null) {
                    cmd.error("no such interface");
                    return null;
                }
            }
            a = cmd.word();
            packHolder pck = new packHolder(true, true);
            pck.IPsrc.fromString(cmd.word());
            pck.UDPsrc = bits.str2num(cmd.word());
            pck.IPtrg.fromString(cmd.word());
            pck.UDPtrg = bits.str2num(cmd.word());
            pck.TCPflg = bits.str2num(cmd.word());
            pck.putSkip(bits.str2num(cmd.word()));
            pck.merge2beg();
            cfgPlymp plc = cfgAll.plmpFind(cmd.word(), false);
            if (plc == null) {
                cmd.error("no such policy map");
                return null;
            }
            tabQos qos = tabQos.convertPolicy(plc.plcmap);
            if (a.equals("tcp")) {
                prtTcp.createTCPheader(pck, null);
            }
            if (a.equals("udp")) {
                prtUdp.createUDPheader(pck);
            }
            if (a.equals("ludp")) {
                prtLudp.createLUDPheader(pck);
            }
            if (a.equals("dccp")) {
                prtDccp.createDCCPheader(pck);
            }
            if (a.equals("sctp")) {
                prtSctp.createSCTPheader(pck);
            }
            pck.merge2beg();
            pipeProgress prg = new pipeProgress(cmd.pipe);
            long cnt = 0;
            if (vrf != null) {
                ipFwd fwd = vrf.getFwd(pck.IPtrg);
                ipFwdIface fwi = ipFwdTab.findSendingIface(fwd, pck.IPtrg);
                if (fwi == null) {
                    cmd.error("no outgoing interface");
                    return null;
                }
                cmd.error("flooding " + pck.IPsrc + " " + pck.UDPsrc + " -> " + pck.IPtrg + " " + pck.UDPtrg + " on " + fwd.vrfName);
                cmd.error("packet is " + pck.dump());
                for (;;) {
                    prg.setCurr(cnt);
                    if (need2stop()) {
                        break;
                    }
                    if (qos.checkPacket(bits.getTime(), pck)) {
                        continue;
                    }
                    fwd.protoPack(fwi, null, pck.copyBytes(true, true));
                    cnt++;
                }
                return null;
            }
            ipCor cor;
            int typ;
            if (pck.IPtrg.isIPv4()) {
                cor = new ipCor4();
                typ = ipIfc4.type;
            } else {
                cor = new ipCor6();
                typ = ipIfc6.type;
            }
            cor.createIPheader(pck);
            pck.merge2beg();
            pck.msbPutW(0, typ);
            pck.putSkip(2);
            pck.merge2beg();
            try {
                pck.ETHsrc.setAddr(ifc.ethtyp.getHwAddr());
            } catch (Exception e) {
            }
            pck.ETHtrg.fillBytes(0xff);
            cmd.error("flooding " + pck.IPsrc + " " + pck.UDPsrc + " -> " + pck.IPtrg + " " + pck.UDPtrg + " on " + ifc.name);
            cmd.error("packet is " + pck.dump());
            for (;;) {
                prg.setCurr(cnt);
                if (need2stop()) {
                    break;
                }
                if (qos.checkPacket(bits.getTime(), pck)) {
                    continue;
                }
                ifc.ethtyp.doTxPack(pck.copyBytes(true, true));
                cnt++;
            }
            return null;
        }
        if (a.equals("message")) {
            clntVoice sv = new clntVoice();
            sv.called = cmd.word();
            sv.calling = cmd.word();
            cmd.error("result = " + sv.sendMessage(bits.str2lst(cmd.getRemaining())));
            return null;
        }
        if (a.equals("conference")) {
            clntVconf sv = new clntVconf();
            sv.calling = cmd.word();
            boolean bg = false;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("-")) {
                    bg = true;
                    continue;
                }
                sv.addPeer(a);
            }
            sv.startWork();
            pipeSide usr = sv.getPipe();
            if (bg) {
                usr.setClose();
                return null;
            }
            sv.prompt = true;
            pipeTerm trm = new pipeTerm(pip, usr);
            trm.doTerm();
            usr.setClose();
            return null;
        }
        if (a.equals("voice")) {
            clntVoice sv = new clntVoice();
            sv.called = cmd.word();
            sv.calling = cmd.word();
            if (sv.calling.length() < 1) {
                sv.calling = "sip:voice@" + cfgAll.getFqdn();
            }
            if (sv.callStart()) {
                sv.callStop();
                cmd.error("failed to place call");
                return null;
            }
            pipeSide usr = sv.getPipe();
            List<String> l = bits.txt2buf(cmd.word());
            if (l == null) {
                sv.setPrompt(true);
                pipeTerm trm = new pipeTerm(pip, usr);
                trm.doTerm();
            } else {
                usr.setTime(120000);
                usr.lineTx = pipeSide.modTyp.modeCRLF;
                usr.lineRx = pipeSide.modTyp.modeCRorLF;
                userScript t = new userScript(usr, "");
                t.addLines(l);
                t.allowConfig = true;
                t.allowExec = true;
                t.cmdAll();
                usr.setClose();
            }
            sv.callStop();
            return null;
        }
        if (a.equals("modem")) {
            clntModem sm = new clntModem();
            sm.called = cmd.word();
            sm.calling = cmd.word();
            if (sm.calling.length() < 1) {
                sm.calling = "sip:modem@" + cfgAll.getFqdn();
            }
            if (sm.callStart()) {
                sm.callStop();
                cmd.error("failed to place call");
                return null;
            }
            pipeTerm trm = new pipeTerm(pip, sm.getPipe());
            trm.doTerm();
            sm.callStop();
            return null;
        }
        if (a.equals("capture")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            a += ".pcap";
            if (cmd.size() > 0) {
                a = cmd.word();
            }
            cmd.error("capturing=" + !ifc.ethtyp.initLog(a));
            return null;
        }
        if (a.equals("monitor")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            ifcEthTyp oldS = ifc.ethtyp.monSes;
            boolean oldH = ifc.ethtyp.monHdr;
            a = cmd.word();
            if (a.length() > 0) {
                cfgIfc trg = cfgAll.ifcFind(a, false);
                if (trg == null) {
                    cmd.error("no such interface");
                    return null;
                }
                ifc.ethtyp.monHdr = trg.ifaceNeedMacs();
                ifc.ethtyp.monSes = trg.ethtyp;
            }
            cmd.error("       rxpps       rxbps       txpps       txbps");
            for (;;) {
                if (need2stop()) {
                    break;
                }
                counter cntr = ifc.ethtyp.getCounter().copyBytes();
                bits.sleep(1000);
                cntr = ifc.ethtyp.getCounter().copyBytes().minus(cntr);
                cmd.error(bits.padBeg(bits.toUser(cntr.packRx), 12, " ") + bits.padBeg(bits.toUser(cntr.byteRx * 8), 12, " ") + bits.padBeg(bits.toUser(cntr.packTx), 12, " ") + bits.padBeg(bits.toUser(cntr.byteTx * 8), 12, " "));
            }
            ifc.ethtyp.monHdr = oldH;
            ifc.ethtyp.monSes = oldS;
            return null;
        }
        if (a.equals("buffer")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            a += ".pcap";
            if (cmd.size() > 0) {
                a = cmd.word();
            }
            byte[] buf = ifc.ethtyp.monBufD;
            if (buf == null) {
                cmd.error("no buffer");
                return null;
            }
            cmd.error("saving " + buf.length + " bytes");
            int pos = ifc.ethtyp.monBufP;
            try {
                RandomAccessFile f = new RandomAccessFile(new File(a), "rw");
                f.setLength(0);
                f.write(packHolder.getPcapHeader(1));
                f.write(buf, pos, buf.length - pos);
                f.write(buf, 0, pos);
                f.close();
            } catch (Exception e) {
            }
            cmd.error("issue pcapfix -d " + a);
            return null;
        }
        if (a.equals("wakeup")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            packWol pckWol = new packWol();
            packHolder pckBin = new packHolder(true, true);
            if (pckWol.addr.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            pckWol.createPayload(pckBin);
            pckBin.merge2beg();
            cmd.error("tx: " + pckBin.dump());
            ifc.ethtyp.doTxPack(pckBin);
            return null;
        }
        if (a.equals("inject")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            packHolder pck = new packHolder(true, true);
            if (pck.convertFromK12("|0   |" + cmd.getRemaining())) {
                cmd.error("error in packet");
                return null;
            }
            if (ifc.ifaceNeedArp()) {
                ifcEther.parseETHheader(pck, false);
            }
            cmd.error("tx: " + pck.dump());
            ifc.ethtyp.doTxPack(pck);
            return null;
        }
        if (a.equals("random")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            packHolder pck = new packHolder(true, true);
            if (pck.convertFromK12("|0   |" + cmd.getRemaining())) {
                cmd.error("error in packet");
                return null;
            }
            for (;;) {
                cmd.pipe.strPut(".");
                if (need2stop()) {
                    break;
                }
                bits.sleep(100);
                packHolder res = pck.copyBytes(true, true);
                for (int i = 0; i < 64; i++) {
                    res.putByte(0, bits.randomB());
                    res.putSkip(1);
                }
                res.merge2end();
                ifc.ethtyp.doTxPack(res);
            }
            return null;
        }
        if (a.equals("replay")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            List<String> txt = bits.txt2buf(cmd.word());
            if (txt == null) {
                cmd.error("no such file");
                return null;
            }
            int ipg = bits.str2num(cmd.word());
            if (ipg < 1) {
                ipg = 1;
            }
            for (int i = 0; i < txt.size(); i++) {
                packHolder pck = new packHolder(true, true);
                if (pck.convertFromK12(txt.get(i))) {
                    continue;
                }
                if (ifc.ifaceNeedArp()) {
                    ifcEther.parseETHheader(pck, false);
                }
                cmd.error("tx: " + pck.dump());
                ifc.ethtyp.doTxPack(pck);
                bits.sleep(ipg);
            }
            return null;
        }
        if (a.equals("speed")) {
            rdr.keyFlush();
            clntSpeed.smllClnt(cmd);
            rdr.keyFlush();
            return null;
        }
        if (a.equals("websock")) {
            uniResLoc url = new uniResLoc();
            if (url.fromString(cmd.word())) {
                cmd.error("bad url");
                return null;
            }
            pipeSide strm = secWebsock.doConnect(cfgAll.getClntPrx(null), url, cmd.getRemaining());
            if (strm == null) {
                cmd.error("failed to connect");
                return null;
            }
            secWebsock ws = new secWebsock(strm, new pipeLine(65536, false));
            ws.startClient();
            pipeTerm trm = new pipeTerm(cmd.pipe, ws.getPipe());
            trm.doTerm();
            return null;
        }
        if (a.equals("netconf")) {
            a = cmd.word();
            String s = null;
            if (a.equals("get")) {
                s = "get/filter";
            }
            if (a.equals("read")) {
                s = "get-config/filter";
            }
            if (a.equals("edit")) {
                s = "edit-config/config";
            }
            if (a.equals("copy")) {
                s = "copy-config";
            }
            if (a.equals("delete")) {
                s = "delete-config";
            }
            if (s == null) {
                cmd.error("invalid command");
                return null;
            }
            a = cmd.word();
            userTerminal trm = new userTerminal(cmd.pipe);
            addrIP trg = trm.resolveAddr(a, 0);
            if (trg == null) {
                cmd.error("server not found");
                return null;
            }
            clntProxy prx = cfgAll.getClntPrx(null);
            if (prx == null) {
                cmd.error("no proxy configured");
                return null;
            }
            pipeSide conn = prx.doConnect(servGeneric.protoTcp, trg, userNetconf.port, "netconf");
            if (conn == null) {
                cmd.error("error opening connection");
                return null;
            }
            a = cmd.word();
            conn = trm.startSecurity(conn, servGeneric.protoSsh, null, a, cmd.word());
            if (conn == null) {
                return null;
            }
            userNetconf nc = new userNetconf(conn, false, false, false);
            if (nc.doHello()) {
                cmd.error("error exchange hello");
                return null;
            }
            a = cmd.word();
            nc.doClient(cmd, s, a, cmd.word());
            nc.doClose();
            conn.setClose();
            return null;
        }
        if (a.equals("snmp")) {
            a = cmd.word();
            clntSnmp sn = new clntSnmp(cmd.pipe, cfgAll.getClntPrx(null), cmd.word());
            sn.community = cmd.word();
            if (a.equals("get")) {
                sn.doGet(cmd.word());
                return null;
            }
            if (a.equals("next")) {
                sn.doNext(cmd.word());
                return null;
            }
            return null;
        }
        if (a.equals("pcep")) {
            clntPcep pc = new clntPcep();
            a = cmd.word();
            a += " " + cmd.word();
            a += " " + cmd.word();
            pc.setTarget(a);
            a = cmd.word();
            int st = -1;
            if (a.equals("te")) {
                st = 0;
            }
            if (a.equals("sr")) {
                st = 1;
            }
            addrIP src = new addrIP();
            addrIP trg = new addrIP();
            src.fromString(cmd.word());
            trg.fromString(cmd.word());
            if (pc.doConnect()) {
                cmd.error("failed to connect");
                return null;
            }
            List<tabHop> res = pc.doCompute(st, src, trg, 0, 0, 0, 0, 0, 0, 2, 0);
            pc.doClose();
            if (res == null) {
                cmd.error("failed to get path");
                return null;
            }
            cmd.error("path=" + tabHop.dumpList(res));
            return null;
        }
        if (a.equals("aaa")) {
            cfgAuther aa = cfgAll.autherFind(cmd.word(), null);
            if (aa == null) {
                cmd.error("no such aaa");
                return null;
            }
            authGeneric aaa = aa.getAuther();
            if (aaa == null) {
                cmd.error("no such aaa");
                return null;
            }
            cmd.pipe.strPut("user:");
            String usr = cmd.pipe.lineGet(0x32);
            cmd.pipe.strPut("pass:");
            int i;
            if (cfgAll.passwdStars) {
                i = 0x33;
            } else {
                i = 0x31;
            }
            String pwd = cmd.pipe.lineGet(i);
            authResult res = aaa.authUserPass(usr, pwd);
            rdr.putStrTab(res.dump());
            return null;
        }
        if (a.equals("ntp")) {
            clntNtp t = new clntNtp(cmd.word());
            if (t.doWork()) {
                return null;
            }
            cmd.error("time=" + bits.time2str(cfgAll.timeZoneName, t.tim1, 3) + " diff=" + t.tim3);
            return null;
        }
        if (a.equals("nrpe")) {
            clntProxy prx = cfgAll.getClntPrx(null);
            if (prx == null) {
                return null;
            }
            clntNrpe ch = new clntNrpe(cmd.pipe, prx.vrf, prx.srcIfc, cmd.word());
            boolean b = ch.doCheck(cmd.getRemaining());
            cmd.error("status=" + b + ", code=" + packNrpe.code2string(ch.code));
            rdr.putStrArr(ch.text);
            return null;
        }
        if (a.equals("smtp")) {
            clntSmtp sm = new clntSmtp(cmd.pipe);
            a = cmd.word();
            String s = cmd.getRemaining().trim();
            if (s.length() < 1) {
                s = "test message";
            }
            sm.rcpt = a;
            sm.putHead("test@" + cfgAll.getFqdn(), a, s);
            sm.putText(bits.str2lst(s));
            sm.putFinish();
            cmd.error("result=" + sm.doSend(1));
            sm.cleanUp();
            return null;
        }
        cmd.badCmd();
        return null;
    }

}

class userPacketSeq implements Comparator<userPacketSeq> {

    public addrIP src;

    public addrIP trg;

    public int seq;

    public userPacketSeq(packHolder pck) {
        src = pck.IPsrc.copyBytes();
        trg = pck.IPtrg.copyBytes();
    }

    public int compare(userPacketSeq o1, userPacketSeq o2) {
        int i = o1.src.compare(o1.src, o2.src);
        if (i != 0) {
            return i;
        }
        return o1.trg.compare(o1.trg, o2.trg);
    }

}
