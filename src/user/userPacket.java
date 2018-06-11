package user;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgRoump;
import cfg.cfgRtr;
import cfg.cfgVrf;
import clnt.clntProxy;
import clnt.clntModem;
import clnt.clntNrpe;
import clnt.clntSmtp;
import clnt.clntSnmp;
import clnt.clntSpeed;
import clnt.clntVconf;
import clnt.clntVoice;
import ifc.ifcEther;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import ip.ipIfc4;
import ip.ipIfc6;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packWol;
import pipe.pipeLine;
import pipe.pipeProgress;
import pipe.pipeSide;
import pipe.pipeTerm;
import prt.prtDccp;
import prt.prtLudp;
import prt.prtSctp;
import prt.prtTcp;
import prt.prtUdp;
import rtr.rtrBgp;
import rtr.rtrBgpMrt;
import rtr.rtrBgpNeigh;
import rtr.rtrBgpSpeak;
import rtr.rtrBgpUtil;
import sec.secWebsock;
import serv.servGeneric;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.uniResLoc;

/**
 * process packet commands
 *
 * @author matecsaba
 */
public class userPacket {

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
     */
    public void doer() {
        String a = cmd.word();
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
                return;
            }
            packHolder pck = new packHolder(true, true);
            cmd.error("converting");
            ipCor4 ic4 = new ipCor4();
            ipCor6 ic6 = new ipCor6();
            int pk = 0;
            int sq = 0;
            for (;;) {
                int i = readMrt(pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                pck.UDPsrc = rtrBgp.port;
                pck.UDPtrg = rtrBgp.port;
                pck.TCPwin = 8192;
                pck.TCPseq = sq;
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
                sq += buf.length;
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
            cmd.error(pk + " packets converted");
            return;
        }
        if (a.equals("mrt2self")) {
            tabRouteEntry.routeType rt = cfgRtr.name2num(cmd.word());
            if (rt == null) {
                cmd.error("invalid routing protocol");
                return;
            }
            cfgRtr rp = cfgAll.rtrFind(rt, bits.str2num(cmd.word()), false);
            if (rp == null) {
                cmd.error("bad process number");
                return;
            }
            if (rp.bgp == null) {
                cmd.error("not a bgp process");
                return;
            }
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            rtrBgpNeigh nei = rp.bgp.findPeer(adr);
            if (nei == null) {
                cmd.error("no such peer");
                return;
            }
            RandomAccessFile fs;
            try {
                a = cmd.word();
                cmd.error("opening " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
                return;
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
                if (cmd.pipe.ready2rx() > 0) {
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
            return;
        }
        if (a.equals("mrtplay")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return;
            }
            int las = bits.str2num(cmd.word());
            RandomAccessFile fs;
            try {
                a = cmd.word();
                cmd.error("opening " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
                return;
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
                strm = prx.doConnect(servGeneric.protoTcp, trg, rtrBgp.port, "mrtplay");
                if (strm != null) {
                    break;
                }
                bits.sleep(1000);
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                return;
            }
            cmd.error("sending safi=" + rtrBgpUtil.safi2string(safi) + " as=" + las + " open");
            rtrBgpNeigh nei = new rtrBgpNeigh(null);
            nei.localAs = las;
            nei.addrFams = safi;
            rtrBgpSpeak spk = new rtrBgpSpeak(null, nei, strm);
            packHolder pck = new packHolder(true, true);
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, nei.localAs);
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaMultiProto, buf);
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
                if (cmd.pipe.ready2rx() > 0) {
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
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return;
        }
        if (a.equals("bgpattr")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return;
            }
            int las = bits.str2num(cmd.word());
            addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
            cfgRoump rmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rmp == null) {
                return;
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
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                return;
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
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaMultiProto, buf);
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
            rmp.roumap.update(rtrBgpUtil.safiUnicast, ntry, false);
            ntry.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
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
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return;
        }
        if (a.equals("bgpgen")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                return;
            }
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                return;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                return;
            }
            int las = bits.str2num(cmd.word());
            addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
            cfgRoump rmp = cfgAll.rtmpFind(cmd.word(), false);
            if (rmp == null) {
                return;
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
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            if (strm == null) {
                cmd.error("failed");
                return;
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
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capa32bitAsNum, buf);
            buf = new byte[4];
            bits.msbPutD(buf, 0, nei.addrFams);
            rtrBgpUtil.placeCapability(pck, rtrBgpUtil.capaMultiProto, buf);
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
            rmp.roumap.update(rtrBgpUtil.safiUnicast, ntry, false);
            ntry.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
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
                if (cmd.pipe.ready2rx() > 0) {
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
                if (cmd.pipe.ready2rx() > 0) {
                    break;
                }
            }
            strm.setClose();
            cmd.error("finished");
            return;
        }
        if (a.equals("flood")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
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
            ipFwd fwd = vrf.getFwd(pck.IPtrg);
            ipFwdIface ifc = ipFwdTab.findSendingIface(vrf.getFwd(pck.IPtrg), pck.IPtrg);
            if (ifc == null) {
                return;
            }
            cmd.error("flooding " + pck.IPsrc + " " + pck.UDPsrc + " -> " + pck.IPtrg + " " + pck.UDPtrg);
            for (;;) {
                cmd.pipe.strPut(".");
                int i = cmd.pipe.ready2rx();
                if (i > 0) {
                    cmd.pipe.moreSkip(i);
                    break;
                }
                i = cmd.pipe.ready2tx();
                if (i < 0) {
                    break;
                }
                fwd.protoPack(ifc, pck.copyBytes(true, true));
            }
            return;
        }
        if (a.equals("message")) {
            clntVoice sv = new clntVoice();
            sv.called = cmd.word();
            sv.calling = cmd.word();
            cmd.error("result = " + sv.sendMessage(bits.str2lst(cmd.getRemaining())));
            return;
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
                return;
            }
            sv.prompt = true;
            pipeTerm trm = new pipeTerm(pip, usr);
            trm.doTerm();
            usr.setClose();
            return;
        }
        if (a.equals("voice")) {
            clntVoice sv = new clntVoice();
            sv.called = cmd.word();
            sv.calling = cmd.word();
            if (sv.calling.length() < 1) {
                sv.calling = "sip:voice@" + cfgAll.hostName;
            }
            if (sv.callStart()) {
                sv.callStop();
                cmd.error("failed to place call");
                return;
            }
            pipeSide usr = sv.getPipe();
            List<String> l = bits.txt2buf(cmd.word());
            if (l == null) {
                sv.setPrompt(true);
                pipeTerm trm = new pipeTerm(pip, usr);
                trm.doTerm();
            } else {
                usr.timeout = 120000;
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
            return;
        }
        if (a.equals("modem")) {
            clntModem sm = new clntModem();
            sm.called = cmd.word();
            sm.calling = cmd.word();
            if (sm.calling.length() < 1) {
                sm.calling = "sip:modem@" + cfgAll.hostName;
            }
            if (sm.callStart()) {
                sm.callStop();
                cmd.error("failed to place call");
                return;
            }
            pipeTerm trm = new pipeTerm(pip, sm.getPipe());
            trm.doTerm();
            sm.callStop();
            return;
        }
        if (a.equals("capture")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            a += ".pcap";
            if (cmd.size() > 0) {
                a = cmd.word();
            }
            if (ifc.thread == null) {
                cmd.error("capturing=" + !ifc.ethtyp.initLog(a));
            } else {
                cmd.error("capturing=" + !ifc.thread.initLog(a));
            }
            return;
        }
        if (a.equals("monitor")) {
            rdr.keyFlush();
            clntSpeed.monInt(cmd);
            rdr.keyFlush();
            return;
        }
        if (a.equals("buffer")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            a += ".pcap";
            if (cmd.size() > 0) {
                a = cmd.word();
            }
            byte[] buf = ifc.ethtyp.monBufD;
            if (buf == null) {
                cmd.error("no buffer");
                return;
            }
            cmd.error("saving " + buf.length + " bytes");
            try {
                RandomAccessFile f = new RandomAccessFile(new File(a), "rw");
                f.setLength(0);
                f.write(packHolder.getPcapHeader(1));
                f.write(buf, ifc.ethtyp.monBufP, buf.length - ifc.ethtyp.monBufP);
                f.write(buf, 0, ifc.ethtyp.monBufP);
                f.close();
            } catch (Exception e) {
            }
            cmd.error("issue pcapfix -d " + a);
            return;
        }
        if (a.equals("wakeup")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            packWol pckWol = new packWol();
            packHolder pckBin = new packHolder(true, true);
            if (pckWol.addr.fromString(cmd.word())) {
                cmd.error("bad address");
                return;
            }
            pckWol.createPayload(pckBin);
            pckBin.merge2beg();
            cmd.error("tx: " + pckBin.dump());
            ifc.ethtyp.doTxPack(pckBin);
            return;
        }
        if (a.equals("inject")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            packHolder pck = new packHolder(true, true);
            if (pck.convertFromK12("|0   |" + cmd.getRemaining())) {
                cmd.error("error in packet");
                return;
            }
            if (ifc.ifaceNeedArp()) {
                ifcEther.parseETHheader(pck, false);
            }
            cmd.error("tx: " + pck.dump());
            ifc.ethtyp.doTxPack(pck);
            return;
        }
        if (a.equals("random")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            packHolder pck = new packHolder(true, true);
            if (pck.convertFromK12("|0   |" + cmd.getRemaining())) {
                cmd.error("error in packet");
                return;
            }
            for (;;) {
                cmd.pipe.strPut(".");
                int i = cmd.pipe.ready2rx();
                if (i > 0) {
                    cmd.pipe.moreSkip(i);
                    break;
                }
                i = cmd.pipe.ready2tx();
                if (i < 0) {
                    break;
                }
                bits.sleep(100);
                packHolder res = pck.copyBytes(true, true);
                for (i = 0; i < 64; i++) {
                    res.putByte(0, bits.randomB());
                    res.putSkip(1);
                }
                res.merge2end();
                ifc.ethtyp.doTxPack(res);
            }
            return;
        }
        if (a.equals("replay")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return;
            }
            List<String> txt = bits.txt2buf(cmd.word());
            if (txt == null) {
                cmd.error("no such file");
                return;
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
            return;
        }
        if (a.equals("speed")) {
            rdr.keyFlush();
            clntSpeed.smllClnt(cmd);
            rdr.keyFlush();
            return;
        }
        if (a.equals("websock")) {
            uniResLoc url = new uniResLoc();
            if (url.fromString(cmd.word())) {
                cmd.error("bad url");
                return;
            }
            pipeSide strm = secWebsock.doConnect(cfgAll.getClntPrx(), url, cmd.getRemaining());
            if (strm == null) {
                cmd.error("failed to connect");
                return;
            }
            secWebsock ws = new secWebsock(strm, new pipeLine(65536, false));
            ws.startClient();
            pipeTerm trm = new pipeTerm(cmd.pipe, ws.getPipe());
            trm.doTerm();
            return;
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
                return;
            }
            if (a.equals("next")) {
                sn.doNext();
                return;
            }
            return;
        }
        if (a.equals("nrpe")) {
            clntNrpe ch = new clntNrpe(cmd.pipe);
            ch.server = cmd.word();
            ch.check = cmd.getRemaining();
            boolean b = ch.doCheck();
            cmd.error("status=" + b + ", code=" + ch.code);
            rdr.putStrArr(ch.text);
            return;
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
            return;
        }
        cmd.badCmd();
        return;
    }

}
