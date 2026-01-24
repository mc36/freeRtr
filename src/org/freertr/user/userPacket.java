package org.freertr.user;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.auth.authGeneric;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAlias;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgAuther;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntHttp;
import org.freertr.clnt.clntModem;
import org.freertr.clnt.clntNrpe;
import org.freertr.clnt.clntNtp;
import org.freertr.clnt.clntPcep;
import org.freertr.clnt.clntPmtud;
import org.freertr.clnt.clntPorts;
import org.freertr.clnt.clntProxy;
import org.freertr.clnt.clntRis;
import org.freertr.clnt.clntSmtp;
import org.freertr.clnt.clntSnmp;
import org.freertr.clnt.clntSpeed;
import org.freertr.clnt.clntVconf;
import org.freertr.clnt.clntVoice;
import org.freertr.clnt.clntXotPad;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ip.ipCor;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipIfc;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNrpe;
import org.freertr.pack.packWol;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeSide;
import org.freertr.pipe.pipeRelay;
import org.freertr.prt.prtDccp;
import org.freertr.prt.prtLudp;
import org.freertr.prt.prtSctp;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpMon;
import org.freertr.rtr.rtrBgpMrt;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.sec.secWebsock;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servP4lang;
import org.freertr.enc.encUrl;
import org.freertr.pack.packXotPad;
import org.freertr.pipe.pipeSetting;
import org.freertr.prt.prtArping;
import org.freertr.rtr.rtrBgpDump;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.sec.secClient;
import org.freertr.serv.servOpenflow;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabHop;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteBlob;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabSessionEntry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

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
    public userRead rdr;

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

    /**
     * do the work
     *
     * @return command to execute, null if nothing
     */
    public cfgAlias doer() {
        if (cfgAll.evalVdcPrivs()) {
            cmd.error("not in a vdc");
            return null;
        }
        String a = cmd.word();
        cfgAlias alias = cfgAll.aliasFind(a, cfgAlias.aliasType.pckt, false);
        if (alias != null) {
            return alias;
        }
        if (a.equals("portscan")) {
            String rem = cmd.word();
            int prt = bits.str2num(cmd.word());
            cfgVrf vrf = cfgAll.getClntVrf();
            cfgIfc ifc = cfgAll.getClntIfc();
            int timeout = 1000;
            int min = 1024;
            int max = 2048;
            int ttl = -1;
            int tos = -1;
            int proto = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("vrf")) {
                    vrf = cfgAll.vrfFind(cmd.word(), false);
                    ifc = null;
                    continue;
                }
                if (a.equals("source")) {
                    ifc = cfgAll.ifcFind(cmd.word(), 0);
                    continue;
                }
                if (a.equals("ipv4")) {
                    proto = 4;
                    continue;
                }
                if (a.equals("ipv6")) {
                    proto = 6;
                    continue;
                }
                if (a.equals("timeout")) {
                    timeout = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("min")) {
                    min = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("max")) {
                    max = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("ttl")) {
                    ttl = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("tos")) {
                    tos = bits.str2num(cmd.word());
                    continue;
                }
            }
            if (vrf == null) {
                cmd.error("vrf not specified");
                return null;
            }
            addrIP trg = clntDns.resolveAddr(cmd.pipe, rem, proto);
            if (trg == null) {
                return null;
            }
            if (timeout < 1) {
                timeout = 1;
            }
            addrIP src = null;
            if (ifc != null) {
                src = ifc.getLocAddr(trg);
            }
            clntPorts trc = new clntPorts();
            trc.vrf = vrf;
            trc.ifc = ifc;
            trc.trg = trg;
            trc.ttl = ttl;
            trc.tos = tos;
            trc.tim = timeout;
            cmd.pipe.linePut("scanning " + trg + " " + prt + ", src=" + src + ", vrf=" + vrf.name + ", ttl=" + ttl + ", tos=" + tos + ", ran=" + min + ".." + max + ", tim=" + timeout);
            for (int i = min; i < max; i++) {
                if (need2stop()) {
                    break;
                }
                cmd.pipe.strPut("" + i);
                cmd.pipe.blockingPut(pipeSide.getEnding(pipeSide.modTyp.modeCR), 0, 1);
                if (trc.testOne(i, prt)) {
                    continue;
                }
                cmd.pipe.linePut("open from " + i);
            }
            return null;
        }
        if (a.equals("txt2sum")) {
            a = cmd.word();
            cmd.error("reading " + a);
            List<String> txt = bits.txt2buf(a);
            if (txt == null) {
                cmd.error("error reading file");
                return null;
            }
            List<packHolder> pcks = rtrBgpDump.logs2pcks(txt);
            int o = pcks.size();
            cmd.error(o + " dumps found");
            if (o < 1) {
                cmd.error("no dumps found");
                return null;
            }
            packHolder tmp = new packHolder(true, true);
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (int i = 0; i < o; i++) {
                packHolder pck = pcks.get(i);
                txt = rtrBgpDump.dumpPacketSum(spk, vrf.core4, vrf.core6, tmp, pck, null);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            return null;
        }
        if (a.equals("txt2full")) {
            a = cmd.word();
            cmd.error("reading " + a);
            List<String> txt = bits.txt2buf(a);
            if (txt == null) {
                cmd.error("error reading file");
                return null;
            }
            List<packHolder> pcks = rtrBgpDump.logs2pcks(txt);
            int o = pcks.size();
            cmd.error(o + " dumps found");
            if (o < 1) {
                cmd.error("no dumps found");
                return null;
            }
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
            packHolder tmp = new packHolder(true, true);
            for (int i = 0; i < o; i++) {
                packHolder pck = pcks.get(i);
                txt = rtrBgpDump.dumpPacketFull(spk, vrf.core4, vrf.core6, ses, tmp, pck);
                txt.add("");
                rdr.putStrArr(txt);
            }
            return null;
        }
        if (a.equals("txt2mrt")) {
            a = cmd.word();
            cmd.error("reading " + a);
            List<String> lst = bits.txt2buf(a);
            if (lst == null) {
                cmd.error("error reading file");
                return null;
            }
            List<packHolder> pcks = rtrBgpDump.logs2pcks(lst);
            int o = pcks.size();
            cmd.error(o + " dumps found");
            if (o < 1) {
                cmd.error("no dumps found");
                return null;
            }
            a = cmd.word();
            cmd.error("writing " + a);
            RandomAccessFile f;
            try {
                f = new RandomAccessFile(new File(a), "rw");
                f.setLength(0);
            } catch (Exception e) {
                return null;
            }
            int ok = 0;
            for (int i = 0; i < o; i++) {
                packHolder pck = pcks.get(i);
                byte[] b = new byte[128];
                int p = rtrBgpMrt.putMrtHeader(b, pck.INTtime, false, 0, 0, pck.IPsrc, pck.IPtrg, pck.dataSize());
                try {
                    f.write(b, 0, p);
                    f.write(pck.getCopy());
                    ok++;
                } catch (Exception e) {
                }
            }
            try {
                f.close();
            } catch (Exception e) {
            }
            cmd.error(ok + " packets converted");
            return null;
        }
        if (a.equals("ris2flt")) {
            encUrl src = new encUrl();
            if (src.fromString(cmd.word())) {
                cmd.error("bad url");
                return null;
            }
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            clntHttp htp = new clntHttp(cmd.pipe, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
            if (htp.doConnect(src)) {
                cmd.error("unable to connect ris");
                return null;
            }
            clntRis ris = new clntRis(htp.pipe, new addrIP());
            ris.clntConnect(src);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            List<String> txt;
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (;;) {
                if (need2stop()) {
                    break;
                }
                int i = ris.readPacket(pck);
                if (i == 1) {
                    break;
                }
                if (i != 0) {
                    continue;
                }
                txt = rtrBgpDump.dumpPacketSum(spk, vrf.core4, vrf.core6, tmp, pck, trg);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            htp.pipe.setClose();
            return null;
        }
        if (a.equals("ris2con")) {
            encUrl src = new encUrl();
            if (src.fromString(cmd.word())) {
                cmd.error("bad url");
                return null;
            }
            clntHttp htp = new clntHttp(cmd.pipe, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
            if (htp.doConnect(src)) {
                cmd.error("unable to connect ris");
                return null;
            }
            clntRis ris = new clntRis(htp.pipe, new addrIP());
            ris.clntConnect(src);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            List<String> txt;
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (;;) {
                if (need2stop()) {
                    break;
                }
                int i = ris.readPacket(pck);
                if (i == 1) {
                    break;
                }
                if (i != 0) {
                    continue;
                }
                txt = rtrBgpDump.dumpPacketSum(spk, vrf.core4, vrf.core6, tmp, pck, null);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            htp.pipe.setClose();
            return null;
        }
        if (a.equals("ris2bmp")) {
            encUrl src = new encUrl();
            if (src.fromString(cmd.word())) {
                cmd.error("bad url");
                return null;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy profile");
                return null;
            }
            addrIP adr = clntDns.justResolv(cmd.word(), prx.proxy.prefer);
            if (adr == null) {
                cmd.error("unable to resolve bmp");
                return null;
            }
            pipeSide pipe = prx.proxy.doConnect(servGeneric.protoTcp, adr, bits.str2num(cmd.word()), "ris2bmp");
            if (pipe == null) {
                cmd.error("unable to connect bmp");
                return null;
            }
            clntHttp htp = new clntHttp(cmd.pipe, cfgAll.getClntPrx(cfgAll.httpProxy), null, false);
            if (htp.doConnect(src)) {
                cmd.error("unable to connect ris");
                return null;
            }
            clntRis ris = new clntRis(htp.pipe, new addrIP());
            ris.clntConnect(src);
            packHolder pck = new packHolder(true, true);
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                if (need2stop()) {
                    break;
                }
                int i = ris.readPacket(pck);
                if (i == 1) {
                    break;
                }
                if (i != 0) {
                    continue;
                }
                rtrBgpMon.createHeader(pck, pck.INTtime + cfgAll.timeServerOffset, false, rtrBgpMon.typMon, pck.IPsrc, pck.INTiface, pck.IPsrc.toIPv4());
                pck.pipeSend(pipe, 0, pck.dataSize(), 1);
            }
            htp.pipe.setClose();
            pipe.setClose();
            return null;
        }
        if (a.equals("pmtud")) {
            String rem = cmd.word();
            cfgVrf vrf = cfgAll.getClntVrf();
            cfgIfc ifc = cfgAll.getClntIfc();
            addrIP via = null;
            int data = 0;
            int timeout = 1000;
            int timemax = 15000;
            int timediv = 10;
            int sgt = 0;
            int tos = 0;
            int flow = 0;
            int alrt = -1;
            int ttl = 255;
            int proto = 0;
            int delay = 1000;
            int min = 1400;
            int max = 1600;
            int mpls = 0;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("icmp")) {
                    mpls = 0;
                    continue;
                }
                if (a.equals("bck-icmp")) {
                    mpls = 1;
                    continue;
                }
                if (a.equals("mpls")) {
                    mpls = 2;
                    continue;
                }
                if (a.equals("bck-mpls")) {
                    mpls = 3;
                    continue;
                }
                if (a.equals("bier")) {
                    mpls = 4;
                    continue;
                }
                if (a.equals("bck-bier")) {
                    mpls = 5;
                    continue;
                }
                if (a.equals("data")) {
                    data = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("alert")) {
                    alrt = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("vrf")) {
                    vrf = cfgAll.vrfFind(cmd.word(), false);
                    ifc = null;
                    continue;
                }
                if (a.equals("source")) {
                    ifc = cfgAll.ifcFind(cmd.word(), 0);
                    continue;
                }
                if (a.equals("viahop")) {
                    via = new addrIP();
                    via.fromString(cmd.word());
                    continue;
                }
                if (a.equals("timeout")) {
                    timeout = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("timediv")) {
                    timediv = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("timemax")) {
                    timemax = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("delay")) {
                    delay = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("min")) {
                    min = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("max")) {
                    max = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("ttl")) {
                    ttl = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("sgt")) {
                    sgt = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("tos")) {
                    tos = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("flow")) {
                    flow = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("ipv4")) {
                    proto = 4;
                    continue;
                }
                if (a.equals("ipv6")) {
                    proto = 6;
                    continue;
                }
            }
            addrIP trg = clntDns.resolveAddr(cmd.pipe, rem, proto);
            if (trg == null) {
                return null;
            }
            addrIP src = null;
            if (ifc != null) {
                src = ifc.getLocAddr(trg);
            }
            ipFwd fwd = vrf.getFwd(trg);
            if (timeout < 1) {
                timeout = 1;
            }
            clntPmtud pm = new clntPmtud(cmd.pipe, trg, fwd, src);
            pm.mpls = mpls;
            pm.hop = via;
            pm.data = data;
            pm.timeout = timeout;
            pm.timediv = timediv;
            pm.timemax = timemax;
            pm.sgt = sgt;
            pm.tos = tos;
            pm.flow = flow;
            pm.alrt = alrt;
            pm.ttl = ttl;
            pm.delay = delay;
            pm.min = min;
            pm.max = max;
            pm.doer();
            return null;
        }
        if (a.equals("arping")) {
            String rem = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            int repeat = 5;
            int proto = 0;
            int delay = 1000;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("delay")) {
                    delay = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("repeat")) {
                    repeat = bits.str2num(cmd.word());
                    continue;
                }
                if (a.equals("ipv4")) {
                    proto = 4;
                    continue;
                }
                if (a.equals("ipv6")) {
                    proto = 6;
                    continue;
                }
            }
            if (delay < 1) {
                delay = 1;
            }
            addrIP trg = clntDns.resolveAddr(cmd.pipe, rem, proto);
            if (trg == null) {
                return null;
            }
            ipIfc ipi;
            if (trg.isIPv4()) {
                ipi = ifc.ipIf4;
            } else {
                ipi = ifc.ipIf6;
            }
            if (ipi == null) {
                cmd.error("protocol not enabled");
                return null;
            }
            addrIP src = ifc.getLocAddr(trg);
            if (src == null) {
                cmd.error("no address configured");
                return null;
            }
            int sent = 0;
            int recv = 0;
            long timBeg = bits.getTime();
            cmd.error("arpinging " + trg + ", src=" + ifc.name + ", cnt=" + repeat + ", gap=" + delay);
            prtArping ap = new prtArping(ipi, trg);
            ap.delay = delay;
            for (int i = 0; i < repeat; i++) {
                if (need2stop()) {
                    break;
                }
                sent++;
                addrType[] ress = ap.doRound();
                if (ress == null) {
                    cmd.error("timeout");
                    continue;
                }
                cmd.error("reply from " + ress[1]);
                recv++;
            }
            cmd.error("result=" + bits.percent(recv, sent) + "%, recv/sent/lost=" + recv + "/" + sent + "/" + (sent - recv) + ", took " + (bits.getTime() - timBeg));
            return null;
        }
        if (a.equals("openflow")) {
            servOpenflow srv = cfgAll.srvrFind(new servOpenflow(), cfgAll.dmnOpenflow, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            int cnt = bits.str2num(cmd.word());
            int ifc = bits.str2num(cmd.word());
            packHolder pck = new packHolder(true, true);
            pck.ETHtrg.fromString(cmd.word());
            pck.ETHsrc.fromString(cmd.word());
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                pck.putByte(0, bits.fromHex(a));
                pck.putSkip(1);
                pck.merge2end();
            }
            cmd.error("sending cnt=" + cnt + " ifc=" + ifc + " adr=" + pck.ETHsrc + " -> " + pck.ETHtrg + " pck=" + pck.dump());
            srv.send2apiPack(cnt, ifc, pck);
            return null;
        }
        if (a.equals("p4lang")) {
            servP4lang srv = cfgAll.srvrFind(new servP4lang(), cfgAll.dmnP4lang, cmd.word());
            if (srv == null) {
                cmd.error("no such server");
                return null;
            }
            int cnt = bits.str2num(cmd.word());
            int ifc = bits.str2num(cmd.word());
            packHolder pck = new packHolder(true, true);
            pck.ETHtrg.fromString(cmd.word());
            pck.ETHsrc.fromString(cmd.word());
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                pck.putByte(0, bits.fromHex(a));
                pck.putSkip(1);
                pck.merge2end();
            }
            cmd.error("sending cnt=" + cnt + " ifc=" + ifc + " adr=" + pck.ETHsrc + " -> " + pck.ETHtrg + " pck=" + pck.dump());
            srv.send2apiPack(cnt, ifc, pck);
            return null;
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
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            cmd.error("converting");
            ipCor4 ic4 = new ipCor4();
            ipCor6 ic6 = new ipCor6();
            tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
            int pk = 0;
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                rtrBgpDump.msg2pcap(ic4, ic6, ses, pck);
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
            cmd.error(pk + " packets (" + ses.size() + " streams) converted");
            return null;
        }
        if (a.equals("mrt2stat")) {
            RandomAccessFile fs = null;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
            }
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            int[] afi = new int[3];
            int[] atr = new int[256];
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                rtrBgpDump.dumpPacketStat(pck, hlp, afi, atr);
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            cmd.error(afi[0] + " other, " + afi[1] + " ipv4 and " + afi[2] + " ipv6");
            for (int i = 0; i < atr.length; i++) {
                if (atr[i] < 1) {
                    continue;
                }
                cmd.error(atr[i] + " " + rtrBgpUtil.attrType2string(i));
            }
            return null;
        }
        if (a.equals("mrt2sum")) {
            RandomAccessFile fs = null;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
            }
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            List<String> txt;
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                txt = rtrBgpDump.dumpPacketSum(spk, vrf.core4, vrf.core6, tmp, pck, null);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            return null;
        }
        if (a.equals("mrt2flt")) {
            a = cmd.word();
            addrIP trg = new addrIP();
            if (trg.fromString(cmd.word())) {
                cmd.error("bad address");
                return null;
            }
            RandomAccessFile fs = null;
            try {
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
            }
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            List<String> txt;
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                txt = rtrBgpDump.dumpPacketSum(spk, vrf.core4, vrf.core6, tmp, pck, trg);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            return null;
        }
        if (a.equals("mrt2full")) {
            RandomAccessFile fs = null;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
            }
            tabGen<tabSessionEntry> ses = new tabGen<tabSessionEntry>();
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            List<String> txt;
            cfgVrf vrf = new cfgVrf("bgp");
            vrf.allocThisVrf();
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                txt = rtrBgpDump.dumpPacketFull(spk, vrf.core4, vrf.core6, ses, tmp, pck);
                if (txt.size() < 1) {
                    continue;
                }
                rdr.putStrArr(txt);
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
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
            RandomAccessFile fs = null;
            RandomAccessFile ft = null;
            try {
                a = cmd.word();
                cmd.error("opening source " + a);
                fs = new RandomAccessFile(new File(a), "r");
            } catch (Exception e) {
            }
            try {
                a = cmd.word();
                cmd.error("opening target " + a);
                ft = new RandomAccessFile(new File(a), "rw");
                ft.setLength(0);
            } catch (Exception e) {
            }
            addrIP sip = new addrIP();
            sip.fromString(cmd.word());
            addrIP tip = new addrIP();
            tip.fromString(cmd.word());
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            int mat = 0;
            int snt = 0;
            int tot = 0;
            rtrBgpSpeak spk = new rtrBgpSpeak(rp.bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            for (;;) {
                long fp;
                try {
                    fp = fs.getFilePointer();
                } catch (Exception e) {
                    break;
                }
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                int typ = pck.getByte(rtrBgpUtil.sizeU - 1);
                pck.getSkip(rtrBgpUtil.sizeU);
                tot++;
                if (sip.compareTo(pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compareTo(pck.IPtrg) != 0) {
                    continue;
                }
                if (need2stop()) {
                    break;
                }
                nei.conn.currChg = 0;
                switch (typ) {
                    case rtrBgpUtil.msgUpdate:
                        nei.conn.parseUpdate(pck, tmp);
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
                    break;
                }
            }
            try {
                fs.close();
            } catch (Exception e) {
            }
            try {
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
            rtrBgpSpeak spk = new rtrBgpSpeak(rp.bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                int typ = pck.getByte(rtrBgpUtil.sizeU - 1);
                pck.getSkip(rtrBgpUtil.sizeU);
                tot++;
                if (sip.compareTo(pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compareTo(pck.IPtrg) != 0) {
                    continue;
                }
                if (need2stop()) {
                    break;
                }
                switch (typ) {
                    case rtrBgpUtil.msgUpdate:
                        nei.conn.parseUpdate(pck, tmp);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            rtrBgp bgp = new rtrBgp(vrf.fwd4, vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, new addrIP());
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, null, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            int snt = 0;
            int tot = 0;
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                tot++;
                if (sip.compareTo(pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compareTo(pck.IPtrg) != 0) {
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            rtrBgp bgp = new rtrBgp(vrf.getFwd(trg), vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, trg);
            nei.localAs = las;
            nei.addrFams = rtrBgpParam.boolsSet(false);
            nei.addrFams[bgp.safi2idx(safi)] = true;
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, strm, 0);
            packHolder pck = new packHolder(true, true);
            packHolder tmp = new packHolder(true, true);
            packHolder hlp = new packHolder(true, true);
            spk.sendOpen();
            spk.sendKeepAlive();
            cmd.error("sending updates as it was from " + sip + " to " + tip);
            int snt = 0;
            int tot = 0;
            for (;;) {
                int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
                if (i == 1) {
                    break;
                }
                if (i == 2) {
                    continue;
                }
                if (pck.getByte(rtrBgpUtil.sizeU - 1) != rtrBgpUtil.msgUpdate) {
                    continue;
                }
                pck.getSkip(rtrBgpUtil.sizeU);
                tot++;
                if (sip.compareTo(pck.IPsrc) != 0) {
                    continue;
                }
                if (tip.compareTo(pck.IPtrg) != 0) {
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            List<tabRouteBlob> blbs = new ArrayList<tabRouteBlob>();
            for (;;) {
                tabRouteBlob blb = new tabRouteBlob();
                if (blb.fromString(cmd)) {
                    break;
                }
                blbs.add(blb);
                cmd.error("will send " + blb);
            }
            pipeSide strm = null;
            for (;;) {
                cmd.error("connecting " + trg);
                clntProxy prx = clntProxy.makeTemp(vrf, ifc);
                strm = prx.doConnect(servGeneric.protoTcp, trg, rtrBgp.port, "bgpattr");
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
            rtrBgp bgp = new rtrBgp(vrf.getFwd(trg), vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, trg);
            nei.localAs = las;
            int safi;
            if (prf.network.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            nei.addrFams = rtrBgpParam.boolsSet(false);
            int idx = bgp.safi2idx(safi);
            nei.addrFams[idx] = true;
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, strm, 0);
            packHolder pck = new packHolder(true, true);
            spk.sendOpen();
            spk.sendKeepAlive();
            cmd.error("sending " + prf + " network");
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.best.unknown = blbs;
            ntry.prefix = prf.copyBytes();
            rmp.roumap.update(rtrBgpUtil.sfiUnicast, 0, ntry, false);
            ntry.best.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
            packHolder tmp = new packHolder(true, true);
            cmd.error("sending update");
            pck.clear();
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            lst.add(ntry);
            rtrBgpUtil.createReachable(spk, pck, tmp, idx, safi, false, false, lst);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            int stp = bits.str2num(cmd.word());
            if (stp < 1) {
                stp = 1;
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
            rtrBgp bgp = new rtrBgp(vrf.getFwd(trg), vrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, trg);
            nei.localAs = las;
            int safi;
            if (prf.network.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            nei.addrFams = rtrBgpParam.boolsSet(false);
            int idx = bgp.safi2idx(safi);
            nei.addrFams[idx] = true;
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, strm, 0);
            packHolder pck = new packHolder(true, true);
            spk.sendOpen();
            spk.sendKeepAlive();
            cmd.error("sending " + num + " random " + prf + " networks");
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
            ntry.prefix = prf.copyBytes();
            rmp.roumap.update(rtrBgpUtil.sfiUnicast, 0, ntry, false);
            ntry.best.nextHop = ifc.getFwdIfc(trg).addr.copyBytes();
            packHolder tmp = new packHolder(true, true);
            for (int o = 0; o < num; o += stp) {
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
                lst.clear();
                pck.clear();
                for (i = 0; i < stp; i++) {
                    addrIP adr = new addrIP();
                    adr.fillRandom();
                    adr.setAnd(adr, prf.wildcard);
                    adr.setOr(adr, prf.network);
                    ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                    lst.add(ntry.copyBytes(tabRoute.addType.ecmp));
                }
                rtrBgpUtil.createReachable(spk, pck, tmp, idx, safi, false, false, lst);
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
                if (qos.checkPacket(pck)) {
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
                ifc = cfgAll.ifcFind(cmd.word(), 0);
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
                prtTcp.createTCPheader(pck, -1, null, null);
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
                    if (qos.checkPacket(pck)) {
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
                if (qos.checkPacket(pck)) {
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
            pipeRelay trm = new pipeRelay(pip, usr, null);
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
                pipeRelay trm = new pipeRelay(pip, usr, null);
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
            pipeRelay trm = new pipeRelay(pip, sm.getPipe(), null);
            trm.doTerm();
            sm.callStop();
            return null;
        }
        if (a.equals("capture")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            a += ".pcap";
            if (cfgAll.capturePath != null) {
                a = cfgAll.capturePath + a;
            }
            if (cmd.size() > 0) {
                a = cmd.word();
            }
            cmd.error("capturing=" + !ifc.ethtyp.initLog(a));
            return null;
        }
        if (a.equals("monitor")) {
            a = cmd.word();
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return null;
            }
            ifcEthTyp oldS = ifc.ethtyp.monSes;
            boolean oldH = ifc.ethtyp.monHdr;
            a = cmd.word();
            if (a.length() > 0) {
                cfgIfc trg = cfgAll.ifcFind(a, 0);
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
            cfgIfc ifc = cfgAll.ifcFind(a, 0);
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
            return null;
        }
        if (a.equals("wakeup")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
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
        if (a.equals("xotpad")) {
            addrIP adr = clntDns.justResolv(cmd.word(), 0);
            if (adr == null) {
                cmd.error("bad address");
                return null;
            }
            pipeSide con = cfgAll.getClntPrx(null).doConnect(servGeneric.protoTcp, adr, packXotPad.port, "xotpad");
            if (con == null) {
                cmd.error("error connecting");
                return null;
            }
            clntXotPad pad = new clntXotPad(con);
            String b = cmd.word();
            if (pad.startWork(b, cmd.word())) {
                cmd.error("error calling");
                return null;
            }
            pipeRelay trm = new pipeRelay(pip, pad.getPipe(), null);
            trm.doTerm();
            return null;
        }
        if (a.equals("websock")) {
            encUrl url = new encUrl();
            if (url.fromString(cmd.word())) {
                cmd.error("bad url");
                return null;
            }
            pipeSide strm = secWebsock.doConnect(cfgAll.getClntPrx(null), null, url, cmd.getRemaining());
            if (strm == null) {
                cmd.error("failed to connect");
                return null;
            }
            secWebsock ws = new secWebsock(strm, new pipeLine(65536, false));
            ws.startClient();
            pipeRelay trm = new pipeRelay(pip, ws.getPipe(), null);
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
            addrIP trg = clntDns.resolveAddr(cmd.pipe, a, 0);
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
            conn = secClient.startSecurity(cmd.pipe, conn, servGeneric.protoSsh, null, a, cmd.word());
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
            if (cmd.pipe.settingsGet(pipeSetting.passStar, false)) {
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
