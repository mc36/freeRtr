package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrClns;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrIsis;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * isis lsp dumper
 *
 * @author matecsaba
 */
public class rtrIsisDump {

    private rtrIsisDump() {
    }

    /**
     * dump lsp
     *
     * @param l list to append
     * @param pck lsp contents
     */
    public static void dumpLsp(List<String> l, packHolder pck) {
        pck.getSkip(rtrIsisLsp.headSize);
        encTlv tlv = rtrIsis.getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrIsisLsp.tlvAreaAddr:
                    String a = "";
                    for (int i = 0; i < tlv.valSiz;) {
                        addrClns adrC = new addrClns();
                        adrC.fromBuf(tlv.valDat, i);
                        a += " " + adrC;
                        i += tlv.valDat[i] + 1;
                    }
                    l.add("area addresses:" + a);
                    break;
                case rtrIsisLsp.tlvIsNeigh:
                    for (int i = 1; i < tlv.valSiz; i += 11) {
                        addrIsis adrI = new addrIsis();
                        adrI.fromBuf(tlv.valDat, i + 4);
                        l.add("is neighbor: metric=" + tlv.valDat[i] + " systemid=" + adrI);
                    }
                    break;
                case rtrIsisLsp.tlvEsNeigh:
                    for (int i = 1; i < tlv.valSiz; i += 10) {
                        addrIsis adrI = new addrIsis();
                        adrI.fromBuf(tlv.valDat, i + 4);
                        l.add("es neighbor: metric=" + tlv.valDat[i] + " neighborid=" + adrI);
                    }
                    break;
                case rtrIsisLsp.tlvInstanceId:
                    a = " iid=" + bits.msbGetW(tlv.valDat, 0) + " itids=";
                    for (int i = 2; i < tlv.valSiz; i += 2) {
                        a += " " + bits.msbGetW(tlv.valDat, i);
                    }
                    l.add("instance id:" + a);
                    break;
                case rtrIsisLsp.tlvAuthen:
                    l.add("authentication: " + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                    break;
                case rtrIsisLsp.tlvExtSeq:
                    l.add("sequence number: session=" + bits.msbGetQ(tlv.valDat, 0) + " packet=" + bits.msbGetD(tlv.valDat, 8));
                    break;
                case rtrIsisLsp.tlvChecksum:
                    l.add("checksum: " + bits.msbGetW(tlv.valDat, 0));
                    break;
                case rtrIsisLsp.tlvPurgeOrig:
                    addrIsis adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    l.add("purge originator: " + adrI);
                    break;
                case rtrIsisLsp.tlvLspBufSiz:
                    l.add("buffer size: " + bits.msbGetW(tlv.valDat, 0));
                    break;
                case rtrIsisLsp.tlvExtIsNeigh:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    l.add("ext is reach: " + adrI + " metric=" + (bits.msbGetD(tlv.valDat, 7) >>> 8));
                    dumpExtIsReach(l, tlv, 11);
                    break;
                case rtrIsisLsp.tlvIsNeighAttr:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    l.add("is neigh attr: " + adrI + " metric=" + (bits.msbGetD(tlv.valDat, 7) >>> 8));
                    dumpExtIsReach(l, tlv, 11);
                    break;
                case rtrIsisLsp.tlvIsAlias:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    l.add("is alias: " + adrI);
                    break;
                case rtrIsisLsp.tlvIpv4intReach:
                    for (int i = 0; i < tlv.valSiz; i += 12) {
                        addrIPv4 adr1 = new addrIPv4();
                        addrIPv4 adr2 = new addrIPv4();
                        adr1.fromBuf(tlv.valDat, i + 4);
                        adr2.fromBuf(tlv.valDat, i + 8);
                        l.add("ipv4 internal: " + adr1 + "/" + adr2 + " metric=" + tlv.valDat[i]);
                    }
                    break;
                case rtrIsisLsp.tlvIpv4extReach:
                    for (int i = 0; i < tlv.valSiz; i += 12) {
                        addrIPv4 adr1 = new addrIPv4();
                        addrIPv4 adr2 = new addrIPv4();
                        adr1.fromBuf(tlv.valDat, i + 4);
                        adr2.fromBuf(tlv.valDat, i + 8);
                        l.add("ipv4 external: " + adr1 + "/" + adr2 + " metric=" + tlv.valDat[i]);
                    }
                    break;
                case rtrIsisLsp.tlvProtSupp:
                    a = "";
                    for (int i = 0; i < tlv.valSiz; i++) {
                        switch (tlv.valDat[i] & 0xff) {
                            case ipCor4.protocolNLPID:
                                a += " ipv4";
                                break;
                            case ipCor6.protocolNLPID:
                                a += " ipv6";
                                break;
                            default:
                                a += " unknown=" + tlv.valDat[i];
                                break;
                        }
                    }
                    l.add("protocol support:" + a);
                    break;
                case rtrIsisLsp.tlvIpv4addr:
                    a = "";
                    for (int i = 0; i < tlv.valSiz; i += 4) {
                        addrIPv4 adr4 = new addrIPv4();
                        adr4.fromBuf(tlv.valDat, i);
                        a += " " + adr4;
                    }
                    l.add("ipv4 int addr:" + a);
                    break;
                case rtrIsisLsp.tlvIpv4teId:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("ipv4 te id:" + adr4);
                    break;
                case rtrIsisLsp.tlvExtIpv4reach:
                    for (int i = 0; i < tlv.valSiz;) {
                        int met = bits.msbGetD(tlv.valDat, i);
                        i += 4;
                        int flg = tlv.valDat[i] & 0xff;
                        int len = flg & 0x3f;
                        i++;
                        adr4 = new addrIPv4();
                        adr4.fromBuf(tlv.valDat, i);
                        i += (len + 7) / 8;
                        addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr4, len);
                        l.add("ext ipv4 reach: metric=" + met + " " + prf);
                        if ((flg & 0x40) == 0) {
                            continue;
                        }
                        len = tlv.valDat[i] & 0xff;
                        i++;
                        dumpExtAddrReach(l, tlv, i, len);
                        i += len;
                    }
                    break;
                case rtrIsisLsp.tlvHostName:
                    l.add("hostname: " + tlv.getStr());
                    break;
                case rtrIsisLsp.tlvIpv6teId:
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("ipv6 te id:" + adr6);
                    break;
                case rtrIsisLsp.tlvMtIsNeigh:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 2);
                    l.add("mt is reach: " + adrI + " metric=" + (bits.msbGetD(tlv.valDat, 9) >>> 8) + " mtid=" + bits.msbGetW(tlv.valDat, 0));
                    dumpExtIsReach(l, tlv, 13);
                    break;
                case rtrIsisLsp.tlvMtNeighAttr:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 2);
                    l.add("mt is attr: " + adrI + " metric=" + (bits.msbGetD(tlv.valDat, 9) >>> 8) + " mtid=" + bits.msbGetW(tlv.valDat, 0));
                    dumpExtIsReach(l, tlv, 13);
                    break;
                case rtrIsisLsp.tlvMultiTopo:
                    a = "";
                    for (int i = 0; i < tlv.valSiz; i += 2) {
                        a += " " + bits.msbGetW(tlv.valDat, i);
                    }
                    l.add("mt id: " + a);
                    break;
                case rtrIsisLsp.tlvIpv6addr:
                    a = "";
                    for (int i = 0; i < tlv.valSiz; i += 16) {
                        adr6 = new addrIPv6();
                        adr6.fromBuf(tlv.valDat, i);
                        a += " " + adr6;
                    }
                    l.add("ipv6 int addr:" + a);
                    break;
                case rtrIsisLsp.tlvIpv4srlg:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 8);
                    addrIPv4 adr4b = new addrIPv4();
                    adr4b.fromBuf(tlv.valDat, 12);
                    a = "";
                    for (int i = 16; i < tlv.valSiz; i += 4) {
                        a += " " + bits.msbGetD(tlv.valDat, i);
                    }
                    l.add("ipv4 srlg: systemid=" + adrI + " local=" + adr4 + " remote=" + adr4b + " srlg=" + a);
                    break;
                case rtrIsisLsp.tlvIpv6srlg:
                    adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 0);
                    adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 8);
                    addrIPv6 adr6b = new addrIPv6();
                    adr6b.fromBuf(tlv.valDat, 24);
                    a = "";
                    for (int i = 40; i < tlv.valSiz; i += 4) {
                        a += " " + bits.msbGetD(tlv.valDat, i);
                    }
                    l.add("ipv6 srlg: systemid=" + adrI + " local=" + adr6 + " remote=" + adr6b + " srlg=" + a);
                    break;
                case rtrIsisLsp.tlvMtIpv4reach:
                    for (int i = 2; i < tlv.valSiz;) {
                        int met = bits.msbGetD(tlv.valDat, i);
                        i += 4;
                        int flg = tlv.valDat[i] & 0xff;
                        int len = flg & 0x3f;
                        i++;
                        adr4 = new addrIPv4();
                        adr4.fromBuf(tlv.valDat, i);
                        i += (len + 7) / 8;
                        addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr4, len);
                        l.add("mt ipv4 reach: metric=" + met + " " + prf + " mtid=" + bits.msbGetW(tlv.valDat, 0));
                        if ((flg & 0x40) == 0) {
                            continue;
                        }
                        len = tlv.valDat[i] & 0xff;
                        i++;
                        dumpExtAddrReach(l, tlv, i, len);
                        i += len;
                    }
                    break;
                case rtrIsisLsp.tlvIpv6reach:
                    for (int i = 0; i < tlv.valSiz;) {
                        int met = bits.msbGetD(tlv.valDat, i);
                        i += 4;
                        int flg = tlv.valDat[i] & 0xff;
                        int len = tlv.valDat[i + 1] & 0xff;
                        i += 2;
                        adr6 = new addrIPv6();
                        adr6.fromBuf(tlv.valDat, i);
                        i += (len + 7) / 8;
                        addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr6, len);
                        l.add("ipv6 reach: metric=" + met + " " + prf);
                        if ((flg & 0x20) == 0) {
                            continue;
                        }
                        len = tlv.valDat[i] & 0xff;
                        i++;
                        dumpExtAddrReach(l, tlv, i, len);
                        i += len;
                    }
                    break;
                case rtrIsisLsp.tlvMtIpv6reach:
                    for (int i = 2; i < tlv.valSiz;) {
                        int met = bits.msbGetD(tlv.valDat, i);
                        i += 4;
                        int flg = tlv.valDat[i] & 0xff;
                        int len = tlv.valDat[i + 1] & 0xff;
                        i += 2;
                        adr6 = new addrIPv6();
                        adr6.fromBuf(tlv.valDat, i);
                        i += (len + 7) / 8;
                        addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr6, len);
                        l.add("mt ipv6 reach: metric=" + met + " " + prf + " mtid=" + bits.msbGetW(tlv.valDat, 0));
                        if ((flg & 0x20) == 0) {
                            continue;
                        }
                        len = tlv.valDat[i] & 0xff;
                        i++;
                        dumpExtAddrReach(l, tlv, i, len);
                        i += len;
                    }
                    break;
                case rtrIsisLsp.tlvRouterCapa:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("router capability: id=" + adr4);
                    dumpRtrCapa(l, tlv, 5);
                    break;
                case rtrIsisLsp.tlvSegRoutV6:
                    l.add("srv6: mtid=" + bits.msbGetW(tlv.valDat, 0));
                    for (int i = 2; i < tlv.valSiz;) {
                        int len = tlv.valDat[i + 6] & 0xff;
                        adr6 = new addrIPv6();
                        adr6.fromBuf(tlv.valDat, i + 7);
                        addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr6, len);
                        l.add("  locator: " + prf + " metric=" + bits.msbGetD(tlv.valDat, i) + " flag=" + (tlv.valDat[i + 4] & 0xff) + " algo=" + (tlv.valDat[i + 5] & 0xff));
                        i += 7;
                        i += (len + 7) / 8;
                        i += tlv.valDat[i + 0] & 0xff;
                        i += 1;
                    }
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpExtIsReach(List<String> l, encTlv tlv, int ofs) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, ofs, 0, tlv.valSiz - ofs);
        pck.putSkip(tlv.valSiz - ofs);
        pck.merge2beg();
        tlv = rtrIsis.getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrIsisTe.typAdminGrp:
                    l.add("  admin group: " + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typBothId:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    addrIPv4 adr4b = new addrIPv4();
                    adr4b.fromBuf(tlv.valDat, 4);
                    l.add("  interface id: " + adr4 + " " + adr4b);
                    break;
                case rtrIsisTe.typLoc4adr:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("  ipv4 local: " + adr4);
                    break;
                case rtrIsisTe.typRem4adr:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("  ipv4 remote: " + adr4);
                    break;
                case rtrIsisTe.typMaxBndwdt:
                    l.add("  max bandwidth: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typMaxReserv:
                    l.add("  max reservable: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typUnReserv:
                    l.add("  unreserved: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typLoc6adr:
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("  ipv6 local: " + adr6);
                    break;
                case rtrIsisTe.typRem6adr:
                    adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("  ipv6 remote: " + adr6);
                    break;
                case rtrIsisTe.typExtAdmin:
                    l.add("  ext admin group: " + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                    break;
                case rtrIsisTe.typLinkMsd:
                    String a = "";
                    for (int i = 0; i < tlv.valSiz; i += 2) {
                        a += " typ=" + tlv.valDat[i] + " len=" + tlv.valDat[i + 1];
                    }
                    l.add("  link msd: " + a);
                    break;
                case rtrIsisTe.typMetric:
                    l.add("  te metric: " + (bits.msbGetD(tlv.valDat, 0) >>> 8));
                    break;
                case rtrIsisTe.typLnkAttr:
                    l.add("  link attrib: " + bits.toHexW(bits.msbGetW(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typLnkProt:
                    l.add("  link protect: " + bits.toHexW(bits.msbGetW(tlv.valDat, 0)));
                    break;
                case rtrIsisTe.typSwchCapa:
                    l.add("  switch capa: sc=" + tlv.valDat[0] + " enc=" + tlv.valDat[1]);
                    break;
                case rtrIsisTe.typBndwdtCnst:
                    l.add("  bandwidth constrain: " + tlv.valDat[0]);
                    break;
                case rtrIsisTe.typUncnstLsp:
                    l.add("  lsp cunt: " + bits.msbGetW(tlv.valDat, 0));
                    break;
                case rtrIsisTe.typASnum:
                    l.add("  as number: " + bits.msbGetD(tlv.valDat, 0));
                    break;
                case rtrIsisTe.typAsbr4id:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("  ipv4 asbr: " + adr4);
                    break;
                case rtrIsisTe.typAsbr6id:
                    adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("  ipv6 asbr: " + adr6);
                    break;
                case rtrIsisTe.typIntAdjCapa:
                    l.add("  intf adjust capa: lsc=" + tlv.valDat[0] + " lenc=" + tlv.valDat[1] + " usc=" + tlv.valDat[2] + " uenc=" + tlv.valDat[3]);
                    break;
                case rtrIsisTe.typMtu:
                    l.add("  mtu: " + bits.msbGetW(tlv.valDat, 1));
                    break;
                case rtrIsisSr.typAdjSid:
                    l.add("  adj sid: flg=" + bits.toHexB(tlv.valDat[0]) + " lab=" + (bits.msbGetD(tlv.valDat, 2) >>> 8));
                    break;
                case rtrIsisSr.typLanAdjSid:
                    addrIsis adrI = new addrIsis();
                    adrI.fromBuf(tlv.valDat, 2);
                    l.add("  adj sid: sysid=" + adrI + " flg=" + bits.toHexB(tlv.valDat[0]) + " lab=" + (bits.msbGetD(tlv.valDat, 8) >>> 8));
                    break;
                default:
                    l.add("  unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpExtAddrReach(List<String> l, encTlv tlv, int ofs, int len) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, ofs, 0, len);
        pck.putSkip(len);
        pck.merge2beg();
        tlv = rtrIsis.getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // tag32
                    String a = "";
                    for (int i = 0; i < tlv.valSiz; i += 4) {
                        a += " " + bits.msbGetD(tlv.valDat, i);
                    }
                    l.add("  tag:" + a);
                    break;
                case 2: // tag64
                    a = "";
                    for (int i = 0; i < tlv.valSiz; i += 8) {
                        a += " " + bits.msbGetQ(tlv.valDat, i);
                    }
                    l.add("  tag:" + a);
                    break;
                case rtrIsisSr.typPrfSeg:
                    l.add("  prefix-sid: flg=" + bits.toHexB(tlv.valDat[0]) + " algo=" + tlv.valDat[1] + " idx=" + bits.msbGetD(tlv.valDat, 2));
                    break;
                case 4: // attributes
                    l.add("  attributes: " + bits.toHexB(tlv.valDat[0]));
                    break;
                case 11: // ipv4 source
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("  ipv4 source: " + adr4);
                    break;
                case 12: // ipv6 source
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("  ipv6 source: " + adr6);
                    break;
                case rtrIsisBr.typBier:
                    int i = bits.msbGetD(tlv.valDat, 7);
                    l.add("  bier: algo=" + tlv.valDat[0] + " igp=" + tlv.valDat[1] + " dom=" + tlv.valDat[2] + " id=" + bits.msbGetW(tlv.valDat, 3) + " si=" + tlv.valDat[7] + " bsl=" + ((i >>> 20) & 0xf) + " lab=" + (i & 0xfffff));
                    break;
                default:
                    l.add("  unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpRtrCapa(List<String> l, encTlv tlv, int ofs) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, ofs, 0, tlv.valSiz - ofs);
        pck.putSkip(tlv.valSiz - ofs);
        pck.merge2beg();
        tlv = rtrIsis.getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // te node capa
                    l.add("  te: " + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrIsisSr.typSrCapa: // sr capa
                    l.add("  sr: flg=" + bits.toHexB(tlv.valDat[0]) + " range=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " base=" + (bits.msbGetD(tlv.valDat, 6) >>> 8));
                    break;
                case 19: // sr algo
                    l.add("  sr algo: " + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                    break;
                case 20: // s-bfd
                    l.add("  s-bfd: " + bits.msbGetD(tlv.valDat, 0));
                    break;
                case 21: // node admin tag
                    l.add("  admin tag: " + bits.msbGetD(tlv.valDat, 0));
                    break;
                case 22: // srlb
                    l.add("  srlb: flg=" + bits.toHexB(tlv.valDat[0]) + " range=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " base=" + (bits.msbGetD(tlv.valDat, 4) >>> 8));
                    break;
                case 23: // node msd
                    String a = "";
                    for (int i = 0; i < tlv.valSiz; i += 2) {
                        a += " typ=" + tlv.valDat[i] + " len=" + tlv.valDat[i + 1];
                    }
                    l.add("  node msd: " + a);
                    break;
                case 24: // srms
                    l.add("  srms: " + tlv.valDat[0]);
                    break;
                default:
                    l.add("  unknown: " + tlv.dump());
                    break;
            }
        }
    }

}
