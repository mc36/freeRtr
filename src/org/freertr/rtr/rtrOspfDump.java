package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * ospfv2 lsa dumper
 *
 * @author matecsaba
 */
public class rtrOspfDump {

    private rtrOspfDump() {
    }

    /**
     * dump lsa4
     *
     * @param l list to append
     * @param pck lsa contents
     * @param lsa lsa contents
     */
    public static void dump4lsa(List<String> l, packHolder pck, rtrOspf4lsa lsa) {
        pck.getSkip(rtrOspf4lsa.headSize);
        switch (lsa.lsaType) {
            case rtrOspf4lsa.lsaRouter:
                l.add("flg=" + bits.toHexW(pck.msbGetW(0)) + " lnk=" + bits.toHexW(pck.msbGetW(2)));
                pck.getSkip(4);
                for (; pck.dataSize() > 0;) {
                    addrIPv4 adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0);
                    addrIPv4 adr4b = new addrIPv4();
                    pck.getAddr(adr4b, 4);
                    String a;
                    switch (pck.getByte(8)) {
                        case rtrOspf4lsa.lnkP2p:
                            a = "router";
                            break;
                        case rtrOspf4lsa.lnkStub:
                            a = "stub";
                            break;
                        case rtrOspf4lsa.lnkTrns:
                            a = "network";
                            break;
                        case rtrOspf4lsa.lnkVirt:
                            a = "virtual";
                            break;
                        default:
                            a = "unknown=" + pck.getByte(8);
                            break;
                    }
                    l.add("  id=" + adr4 + " data=" + adr4b + " type=" + a + " tos=" + pck.getByte(9) + " metric=" + pck.msbGetW(10));
                    pck.getSkip(12);
                }
                break;
            case rtrOspf4lsa.lsaNetwork:
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                pck.getSkip(4);
                l.add("netmask=" + adr4);
                for (; pck.dataSize() > 0;) {
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0);
                    l.add("  router=" + adr4);
                    pck.getSkip(4);
                }
                break;
            case rtrOspf4lsa.lsaSumNet:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                pck.getSkip(4);
                l.add("netmask=" + adr4);
                for (; pck.dataSize() > 0;) {
                    l.add("  tos=" + pck.getByte(0) + " metric=" + (pck.msbGetD(1) >>> 8));
                    pck.getSkip(4);
                }
                break;
            case rtrOspf4lsa.lsaSumAsBr:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                pck.getSkip(4);
                l.add("netmask=" + adr4);
                for (; pck.dataSize() > 0;) {
                    l.add("  tos=" + pck.getByte(0) + " metric=" + (pck.msbGetD(1) >>> 8));
                    pck.getSkip(4);
                }
                break;
            case rtrOspf4lsa.lsaAsExt:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                pck.getSkip(4);
                l.add("netmask=" + adr4);
                for (; pck.dataSize() > 0;) {
                    pck.getAddr(adr4, 4);
                    l.add("  flg=" + bits.toHexB(pck.getByte(0)) + " metric=" + (pck.msbGetD(1) >>> 8) + " fwd=" + adr4 + " tag=" + pck.msbGetD(8));
                    pck.getSkip(12);
                }
                break;
            case rtrOspf4lsa.lsaNssaExt:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 0);
                pck.getSkip(4);
                l.add("netmask=" + adr4);
                for (; pck.dataSize() > 0;) {
                    pck.getAddr(adr4, 4);
                    l.add("  flg=" + bits.toHexB(pck.getByte(0)) + " metric=" + (pck.msbGetD(1) >>> 8) + " fwd=" + adr4 + " tag=" + pck.msbGetD(8));
                    pck.getSkip(12);
                }
                break;
            case rtrOspf4lsa.lsaOpArea:
                switch (lsa.lsaID.getBytes()[0]) {
                    case 1: // te
                        dumpTeLsa(l, pck);
                        break;
                    case 4: // router info
                        dumpRiLsa(l, pck);
                        break;
                    case 7: // prefix
                        dumpPrfLsa(l, pck);
                        break;
                    case 8: // link
                        dumpLnkLsa(l, pck);
                        break;
                    default:
                        break;
                }
                break;
            default:
                break;
        }
    }

    /**
     * dump lsa6
     *
     * @param l list to append
     * @param pck lsa contents
     * @param lsa lsa contents
     */
    public static void dump6lsa(List<String> l, packHolder pck, rtrOspf6lsa lsa) {
        pck.getSkip(rtrOspf6lsa.headSize);
        switch (lsa.lsaType) {
            case rtrOspf6lsa.lsaRouter:
                l.add("flg=" + bits.toHexB(pck.getByte(0)) + " opt=" + (pck.msbGetD(1) >>> 8));
                pck.getSkip(4);
                for (; pck.dataSize() > 0;) {
                    addrIPv4 adr4 = new addrIPv4();
                    pck.getAddr(adr4, 12);
                    String a;
                    switch (pck.getByte(0)) {
                        case rtrOspf6lsa.lnkP2p:
                            a = "router";
                            break;
                        case rtrOspf6lsa.lnkTrns:
                            a = "network";
                            break;
                        case rtrOspf6lsa.lnkVirt:
                            a = "virtual";
                            break;
                        default:
                            a = "unknown=" + pck.getByte(0);
                            break;
                    }
                    l.add("  iface=" + pck.msbGetD(4) + " type=" + a + " neigh=" + adr4 + "/" + bits.toHexD(pck.msbGetD(8)) + " metric=" + pck.msbGetW(2));
                    pck.getSkip(16);
                }
                break;
            case rtrOspf6lsa.lsaNetwork:
                l.add("opt=" + (pck.msbGetD(1) >>> 8));
                pck.getSkip(4);
                for (; pck.dataSize() > 0;) {
                    addrIPv4 adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0);
                    l.add("  router=" + adr4);
                    pck.getSkip(4);
                }
                break;
            case rtrOspf6lsa.lsaInterPrf:
                rtrOspf6pref prf6 = new rtrOspf6pref();
                rtrOspf6area.prefixRead(pck, 4, prf6);
                l.add("opt=" + prf6.option + " metric=" + (pck.msbGetD(1) >>> 8) + " prefix=" + prf6.prefix);
                break;
            case rtrOspf6lsa.lsaInterRtr:
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 8);
                l.add("opt=" + (pck.msbGetD(1) >>> 8) + " metric=" + (pck.msbGetD(5) >>> 8) + " router=" + adr4);
                break;
            case rtrOspf6lsa.lsaAsExt:
                prf6 = new rtrOspf6pref();
                rtrOspf6area.prefixRead(pck, 4, prf6);
                l.add("flg=" + bits.toHexB(pck.getByte(0)) + " metric=" + (pck.msbGetD(1) >>> 8) + " ref=" + prf6.option + " prefix=" + prf6.prefix);
                break;
            case rtrOspf6lsa.lsaNssaExt:
                prf6 = new rtrOspf6pref();
                rtrOspf6area.prefixRead(pck, 4, prf6);
                l.add("flg=" + bits.toHexB(pck.getByte(0)) + " metric=" + (pck.msbGetD(1) >>> 8) + " ref=" + prf6.option + " prefix=" + prf6.prefix);
                break;
            case rtrOspf6lsa.lsaLink:
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 4);
                l.add("pri=" + pck.getByte(0) + " opt=" + (pck.msbGetD(1) >>> 8) + " addr=" + adr6 + " prefs=" + pck.msbGetD(20));
                pck.getSkip(24);
                for (; pck.dataSize() > 0;) {
                    prf6 = new rtrOspf6pref();
                    pck.getSkip(rtrOspf6area.prefixRead(pck, 0, prf6));
                    l.add("  opt=" + prf6.option + " prefix=" + prf6.prefix);
                }
                break;
            case rtrOspf6lsa.lsaPrefix:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 8);
                l.add("prefs=" + pck.msbGetW(0) + " ref=" + pck.msbGetW(2) + " link=" + pck.msbGetD(4) + " router=" + adr4);
                pck.getSkip(12);
                for (; pck.dataSize() > 0;) {
                    prf6 = new rtrOspf6pref();
                    pck.getSkip(rtrOspf6area.prefixRead(pck, 0, prf6));
                    l.add("  opt=" + prf6.option + " metric=" + prf6.metric + " prefix=" + prf6.prefix);
                }
                break;
            case rtrOspf6lsa.lsaGrace:
                l.add("grace:" + bits.byteDump(pck.getCopy(), 0, -1));
                break;
            case rtrOspf6lsa.lsaRtrInfo:
                dumpRiLsa(l, pck);
                break;
            case rtrOspf6lsa.lsaTraffEng:
                dumpTeLsa(l, pck);
                break;
            case rtrOspf6lsa.lsaErouter:
                l.add("flg=" + bits.toHexB(pck.getByte(0)) + " opt=" + (pck.msbGetD(1) >>> 8));
                pck.getSkip(4);
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEnetwork:
                l.add("opt=" + (pck.msbGetD(1) >>> 8));
                pck.getSkip(4);
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEinterPrf:
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEinterRtr:
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEasExt:
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEnssaExt:
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaElink:
                l.add("pri=" + bits.toHexB(pck.getByte(0)) + " opt=" + (pck.msbGetD(1) >>> 8));
                pck.getSkip(4);
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaEprefix:
                adr4 = new addrIPv4();
                pck.getAddr(adr4, 8);
                l.add("ref=" + pck.msbGetW(2) + " link=" + pck.msbGetD(4) + " router=" + adr4);
                pck.getSkip(12);
                dumpElsa(l, pck);
                break;
            case rtrOspf6lsa.lsaSegRoutV6:
                encTlv tlv = rtrOspfTe.getTlvHandler();
                for (;;) {
                    if (tlv.getBytes(pck)) {
                        break;
                    }
                    if (tlv.valTyp != 1) {
                        l.add("unknown: " + tlv.dump());
                        continue;
                    }
                    int len = tlv.valDat[2] & 0xff;
                    adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 8);
                    addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr6, len);
                    l.add("locator: " + prf + " rout=" + (tlv.valDat[0] & 0xff) + " algo=" + (tlv.valDat[1] & 0xff) + " flag=" + (tlv.valDat[3] & 0xff) + " metric=" + bits.msbGetD(tlv.valDat, 4));
                }
                break;
            default:
                break;
        }
    }

    private static void dumpElsa(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrOspf6lsa.tlvRouter:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 12);
                    String a;
                    switch (tlv.valDat[0]) {
                        case rtrOspf6lsa.lnkP2p:
                            a = "router";
                            break;
                        case rtrOspf6lsa.lnkTrns:
                            a = "network";
                            break;
                        case rtrOspf6lsa.lnkVirt:
                            a = "virtual";
                            break;
                        default:
                            a = "unknown=" + pck.getByte(0);
                            break;
                    }
                    l.add("  iface=" + bits.msbGetD(tlv.valDat, 4) + " type=" + a + " neigh=" + adr4 + "/" + bits.toHexD(bits.msbGetD(tlv.valDat, 8)) + " metric=" + bits.msbGetW(tlv.valDat, 2));
                    packHolder p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 16, 0, tlv.valSiz - 16);
                    p2.putSkip(tlv.valSiz - 16);
                    p2.merge2beg();
                    dumpLnkLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvAttach:
                    for (int i = 0; i < tlv.valSiz; i += 4) {
                        adr4 = new addrIPv4();
                        adr4.fromBuf(tlv.valDat, i);
                        l.add("  router=" + adr4);
                    }
                    break;
                case rtrOspf6lsa.tlvInterPrf:
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 4, 0, tlv.valSiz - 4);
                    p2.putSkip(tlv.valSiz - 4);
                    p2.merge2beg();
                    rtrOspf6pref prf6 = new rtrOspf6pref();
                    p2.getSkip(rtrOspf6area.prefixRead(pck, 4, prf6));
                    l.add("  metric=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " prefix=" + prf6);
                    dumpPrfLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvInterRtr:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 8);
                    l.add("  opt=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " metric=" + (bits.msbGetD(tlv.valDat, 5) >>> 8) + " router=" + adr4);
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 12, 0, tlv.valSiz - 12);
                    p2.putSkip(tlv.valSiz - 12);
                    p2.merge2beg();
                    dumpLnkLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvAsExt:
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 4, 0, tlv.valSiz - 4);
                    p2.putSkip(tlv.valSiz - 4);
                    p2.merge2beg();
                    prf6 = new rtrOspf6pref();
                    p2.getSkip(rtrOspf6area.prefixRead(pck, 4, prf6));
                    l.add("  flg=" + bits.toHexB(tlv.valDat[0]) + "metric=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " prefix=" + prf6);
                    dumpPrfLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvPrefix:
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 4, 0, tlv.valSiz - 4);
                    p2.putSkip(tlv.valSiz - 4);
                    p2.merge2beg();
                    prf6 = new rtrOspf6pref();
                    p2.getSkip(rtrOspf6area.prefixRead(pck, 4, prf6));
                    l.add("  metric=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " prefix=" + prf6);
                    dumpPrfLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvLink6:
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 16, 0, tlv.valSiz - 16);
                    p2.putSkip(tlv.valSiz - 16);
                    p2.merge2beg();
                    dumpLnkLsa2(l, p2);
                    break;
                case rtrOspf6lsa.tlvLink4:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 4, 0, tlv.valSiz - 4);
                    p2.putSkip(tlv.valSiz - 4);
                    p2.merge2beg();
                    dumpLnkLsa2(l, p2);
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpTeLsa(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrOspfTe.typRouter4:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("router id=" + adr4);
                    break;
                case rtrOspfTe.typLink:
                    packHolder p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
                    p2.putSkip(tlv.valSiz);
                    p2.merge2beg();
                    for (;;) {
                        if (tlv.getBytes(p2)) {
                            break;
                        }
                        switch (tlv.valTyp) {
                            case rtrOspfTe.typLnkTyp:
                                l.add("link type=" + tlv.valDat[0]);
                                break;
                            case rtrOspfTe.typLnkId:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("link id=" + adr4);
                                break;
                            case rtrOspfTe.typLoc4adr:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("local ipv4=" + adr4);
                                break;
                            case rtrOspfTe.typRem4adr:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("remote ipv4=" + adr4);
                                break;
                            case rtrOspfTe.typMetric:
                                l.add("te metric=" + bits.msbGetD(tlv.valDat, 0));
                                break;
                            case rtrOspfTe.typMaxBndwdt:
                                l.add("max bandwidth: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typMaxReserv:
                                l.add("reservable bandwidth: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typUnReserv:
                                l.add("unreserved bandwidth: " + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typAdminGrp:
                                l.add("admin group=" + bits.msbGetD(tlv.valDat, 0));
                                break;
                            case rtrOspfTe.typBothTe:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                addrIPv4 adr4b = new addrIPv4();
                                adr4b.fromBuf(tlv.valDat, 4);
                                l.add("te id: local=" + adr4 + " remote=" + adr4b);
                                break;
                            case rtrOspfTe.typBothId:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                adr4b = new addrIPv4();
                                adr4b.fromBuf(tlv.valDat, 4);
                                l.add("id: local=" + adr4 + " remote=" + adr4b);
                                break;
                            case rtrOspfTe.typIntRaUp:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("inter-ra up=" + adr4);
                                break;
                            case rtrOspfTe.typIntRaDn:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("inter-ra dn=" + adr4);
                                break;
                            case rtrOspfTe.typLnkProt:
                                l.add("link protetc=" + bits.toHexB(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typSwchCapa:
                                l.add("switch capa: sc=" + tlv.valDat[0] + " enc=" + tlv.valDat[1]);
                                break;
                            case rtrOspfTe.typSrlg:
                                String a = "";
                                for (int i = 0; i < tlv.valSiz; i += 4) {
                                    a += " " + bits.msbGetD(tlv.valDat, i);
                                }
                                l.add("srlg=" + a);
                                break;
                            case rtrOspfTe.typBndwdtCnst:
                                l.add("bandwidth constrain=" + tlv.valDat[0]);
                                break;
                            case rtrOspfTe.typNeighId:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 4);
                                l.add("neighbor id=" + adr4 + "/" + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typLoc6adr:
                                addrIPv6 adr6 = new addrIPv6();
                                adr6.fromBuf(tlv.valDat, 0);
                                l.add("local ipv6=" + adr6);
                                break;
                            case rtrOspfTe.typRem6adr:
                                adr6 = new addrIPv6();
                                adr6.fromBuf(tlv.valDat, 0);
                                l.add("remote ipv6=" + adr6);
                                break;
                            case rtrOspfTe.typASnum:
                                l.add("as number=" + bits.msbGetD(tlv.valDat, 0));
                                break;
                            case rtrOspfTe.typAsbr4id:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("ipv4 asbr=" + adr4);
                                break;
                            case rtrOspfTe.typUncnstLsp:
                                l.add("lsp count=" + bits.msbGetD(tlv.valDat, 0));
                                break;
                            case rtrOspfTe.typAsbr6id:
                                adr6 = new addrIPv6();
                                adr6.fromBuf(tlv.valDat, 0);
                                l.add("ipv6 asbr=" + adr6);
                                break;
                            case rtrOspfTe.typIntAdjCapa:
                                l.add("intf adjust capa: lsc=" + tlv.valDat[0] + " lenc=" + tlv.valDat[1] + " usc=" + tlv.valDat[2] + " uenc=" + tlv.valDat[3]);
                                break;
                            case rtrOspfTe.typExtAdmin:
                                l.add("ext admin group= " + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                                break;
                            case rtrOspfTe.typUniLnkDly:
                                l.add("uni link delay: flg=" + bits.toHexB(tlv.valDat[0]) + " delay=" + (bits.msbGetD(tlv.valDat, 1) >>> 8));
                                break;
                            case rtrOspfTe.typRngUniLnkDly:
                                l.add("uni link delay: flg=" + bits.toHexB(tlv.valDat[0]) + " min=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " max=" + (bits.msbGetD(tlv.valDat, 5) >>> 8));
                                break;
                            case rtrOspfTe.typUniLnkDlyVar:
                                l.add("uni link delay var=" + (bits.msbGetD(tlv.valDat, 1) >>> 8));
                                break;
                            case rtrOspfTe.typUniLnkLos:
                                l.add("uni link loss: flg=" + bits.toHexB(tlv.valDat[0]) + " loss=" + (bits.msbGetD(tlv.valDat, 1) >>> 8));
                                break;
                            case rtrOspfTe.typUniResBwd:
                                l.add("uni resid bandwidth=" + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typUniAvaBwd:
                                l.add("uni avail bandwidth=" + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typUniUtlBwd:
                                l.add("uni util bandwidth=" + (long) Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 0)));
                                break;
                            case rtrOspfTe.typPrtLabRes:
                                l.add("port lab restr: mat=" + tlv.valDat[0] + " rst=" + tlv.valDat[1] + " sc=" + tlv.valDat[2] + " enc=" + tlv.valDat[3]);
                                break;
                            case rtrOspfTe.typNetRtrMet:
                                l.add("net2rtr te metric=" + bits.msbGetD(tlv.valDat, 0));
                                break;
                            default:
                                l.add("unknown: " + tlv.dump());
                                break;
                        }
                    }
                    break;
                case rtrOspfTe.typRouter6:
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 0);
                    l.add("router id=" + adr6);
                    break;
                case rtrOspfTe.typLnkLoc:
                    p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
                    p2.putSkip(tlv.valSiz);
                    p2.merge2beg();
                    for (;;) {
                        if (tlv.getBytes(p2)) {
                            break;
                        }
                        switch (tlv.valTyp) {
                            case 1:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 1);
                                addrPrefix<addrIPv4> prf4 = new addrPrefix<addrIPv4>(adr4, tlv.valDat[0]);
                                l.add("ipv4 node addr=" + prf4);
                                break;
                            case 2:
                                adr6 = new addrIPv6();
                                adr6.fromBuf(tlv.valDat, 1);
                                addrPrefix<addrIPv6> prf6 = new addrPrefix<addrIPv6>(adr6, tlv.valDat[0]);
                                l.add("ipv6 node addr=" + prf6);
                                break;
                            case 5:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("local te id=" + adr4);
                                break;
                            case 12:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("inter-ra up=" + adr4);
                                break;
                            case 13:
                                adr4 = new addrIPv4();
                                adr4.fromBuf(tlv.valDat, 0);
                                l.add("inter-ra dn=" + adr4);
                                break;
                            case 14:
                                l.add("matrix= " + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                                break;
                            default:
                                l.add("unknown: " + tlv.dump());
                                break;
                        }
                    }
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpRiLsa(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrOspfRi.typInfCapa:
                    l.add("info capab=" + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrOspfRi.typFncCapa:
                    l.add("func capab=" + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrOspfRi.typMshGrp4:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 4);
                    l.add("ipv4 mestgrp: grp=" + bits.msbGetD(tlv.valDat, 0) + " adr=" + adr4);
                    break;
                case rtrOspfRi.typMshGrp6:
                    addrIPv6 adr6 = new addrIPv6();
                    adr6.fromBuf(tlv.valDat, 4);
                    l.add("ipv6 mestgrp: grp=" + bits.msbGetD(tlv.valDat, 0) + " adr=" + adr6);
                    break;
                case rtrOspfRi.typTeCapa:
                    l.add("te capab=" + bits.toHexD(bits.msbGetD(tlv.valDat, 0)));
                    break;
                case rtrOspfRi.typPceD:
                    l.add("pced=" + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                    break;
                case rtrOspfRi.typHstnam:
                    l.add("hostname=" + tlv.getStr());
                    break;
                case rtrOspfRi.typSrAlgo:
                    l.add("sr algo=" + bits.byteDump(tlv.valDat, 0, tlv.valSiz));
                    break;
                case rtrOspfRi.typSrBase:
                    l.add("sr base: range=" + (bits.msbGetD(tlv.valDat, 0) >>> 8) + " base=" + (bits.msbGetD(tlv.valDat, 8) >>> 8));
                    break;
                case rtrOspfRi.typNodAdm:
                    l.add("admin tag=" + bits.msbGetD(tlv.valDat, 0));
                    break;
                case rtrOspfRi.typSbfd:
                    l.add("s-bfd=" + bits.msbGetD(tlv.valDat, 0));
                    break;
                case rtrOspfRi.typNodMsd:
                    String a = "";
                    for (int i = 0; i < tlv.valSiz; i += 2) {
                        a += " typ=" + tlv.valDat[i] + " len=" + tlv.valDat[i + 1];
                    }
                    l.add("node msd: " + a);
                    break;
                case rtrOspfRi.typSrLocBlk:
                    l.add("srlb: range=" + (bits.msbGetD(tlv.valDat, 0) >>> 8) + " base=" + (bits.msbGetD(tlv.valDat, 8) >>> 8));
                    break;
                case rtrOspfRi.typSrMsPref:
                    l.add("srms prefer=" + tlv.valDat[0]);
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpPrfLsa(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 4);
                    addrPrefix<addrIPv4> prf4 = new addrPrefix<addrIPv4>(adr4, tlv.valDat[1]);
                    l.add("type=" + tlv.valDat[0] + " af=" + tlv.valDat[2] + " flg=" + bits.toHexB(tlv.valDat[3]) + " prefix=" + prf4);
                    packHolder p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 8, 0, tlv.valSiz - 8);
                    p2.putSkip(tlv.valSiz - 8);
                    p2.merge2beg();
                    dumpPrfLsa2(l, p2);
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpPrfLsa2(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrOspfSr.prfSidLab:
                    l.add("sid=" + bits.msbGetD(tlv.valDat, 0));
                    break;
                case rtrOspfSr.prfPrfSid:
                    l.add("prefix sid: flg=" + bits.toHexB(tlv.valDat[0]) + " mtid=" + tlv.valDat[2] + " algo=" + tlv.valDat[3] + " idx=" + bits.msbGetD(tlv.valDat, 4));
                    break;
                case rtrOspfBr.typBierInfo:
                    l.add("bier info: dom=" + tlv.valDat[0] + " mtid=" + tlv.valDat[1] + " bfr=" + bits.msbGetW(tlv.valDat, 2) + " algo=" + tlv.valDat[4] + " igp=" + tlv.valDat[5]);
                    break;
                case rtrOspfBr.typBierMpls:
                    l.add("bier mpls: si=" + tlv.valDat[0] + " lab=" + (bits.msbGetD(tlv.valDat, 1) >>> 8) + " bsl=" + (tlv.valDat[4] >>> 4));
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpLnkLsa(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 4);
                    addrIPv4 adr4b = new addrIPv4();
                    adr4b.fromBuf(tlv.valDat, 8);
                    l.add("type=" + tlv.valDat[0] + " id=" + adr4 + " data=" + adr4b);
                    packHolder p2 = new packHolder(true, true);
                    p2.putCopy(tlv.valDat, 12, 0, tlv.valSiz - 12);
                    p2.putSkip(tlv.valSiz - 12);
                    p2.merge2beg();
                    dumpLnkLsa2(l, p2);
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

    private static void dumpLnkLsa2(List<String> l, packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrOspfSr.lnkSidLab:
                    l.add("sid=" + bits.msbGetD(tlv.valDat, 0));
                    break;
                case rtrOspfSr.lnkAdjSid:
                    l.add("adj sid: flg=" + bits.toHexB(tlv.valDat[0]) + " mtid=" + tlv.valDat[2] + " weight=" + tlv.valDat[3] + " lab=" + (bits.msbGetD(tlv.valDat, 4) >>> 8));
                    break;
                case rtrOspfSr.lnkLanAdjSid:
                    addrIPv4 adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 4);
                    l.add("adj sid: flg=" + bits.toHexB(tlv.valDat[0]) + " mtid=" + tlv.valDat[2] + " weight=" + tlv.valDat[3] + " neigh=" + adr4 + " lab=" + bits.msbGetD(tlv.valDat, 8));
                    break;
                case 4:
                    l.add("net2rtr metric: mtid=" + tlv.valDat[0] + " metric=" + bits.msbGetW(tlv.valDat, 2));
                    break;
                case 5:
                    l.add("rtm set: flg=" + bits.toHexB(tlv.valDat[0]));
                    break;
                case 6:
                    String a = "";
                    for (int i = 0; i < tlv.valSiz; i += 2) {
                        a += " typ=" + tlv.valDat[i] + " len=" + tlv.valDat[i + 1];
                    }
                    l.add("link msd: " + a);
                    break;
                case 7:
                    l.add("graceful shutdown");
                    break;
                case 8:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("remote ipv4=" + adr4);
                    break;
                case 9:
                    l.add("loc/rem iface id: loc=" + bits.msbGetD(tlv.valDat, 0) + " rem=" + bits.msbGetD(tlv.valDat, 4));
                    break;
                case rtrOspfSr.lnkRemote:
                    adr4 = new addrIPv4();
                    adr4.fromBuf(tlv.valDat, 0);
                    l.add("sr remote=" + adr4);
                    break;
                default:
                    l.add("unknown: " + tlv.dump());
                    break;
            }
        }
    }

}
