package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.ifc.ifcDot1ad;
import net.freertr.ifc.ifcDot1ah;
import net.freertr.ifc.ifcDot1q;
import net.freertr.ifc.ifcQinq1;
import net.freertr.ifc.ifcQinq2;
import net.freertr.ifc.ifcQinq3;
import net.freertr.ip.ipCor;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipIcmp4;
import net.freertr.ip.ipIcmp6;
import net.freertr.ip.ipIfc4;
import net.freertr.ip.ipIfc6;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtDccp;
import net.freertr.prt.prtLudp;
import net.freertr.prt.prtSctp;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.util.counter;

/**
 * quality of service worker
 *
 * @author matecsaba
 */
public class tabQos {

    /**
     * name of policy
     */
    public String policyName;

    /**
     * last left value
     */
    public long lastLeft;

    /**
     * list of classes
     */
    protected final List<tabQosN> classesD;

    private int classesP;

    private ipCor ip4cor;

    private ipCor ip6cor;

    /**
     * create new qos class
     */
    protected tabQos() {
        classesD = new ArrayList<tabQosN>();
        ip4cor = new ipCor4();
        ip6cor = new ipCor6();
    }

    /**
     * get one class
     *
     * @param i index
     * @return class, null if not found
     */
    public tabQosN getClass(int i) {
        if (i >= classesD.size()) {
            return null;
        }
        return classesD.get(i);
    }

    /**
     * get statistics
     *
     * @param dump dump entries
     * @return statistics
     */
    public List<String> getStats(boolean dump) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < classesD.size(); i++) {
            tabQosN cls = classesD.get(i);
            if (cls == null) {
                continue;
            }
            cls.getStats(l);
            if (!dump) {
                continue;
            }
            l.addAll(cls.entry.usrString("    "));
            if (cls.entry.aclMatch == null) {
                continue;
            }
            l.addAll(cls.entry.aclMatch.dump("      "));
        }
        return l;
    }

    private static boolean convertSubPlcy(tabQos res, tabQosN prnt, tabListing<tabPlcmapN, addrIP> pm) {
        int frst = res.classesD.size();
        for (int i = 0; i < pm.size(); i++) {
            tabQosN no = new tabQosN();
            no.entry = pm.get(i);
            if (no.entry == null) {
                continue;
            }
            no.parent = prnt;
            if (no.entry.action == tabListingEntry.actionType.actPriorty) {
                res.classesD.add(frst, no);
                frst++;
            } else {
                res.classesD.add(no);
            }
            if (no.entry.child == null) {
                continue;
            }
            if (prnt != null) {
                return true;
            }
            int old = res.classesD.size();
            if (convertSubPlcy(res, no, no.entry.child)) {
                return true;
            }
            no.childs = res.classesD.size() - old;
        }
        return false;
    }

    /**
     * compile policy map
     *
     * @param pm policy map to convert
     * @return compiled qos worker, null on error
     */
    public static tabQos convertPolicy(tabListing<tabPlcmapN, addrIP> pm) {
        if (pm == null) {
            return null;
        }
        tabQos res = new tabQos();
        res.policyName = pm.listName;
        if (convertSubPlcy(res, null, pm)) {
            return null;
        }
        if (res.classesD.size() < 1) {
            return null;
        }
        return res;
    }

    private boolean classifyLayer3(packHolder pck) {
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        switch (pck.ETHtype) {
            case ipIfc4.type:
                return ip4cor.parseIPheader(pck, true);
            case ipIfc6.type:
                return ip6cor.parseIPheader(pck, true);
            case ipMpls.typeM:
            case ipMpls.typeU:
                int i = pck.dataSize();
                ipMpls.parseMPLSheader(pck);
                pck.setBytesLeft(i);
                return true;
            case ifcDot1q.type:
                pck.getSkip(-2);
                new ifcDot1q().parseHeader(pck);
                pck.getSkip(-ifcDot1q.size);
                pck.getSkip(2);
                return true;
            case ifcDot1ah.type:
                pck.getSkip(-2);
                new ifcDot1ah().parseHeader(pck);
                pck.getSkip(-ifcDot1ah.size);
                pck.getSkip(2);
                return true;
            case ifcDot1ad.type:
                pck.getSkip(-2);
                new ifcDot1ad().parseHeader(pck);
                pck.getSkip(-ifcDot1ad.size);
                pck.getSkip(2);
                return true;
            case ifcQinq1.type:
                pck.getSkip(-2);
                new ifcQinq1().parseHeader(pck);
                pck.getSkip(-ifcQinq1.size);
                pck.getSkip(2);
                return true;
            case ifcQinq2.type:
                pck.getSkip(-2);
                new ifcQinq2().parseHeader(pck);
                pck.getSkip(-ifcQinq2.size);
                pck.getSkip(2);
                return true;
            case ifcQinq3.type:
                pck.getSkip(-2);
                new ifcQinq3().parseHeader(pck);
                pck.getSkip(-ifcQinq3.size);
                pck.getSkip(2);
                return true;
            default:
                return true;
        }
    }

    /**
     * classify layer4 headers
     *
     * @param pck packet to parse
     */
    public static void classifyLayer4(packHolder pck) {
        pck.UDPsrc = 0;
        pck.UDPtrg = 0;
        pck.UDPsiz = 0;
        switch (pck.IPprt) {
            case prtTcp.protoNum:
                prtTcp.parseTCPports(pck);
                break;
            case prtUdp.protoNum:
                prtUdp.parseUDPports(pck);
                break;
            case prtLudp.protoNum:
                prtLudp.parseLUDPports(pck);
                break;
            case prtDccp.protoNum:
                prtDccp.parseDCCPports(pck);
                break;
            case prtSctp.protoNum:
                prtSctp.parseSCTPports(pck);
                break;
            case ipIcmp4.protoNum:
                ipIcmp4.parseICMPports(pck);
                pck.UDPtrg = pck.UDPsrc;
                break;
            case ipIcmp6.protoNum:
                ipIcmp6.parseICMPports(pck);
                pck.UDPtrg = pck.UDPsrc;
                break;
        }
    }

    private void doClassifycation(packHolder pck) {
        pck.INTclass = -1;
        for (int i = 0; i < classesD.size(); i++) {
            tabQosN cls = classesD.get(i);
            if (cls == null) {
                continue;
            }
            if (!cls.entry.matches(pck)) {
                i += cls.childs;
                continue;
            }
            if (cls.childs > 0) {
                continue;
            }
            pck.INTclass = i;
            break;
        }
    }

    /**
     * classify packet
     *
     * @param pck packet to classify
     */
    public void classifyPack(packHolder pck) {
        if (!classifyLayer3(pck)) {
            pck.getSkip(pck.IPsiz);
            classifyLayer4(pck);
            pck.getSkip(-pck.IPsiz);
        }
        pck.getSkip(-2);
        doClassifycation(pck);
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     * @param cls class to use
     */
    public void updatePack(packHolder pck, tabQosN cls) {
        pck.INTqosGrp = cls.entry.qosSet.update(pck.INTqosGrp);
        int o = cls.entry.ttlSet.update(pck.IPttl);
        int p = cls.entry.flowSet.update(pck.IPid);
        switch (pck.ETHtype) {
            case ipIfc4.type:
                int i = cls.entry.updateTos(pck.IPtos);
                if ((pck.IPtos == i) && (pck.IPttl == o) && (pck.IPid == p)) {
                    return;
                }
                pck.getSkip(2);
                ip4cor.updateIPheader(pck, null, null, -1, o, i, p, -1);
                pck.getSkip(-2);
                return;
            case ipIfc6.type:
                i = cls.entry.updateTos(pck.IPtos);
                if ((pck.IPtos == i) && (pck.IPttl == o) && (pck.IPid == p)) {
                    return;
                }
                pck.getSkip(2);
                ip6cor.updateIPheader(pck, null, null, -1, o, i, p, -1);
                pck.getSkip(-2);
                return;
            case ipMpls.typeM:
            case ipMpls.typeU:
                i = pck.MPLSexp;
                pck.MPLSexp = cls.entry.expSet.update(i);
                if (i == pck.MPLSexp) {
                    return;
                }
                pck.getSkip(2 + ipMpls.sizeL);
                ipMpls.createMPLSheader(pck);
                pck.getSkip(-2);
                return;
            case ifcDot1q.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcDot1q.size);
                new ifcDot1q().createHeader(pck);
                return;
            case ifcDot1ah.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcDot1ah.size);
                new ifcDot1ah().createHeader(pck);
                return;
            case ifcDot1ad.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcDot1ad.size);
                new ifcDot1ad().createHeader(pck);
                return;
            case ifcQinq1.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcQinq1.size);
                new ifcQinq1().createHeader(pck);
                return;
            case ifcQinq2.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcQinq2.size);
                new ifcQinq2().createHeader(pck);
                return;
            case ifcQinq3.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcQinq3.size);
                new ifcQinq3().createHeader(pck);
                return;
            default:
                return;
        }
    }

    /**
     * put one packet to queue
     *
     * @param pck packet to queue
     */
    public void enqueuePack(packHolder pck) {
        if (pck.INTclass < 0) {
            return;
        }
        tabQosN cls = classesD.get(pck.INTclass);
        if (cls == null) {
            return;
        }
        synchronized (cls) {
            cls.enqueuePack(pck);
        }
        cls.cntr.rx(pck);
    }

    /**
     * check packet
     *
     * @param curr current time
     * @param pck packet to process
     * @return false if allowed, true if droping
     */
    public synchronized boolean checkPacket(long curr, packHolder pck) {
        pck.getSkip(pck.IPsiz);
        classifyLayer4(pck);
        pck.getSkip(-pck.IPsiz);
        doClassifycation(pck);
        if (pck.INTclass < 0) {
            return true;
        }
        tabQosN cls = classesD.get(pck.INTclass);
        if (cls == null) {
            return true;
        }
        cls.recUpdateTime(curr);
        if (cls.checkPacket(pck)) {
            cls.cntr.drop(pck, counter.reasons.noBuffer);
            return true;
        }
        cls.updateBytes(pck.dataSize());
        cls.cntr.tx(pck);
        return false;
    }

    /**
     * get one packet from queue
     *
     * @param curr current time
     * @return packet read, null if nothing
     */
    public synchronized packHolder dequeuePack(long curr) {
        lastLeft = 0;
        int classesN = classesD.size();
        for (int rnd = 0; rnd < classesN; rnd++) {
            tabQosN cls = classesD.get(classesP);
            if (cls == null) {
                classesP = (classesP + 1) % classesN;
                continue;
            }
            long left = cls.updateTime(curr);
            if (left < 1) {
                rnd = 0;
            } else if (cls.packets.size() > 0) {
                if (lastLeft < left) {
                    lastLeft = left;
                }
            }
            packHolder pck;
            synchronized (cls) {
                pck = cls.dequeuePack();
            }
            if (pck == null) {
                classesP = (classesP + 1) % classesN;
                continue;
            }
            updatePack(pck, cls);
            cls.updateBytes(pck.dataSize());
            cls.cntr.tx(pck);
            return pck;
        }
        return null;
    }

}
