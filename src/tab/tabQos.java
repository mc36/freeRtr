package tab;

import addr.addrIP;
import ifc.ifcDot1ad;
import ifc.ifcDot1ah;
import ifc.ifcDot1q;
import ip.ipCor;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipMpls;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import prt.prtDccp;
import prt.prtSctp;
import prt.prtTcp;
import prt.prtUdp;
import prt.prtLudp;

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
     * get statistics
     *
     * @return statistics
     */
    public List<String> getStats() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < classesD.size(); i++) {
            tabQosN cls = classesD.get(i);
            if (cls == null) {
                continue;
            }
            cls.getStats(l);
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
            if (no.entry.action == tabPlcmapN.actionType.actPriorty) {
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

    /**
     * classify packet
     *
     * @param pck packet to classify
     */
    public void classifyPack(packHolder pck) {
        pck.ETHtype = pck.msbGetW(0);
        pck.getSkip(2);
        boolean b;
        switch (pck.ETHtype) {
            case ipIfc4.type:
                b = ip4cor.parseIPheader(pck, true);
                break;
            case ipIfc6.type:
                b = ip6cor.parseIPheader(pck, true);
                break;
            case ipMpls.typeM:
            case ipMpls.typeU:
                int i = pck.dataSize();
                ipMpls.parseMPLSheader(pck);
                pck.setBytesLeft(i);
                b = true;
                break;
            case ifcDot1q.type:
                pck.getSkip(-2);
                ifcDot1q.parseHeader(pck);
                pck.getSkip(2);
                b = true;
                break;
            case ifcDot1ah.type:
                pck.getSkip(-2);
                ifcDot1ah.parseHeader(pck);
                pck.getSkip(2);
                b = true;
                break;
            case ifcDot1ad.type:
                pck.getSkip(-2);
                ifcDot1ad.parseHeader(pck);
                pck.getSkip(2);
                b = true;
                break;
            default:
                b = true;
                break;
        }
        if (!b) {
            pck.getSkip(pck.IPsiz);
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
            }
            pck.getSkip(-pck.IPsiz);
        }
        pck.getSkip(-2);
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
     * update one packet
     *
     * @param pck packet to update
     * @param cls class to use
     */
    public void updatePack(packHolder pck, tabQosN cls) {
        pck.INTqosGrp = cls.entry.qosSet.update(pck.INTqosGrp);
        int o = cls.entry.ttlSet.update(pck.IPttl);
        switch (pck.ETHtype) {
            case ipIfc4.type:
                int i = cls.entry.updateTos(pck.IPtos);
                if ((pck.IPtos == i) && (pck.IPttl == o)) {
                    return;
                }
                pck.getSkip(2);
                ip4cor.updateIPheader(pck, null, null, -1, o, i, -1);
                pck.getSkip(-2);
                return;
            case ipIfc6.type:
                i = cls.entry.updateTos(pck.IPtos);
                if ((pck.IPtos == i) && (pck.IPttl == o)) {
                    return;
                }
                pck.getSkip(2);
                ip6cor.updateIPheader(pck, null, null, -1, o, i, -1);
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
                ifcDot1q.createHeader(pck);
                return;
            case ifcDot1ah.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcDot1ah.size);
                ifcDot1ah.createHeader(pck);
                return;
            case ifcDot1ad.type:
                i = pck.ETHcos;
                pck.ETHcos = cls.entry.cosSet.update(i);
                if (i == pck.ETHcos) {
                    return;
                }
                pck.getSkip(ifcDot1ad.size);
                ifcDot1ad.createHeader(pck);
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
            cls.countPack++;
            cls.countByte += pck.dataSize();
            return pck;
        }
        return null;
    }

}
