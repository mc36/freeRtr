package tab;

import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import util.bits;

/**
 * qos entry
 *
 * @author matecsaba
 */
public class tabQosN {

    /**
     * traffic class entry
     */
    protected tabPlcmapN entry;

    /**
     * parent of this node (common bandwidth pool)
     */
    protected tabQosN parent;

    /**
     * skip them if not matches
     */
    protected int childs;

    /**
     * bytes used in this interval
     */
    protected int bytes;

    /**
     * beginning of interval
     */
    protected long time;

    /**
     * list of packets
     */
    protected final List<packHolder> packets;

    /**
     * packet counter
     */
    protected long countPack;

    /**
     * byte counter
     */
    protected long countByte;

    /**
     * create new qos class
     */
    public tabQosN() {
        packets = new ArrayList<packHolder>();
    }

    /**
     * get statistics
     *
     * @param l list to append
     */
    protected void getStats(List<String> l) {
        l.add("description=" + entry.description);
        l.add("  childs=" + childs + ", queues=" + packets.size() + "/" + getQueues() + ", interval=" + getInterval() + ", bytes/interval=" + getBytePerInt());
        l.add("  match=" + entry.getCounters());
        l.add("  transmit=" + countPack + " packets (" + countByte + " bytes)");
    }

    /**
     * update byte counts (in parents also)
     *
     * @param add bytes to add
     */
    protected void updateBytes(int add) {
        bytes += add;
        if (parent == null) {
            return;
        }
        parent.updateBytes(add);
    }

    /**
     * get interval in ms
     *
     * @return interval
     */
    protected int getInterval() {
        int i = entry.interval;
        if (i < 1) {
            return 100;
        } else {
            return i;
        }
    }

    /**
     * get bytes/interval
     *
     * @return bytes/int
     */
    protected int getBytePerInt() {
        int i = getInterval();
        if (i > 1000) {
            return entry.accessRate * (i / 1000);
        }
        return entry.accessRate / (1000 / i);
    }

    /**
     * get queue size
     *
     * @return queue size
     */
    protected int getQueues() {
        int i = entry.queues;
        if (i > 0) {
            return i;
        } else {
            return 128;
        }
    }

    /**
     * check if there is room for bytes
     *
     * @param add bytes to check
     * @return false if available, true if full
     */
    protected boolean checkMyBytes(int add) {
        if (bytes < 1) {
            return false;
        }
        return (bytes + add) > getBytePerInt();
    }

    /**
     * check if there is room for bytes
     *
     * @param add bytes to check
     * @return false if available, true if full
     */
    protected boolean checkPrntBytes(int add) {
        if (parent == null) {
            return false;
        }
        if (parent.checkMyBytes(add)) {
            return true;
        }
        return parent.checkPrntBytes(add);
    }

    /**
     * update time
     *
     * @param curr current time
     * @return ms left from interval, 0 means just reset
     */
    protected long updateTime(long curr) {
        long left = getInterval() - (curr - time);
        if (left > 0) {
            return left;
        }
        time = curr;
        bytes -= (entry.accessRate - entry.exceedRate);
        if (bytes < 0) {
            bytes = 0;
        }
        return 0;
    }

    /**
     * enqueue one packet
     *
     * @param pck packet to enqueue
     */
    public void enqueuePack(packHolder pck) {
        entry.countPack++;
        entry.countByte += pck.dataSize();
        if (packets.size() > getQueues()) {
            if (!entry.randomDetect) {
                return;
            }
            packets.remove(bits.random(0, packets.size()));
        }
        packets.add(pck.copyBytes(true, true));
    }

    /**
     * dequeue one packet
     *
     * @return packet got, null if nothing
     */
    public packHolder dequeuePack() {
        if (packets.size() < 1) {
            return null;
        }
        switch (entry.action) {
            case actPermit:
                return packets.remove(0);
            case actPolice:
            case actPriorty:
                packHolder pck = packets.remove(0);
                if (checkMyBytes(pck.dataSize())) {
                    packets.clear();
                    return null;
                }
                if (checkPrntBytes(pck.dataSize())) {
                    return null;
                }
                return pck;
            case actShaper:
                pck = packets.get(0);
                if (checkMyBytes(pck.dataSize())) {
                    return null;
                }
                if (checkPrntBytes(pck.dataSize())) {
                    return null;
                }
                packets.remove(0);
                return pck;
            case actBndwdth:
                pck = packets.get(0);
                if (checkMyBytes(pck.dataSize())) {
                    bytes = 0;
                    return null;
                }
                if (checkPrntBytes(pck.dataSize())) {
                    return null;
                }
                packets.remove(0);
                return pck;
            case actDeny:
            default:
                packets.clear();
                return null;
        }
    }

}
