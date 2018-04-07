package tab;

import cfg.cfgAll;
import cfg.cfgIfc;
import java.util.ArrayList;
import java.util.List;

import user.userFormat;
import util.bits;
import util.debugger;
import util.logger;

/**
 * represents one label table
 *
 * @author matecsaba
 */
public class tabLabel {

    /**
     * label table
     */
    public final static tabGen<tabLabelNtry> labels = new tabGen<tabLabelNtry>();

    private tabLabel() {
    }

    /**
     * get sh mpls for output
     *
     * @return list of string
     */
    public static userFormat getShFor() {
        userFormat lst = new userFormat("|", "label|vrf|iface|hop|label|targets|bytes");
        for (int i = 0; i < labels.size(); i++) {
            tabLabelNtry ntry = labels.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(ntry.getList());
        }
        return lst;
    }

    /**
     * get sh mpls int output
     *
     * @return list of string
     */
    public static userFormat getShInt() {
        userFormat lst = new userFormat("|", "interface|packet|secure|ldp4|ldp6|rsvp4|rsvp6");
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            boolean sec = false;
            if (ntry.mplsPack != null) {
                sec = ntry.mplsPack.security;
            }
            lst.add(ntry.name + "|" + (ntry.mplsPack != null) + "|" + sec + "|" + (ntry.mplsLdp4 != null) + "|" + (ntry.mplsLdp6 != null) + "|" + (ntry.mplsRsvp4 != null) + "|" + (ntry.mplsRsvp6 != null));
        }
        return lst;
    }

    /**
     * find one label
     *
     * @param label label to find
     * @return label entry, null if not found
     */
    public static tabLabelNtry find(int label) {
        tabLabelNtry ntry = new tabLabelNtry(label);
        return labels.find(ntry);
    }

    /**
     * allocate one label
     *
     * @param key key to use for deallocation
     * @return label entry, null if nothing
     */
    public static tabLabelNtry allocate(int key) {
        for (int retry = 0; retry < 16; retry++) {
            int i = bits.random(0x20, 0xffff0);
            tabLabelNtry ntry = new tabLabelNtry(i);
            if (labels.add(ntry) != null) {
                continue;
            }
            if (debugger.tabLabelEvnt) {
                logger.debug("allocate " + ntry.getValue());
            }
            ntry.key = key;
            ntry.working = true;
            return ntry;
        }
        logger.warn("failed to allocate new label");
        return null;
    }

    /**
     * allocate label block
     *
     * @param key key to use for deallocation
     * @param num number of labels
     * @return label list, null if nothing
     */
    public static tabLabelNtry[] allocate(int key, int num) {
        if (num < 1) {
            return null;
        }
        for (int retry = 0; retry < 32; retry++) {
            int beg = bits.random(0x20, 0xffff0 - num);
            tabLabelNtry[] res = new tabLabelNtry[num];
            for (int i = 0; i < num; i++) {
                tabLabelNtry ntry = new tabLabelNtry(beg + i);
                if (labels.add(ntry) != null) {
                    break;
                }
                if (debugger.tabLabelEvnt) {
                    logger.debug("allocate " + ntry.getValue());
                }
                ntry.key = key;
                ntry.working = true;
                res[i] = ntry;
            }
            if (res[num - 1] == null) {
                release(res, key);
                continue;
            }
            return res;
        }
        logger.warn("failed to allocate new label block");
        return null;
    }

    /**
     * delete one label
     *
     * @param label to delete
     * @param key key to use
     * @return label entry, null if nothing
     */
    public static tabLabelNtry release(tabLabelNtry label, int key) {
        if (label == null) {
            return null;
        }
        if (!label.working) {
            return null;
        }
        if (label.key != key) {
            return null;
        }
        if (debugger.tabLabelEvnt) {
            logger.debug("release " + label.getValue());
        }
        label = labels.del(label);
        if (label == null) {
            return null;
        }
        label.working = false;
        return label;
    }

    /**
     * delete label block
     *
     * @param label to delete
     * @param key key to use
     * @return label entry, null if nothing
     */
    public static tabLabelNtry[] release(tabLabelNtry[] label, int key) {
        if (label == null) {
            return null;
        }
        tabLabelNtry[] res = new tabLabelNtry[label.length];
        for (int i = 0; i < label.length; i++) {
            tabLabelNtry ntry = label[i];
            ntry = release(ntry, key);
            res[i] = ntry;
        }
        return res;
    }

    /**
     * convert integer to list
     *
     * @param val value to convert
     * @return converted
     */
    public static List<Integer> int2labels(int val) {
        List<Integer> res = new ArrayList<Integer>();
        res.add(val);
        return res;
    }

    /**
     * prepend some labels
     *
     * @param trg where to prepend
     * @param src labels to prepend
     * @return updated target list
     */
    public static List<Integer> prependLabels(List<Integer> trg, List<Integer> src) {
        if (src == null) {
            return trg;
        }
        if (src == trg) {
            return trg;
        }
        if (trg == null) {
            trg = new ArrayList<Integer>();
        }
        for (int i = 0; i < src.size(); i++) {
            trg.add(i, (int) src.get(i));
        }
        return trg;
    }

    /**
     * prepend one label
     *
     * @param trg where to prepend
     * @param val label to prepend
     * @return updated target list
     */
    public static List<Integer> prependLabel(List<Integer> trg, int val) {
        return prependLabels(trg, int2labels(val));
    }

    /**
     * copy labels
     *
     * @param src labels to copy
     * @return copyed labels
     */
    public static List<Integer> copyLabels(List<Integer> src) {
        return prependLabels(null, src);
    }

}
