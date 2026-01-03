package org.freertr.ifc;

import java.util.List;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * atm encapsulation handler
 *
 * @author matecsaba
 */
public class ifcAtmEnc {

    /**
     * snap mode
     */
    public static final int ancSnap = 1;

    /**
     * mux mode
     */
    public static final int ancMux = 2;

    /**
     * nlpid mode
     */
    public static final int ancNlpid = 3;

    /**
     * create instance
     */
    public ifcAtmEnc() {
    }

    /**
     * current mode
     */
    public int ancMode = ancSnap;

    /**
     * get help text
     *
     * @param l storage
     */
    public static void ancHelp(userHelp l) {
        l.add(null, false, 2, new int[]{3}, "payload", "set payload encapsulation");
        l.add(null, false, 3, new int[]{-1}, "snap", "snap format");
        l.add(null, false, 3, new int[]{-1}, "mux", "mux format");
        l.add(null, false, 3, new int[]{-1}, "nlpid", "nlpid format");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void ancConfig(List<String> l, String beg) {
        String a;
        switch (ancMode) {
            case ancSnap:
                a = "snap";
                break;
            case ancMux:
                a = "mux";
                break;
            case ancNlpid:
                a = "nlpid";
                break;
            default:
                a = "unknown=" + ancMode;
                break;
        }
        l.add(beg + "payload " + a);
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd command
     */
    public void ancConfig(String a, cmds cmd) {
        if (!a.equals("payload")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("snap")) {
            ancMode = ancSnap;
            return;
        }
        if (a.equals("mux")) {
            ancMode = ancMux;
            return;
        }
        if (a.equals("nlpid")) {
            ancMode = ancNlpid;
            return;
        }
        cmd.badCmd();
    }

    /**
     * send packet
     *
     * @param pck packet
     * @return true on error, false on success
     */
    public boolean ancSend(packHolder pck) {
        switch (ancMode) {
            case ancSnap:
                pck.putByte(0, 0xaa);
                pck.putByte(1, 0xaa);
                pck.putByte(2, 0x03);
                pck.putByte(3, 0);
                pck.putByte(4, 0);
                pck.putByte(5, 0);
                pck.putSkip(6);
                pck.merge2beg();
                return false;
            default:
                return true;
        }
    }

    /**
     * receive packet
     *
     * @param pck packet
     * @return true on error, false on success
     */
    public boolean ancRecv(packHolder pck) {
        switch (ancMode) {
            case ancSnap:
                pck.getSkip(6);
                return false;
            default:
                return true;
        }
    }

}
