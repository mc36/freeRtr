package org.freertr.pipe;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.tab.tabGen;
import org.freertr.user.userHelp;
import org.freertr.user.userScript;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * chat script for lines
 *
 * @author matecsaba
 */
public class pipeChat {

    /**
     * create instance
     */
    public pipeChat() {
    }

    /**
     * name of script
     */
    public String scrName;

    /**
     * actions
     */
    protected tabGen<pipeScriptNtry> actions = new tabGen<pipeScriptNtry>();

    /**
     * reindex script
     *
     * @param beg first number
     * @param inc increment
     */
    public void reindex(int beg, int inc) {
        if (beg < 1) {
            beg = 10;
        }
        if (inc < 1) {
            inc = 10;
        }
        for (int i = 0; i < actions.size(); i++) {
            pipeScriptNtry t = actions.get(i);
            t.seq = (inc * i) + beg;
        }
    }

    /**
     * get config
     *
     * @param beg beginning string
     * @param filter filter defaults
     * @return configuration
     */
    public List<String> getCfg(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < actions.size(); i++) {
            l.add(beg + actions.get(i).getCfg(filter));
        }
        return l;
    }

    /**
     * do configuration
     *
     * @param cmd command to do
     * @param negate negated command
     * @return false on success, true on error
     */
    public boolean doCfg(cmds cmd, boolean negate) {
        pipeScriptNtry ntry = new pipeScriptNtry();
        if (ntry.fromString(cmd)) {
            return true;
        }
        if (negate) {
            actions.del(ntry);
            return false;
        }
        if (ntry.seq < 1) {
            ntry.seq = 0;
            pipeScriptNtry old = actions.get(actions.size() - 1);
            if (old == null) {
                old = ntry;
            }
            ntry.seq = old.seq + 10;
        }
        actions.put(ntry);
        return false;
    }

    /**
     * get help text
     *
     * @param l where to write
     */
    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "set sequence number");
        l.add(null, false, 2, new int[]{1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "tcl", "execute tcl commands");
        l.add(null, false, 2, new int[]{2, -1}, "<text>", "tcl command");
        l.add(null, false, 1, new int[]{2}, "sleep", "wait some time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms to wait");
        l.add(null, false, 1, new int[]{-1}, "success", "exit with success");
        l.add(null, false, 1, new int[]{-1}, "failure", "exit with failure");
        l.add(null, false, 1, new int[]{-1}, "disconnect", "disconnect session");
        l.add(null, false, 1, new int[]{2}, "onerror", "go to sequence on error");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms to wait");
        l.add(null, false, 1, new int[]{2}, "goto", "go to sequence");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms to wait");
        l.add(null, false, 1, new int[]{2}, "send", "send text to line");
        l.add(null, false, 2, new int[]{2, -1}, "<text>", "text to send");
        l.add(null, false, 1, new int[]{2}, "password", "send text to line");
        l.add(null, false, 2, new int[]{2, -1}, "<text>", "text to send");
        l.add(null, false, 1, new int[]{2}, "recv", "wait for text to arrive");
        l.add(null, false, 2, new int[]{3}, "<time>", "timeout in ms");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "regexp to wait");
        l.add(null, false, 1, new int[]{2}, "binsend", "send binary to line");
        l.add(null, false, 2, new int[]{2, -1}, "<text>", "ascii code");
        l.add(null, false, 1, new int[]{2}, "binrecv", "wait for binary to arrive");
        l.add(null, false, 2, new int[]{3}, "<time>", "timeout in ms");
        l.add(null, false, 3, new int[]{3, -1}, "<text>", "regexp to wait");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex time map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this chat map");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
    }

    /**
     * play this chat script
     *
     * @param pipe pipeline to use
     * @return false on success, true on error
     */
    public boolean doScript(pipeSide pipe) {
        if (debugger.lineScript) {
            logger.debug("starting script");
        }
        int sq = 0;
        int oe = -1;
        for (;;) {
            if (sq >= actions.size()) {
                if (debugger.lineScript) {
                    logger.debug("script successful");
                }
                return false;
            }
            pipeScriptNtry ntry = actions.get(sq);
            if (ntry == null) {
                sq++;
                continue;
            }
            if (debugger.lineScript) {
                logger.debug("command: " + ntry);
            }
            String s = ntry.playThis(pipe);
            if (s == null) {
                sq++;
                continue;
            }
            if (s.length() > 0) {
                if (oe >= 0) {
                    sq = oe;
                    oe = -1;
                    continue;
                }
                if (debugger.lineScript) {
                    logger.debug("script failed: " + s);
                }
                return true;
            }
            switch (ntry.act) {
                case success:
                    return false;
                case failure:
                    return true;
                case gotoo:
                    ntry.seq = ntry.tim;
                    sq = indexOf(ntry);
                    if (sq < 0) {
                        return true;
                    }
                    continue;
                case onerror:
                    ntry.seq = ntry.tim;
                    oe = indexOf(ntry);
                    sq++;
                    continue;
                default:
                    break;
            }
            sq++;
        }
    }

    private int indexOf(pipeScriptNtry ntry) {
        for (int i = 0; i < actions.size(); i++) {
            if (ntry.compareTo(actions.get(i)) == 0) {
                return i;
            }
        }
        return -1;
    }

}

class pipeScriptNtry implements Comparable<pipeScriptNtry> {

    public int seq;

    public command act;

    public int tim;

    public String str;

    public enum command {
        descr,
        txtTx, txtRx, passwd,
        binTx, binRx,
        sleep, tcl,
        success, failure, onerror, gotoo, disconn
    }

    public int compareTo(pipeScriptNtry o) {
        if (seq < o.seq) {
            return -1;
        }
        if (seq > o.seq) {
            return +1;
        }
        return 0;
    }

    public String getCfg(int filter) {
        String s;
        switch (act) {
            case descr:
                s = "description " + str;
                break;
            case txtTx:
                s = "send " + str;
                break;
            case txtRx:
                s = "recv " + tim + " " + str;
                break;
            case passwd:
                s = "password " + authLocal.passwdEncode(str, (filter & 2) != 0);
                break;
            case binTx:
                s = "binsend " + str;
                break;
            case binRx:
                s = "binrecv " + tim + " " + str;
                break;
            case sleep:
                s = "sleep " + tim;
                break;
            case tcl:
                s = "tcl " + str;
                break;
            case disconn:
                s = "disconnect";
                break;
            case success:
                s = "success";
                break;
            case failure:
                s = "failure";
                break;
            case onerror:
                s = "onerror " + tim;
                break;
            case gotoo:
                s = "goto " + tim;
                break;
            default:
                s = "unknown";
                break;
        }
        return "sequence " + seq + " " + s;
    }

    public boolean fromString(cmds cmd) {
        String s = cmd.word();
        if (s.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            s = cmd.word();
        }
        if (s.equals("description")) {
            act = command.descr;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("send")) {
            act = command.txtTx;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("recv")) {
            act = command.txtRx;
            tim = bits.str2num(cmd.word());
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("password")) {
            act = command.passwd;
            str = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("binsend")) {
            act = command.binTx;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("binrecv")) {
            act = command.binRx;
            tim = bits.str2num(cmd.word());
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("sleep")) {
            act = command.sleep;
            tim = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("tcl")) {
            act = command.tcl;
            str = cmd.getRemaining();
            return false;
        }
        if (s.equals("disconnect")) {
            act = command.disconn;
            return false;
        }
        if (s.equals("success")) {
            act = command.success;
            return false;
        }
        if (s.equals("failure")) {
            act = command.failure;
            return false;
        }
        if (s.equals("onerror")) {
            act = command.onerror;
            tim = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("goto")) {
            act = command.gotoo;
            tim = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    public String playThis(pipeSide pipe) {
        switch (act) {
            case txtTx:
            case passwd:
                pipe.strPut(str);
                return null;
            case binTx:
                byte[] buf = new byte[1];
                buf[0] = (byte) (bits.str2num(str) & 0xff);
                pipe.blockingPut(buf, 0, buf.length);
                return null;
            case txtRx:
                long beg = bits.getTime();
                String s = "";
                for (;;) {
                    if ((bits.getTime() - beg) > tim) {
                        return "timeout, buffer=" + s;
                    }
                    buf = new byte[1];
                    if (pipe.nonBlockGet(buf, 0, buf.length) != buf.length) {
                        if (pipe.isClosed() != 0) {
                            return "connection closed";
                        }
                        bits.sleep(500);
                        continue;
                    }
                    if (buf[0] < 32) {
                        buf[0] = 32;
                    }
                    s += new String(buf);
                    if (s.matches(str)) {
                        break;
                    }
                    int i = s.length() - 512;
                    if (i < 0) {
                        i = 0;
                    }
                    s = s.substring(i, s.length());
                }
                return null;
            case binRx:
                beg = bits.getTime();
                for (;;) {
                    if ((bits.getTime() - beg) > tim) {
                        return "timeout";
                    }
                    buf = new byte[1];
                    if (pipe.nonBlockGet(buf, 0, buf.length) != buf.length) {
                        if (pipe.isClosed() != 0) {
                            return "connection closed";
                        }
                        bits.sleep(500);
                        continue;
                    }
                    String a = "" + (buf[0] & 0xff);
                    if (a.matches(str)) {
                        break;
                    }
                }
                return null;
            case sleep:
                bits.sleep(tim);
                return null;
            case tcl:
                userScript t = new userScript(pipe, str);
                t.cmdAll();
                return null;
            case disconn:
                pipe.setClose();
                return null;
            case success:
                return "";
            case failure:
                return "failure by request";
            case onerror:
                return "";
            case gotoo:
                return "";
            case descr:
                return null;
            default:
                return "bad command";
        }
    }

}
