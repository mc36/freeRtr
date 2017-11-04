package pack;

import java.util.ArrayList;
import java.util.List;

import pipe.pipeSide;
import util.bits;

/**
 * pipe interface textifier
 *
 * @author matecsaba
 */
public class packText {

    private pipeSide pipe; // pipeline side

    /**
     * create pipe blockifier
     *
     * @param stream stream to use
     */
    public packText(pipeSide stream) {
        pipe = stream;
    }

    /**
     * send dotted line
     *
     * @param s string to send
     * @return false on success, true on error
     */
    public boolean dottedSend(String s) {
        if (s.length() > 0) {
            if (s.substring(0, 1).equals(".")) {
                s = "." + s;
            }
        }
        pipe.linePut(s);
        return false;
    }

    /**
     * read one line
     *
     * @return line readed, null on error
     */
    public String dottedRecv() {
        if (pipe.isClosed() != 0) {
            return null;
        }
        String s = pipe.lineGet(0x41);
        if (s.equals(".")) {
            return null;
        }
        if (s.length() > 0) {
            if (s.substring(0, 1).equals(".")) {
                s = s.substring(1, s.length());
            }
        }
        return s;
    }

    /**
     * read all lines
     *
     * @return lines readed
     */
    public List<String> dottedRecvAll() {
        List<String> l = new ArrayList<String>();
        for (;;) {
            String s = dottedRecv();
            if (s == null) {
                break;
            }
            l.add(s);
        }
        return l;
    }

    /**
     * receive all lines
     *
     * @param lst list to append
     */
    public void recvAll(List<String> lst) {
        for (;;) {
            if (pipe.ready2rx() < 1) {
                break;
            }
            String a = pipe.lineGet(1);
            if (a.length() < 1) {
                continue;
            }
            lst.add(a);
        }
    }

    /**
     * receive all lines
     *
     * @return list of lines
     */
    public List<String> recvAll() {
        List<String> lst = new ArrayList<String>();
        recvAll(lst);
        return lst;
    }

    /**
     * send coded value
     *
     * @param c code value
     * @param s string to send
     * @return false on success, true on error
     */
    public boolean codedSend(int c, String s) {
        List<String> l = new ArrayList<String>();
        l.add(s);
        return codedSend(c, l);
    }

    /**
     * send coded lines
     *
     * @param c code value
     * @param l strings to send
     * @return false on success, true on error
     */
    public boolean codedSend(int c, List<String> l) {
        String cc = bits.padBeg("" + c, 3, "0");
        for (int i = 0; i < l.size() - 1; i++) {
            pipe.linePut(cc + "-" + l.get(i));
        }
        pipe.linePut(cc + " " + l.get(l.size() - 1));
        return false;
    }

    /**
     * send coded value
     *
     * @return last line readed, null on error
     */
    public String codedRecv() {
        for (;;) {
            String s = pipe.lineGet(0x41);
            if (s.length() < 1) {
                if (pipe.isClosed() != 0) {
                    return null;
                }
                continue;
            }
            int i = s.indexOf(" ");
            int o = s.indexOf("-");
            if (i == 1) {
                continue;
            }
            if (i < 0) {
                i = s.length();
            }
            if (o < 0) {
                o = s.length();
            }
            if (i < o) {
                return s;
            }
        }
    }

}
