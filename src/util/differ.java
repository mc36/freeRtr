package util;

import java.util.ArrayList;
import java.util.List;

/**
 * difference generator
 *
 * @author matecsaba
 */
public class differ {

    private List<differLin> r1;

    private List<differLin> r2;

    /**
     * calculate diff
     *
     * @param t1 text1
     * @param t2 text2
     */
    public void calc(List<String> t1, List<String> t2) {
        int p1 = 0;
        int p2 = 0;
        r1 = new ArrayList<differLin>();
        r2 = new ArrayList<differLin>();
        for (;;) {
            String l1 = null;
            String l2 = null;
            if (p1 < t1.size()) {
                l1 = t1.get(p1);
                p1++;
            }
            if (p2 < t2.size()) {
                l2 = t2.get(p2);
                p2++;
            }
            if ((l1 == null) && (l2 == null)) {
                break;
            }
            if (l1 == null) {
                l1 = "";
            }
            if (l2 == null) {
                l2 = "";
            }
            r1.add(new differLin(l1));
            r2.add(new differLin(l2));
        }
    }

    /**
     * get text results
     *
     * @param wid width of screen
     * @param ofs offset on screen
     * @return results
     */
    public List<String> getText(int wid, int ofs) {
        wid = (wid - 3) / 2;
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < r1.size(); i++) {
            String l1 = "" + r1.get(i);
            String l2 = "" + r2.get(i);
            String a = l1.equals(l2) ? " | " : " - ";
            lst.add(bits.padBeg("", ofs, " ") + getLine(l1, wid, ofs) + a + getLine(l2, wid, ofs));
        }
        return lst;
    }

    private String getLine(String s, int wid, int ofs) {
        if (ofs < s.length()) {
            s = s.substring(ofs, s.length());
        } else {
            s = "";
        }
        return bits.padEnd(s, wid, " ").substring(0, wid);
    }

}

class differLin {

    public String ln;

    public differLin(String s) {
        ln = s;
    }

    public String toString() {
        return ln;
    }

}
