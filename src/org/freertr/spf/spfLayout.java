package org.freertr.spf;

import java.util.ArrayList;
import java.util.List;
import org.freertr.tab.tabGen;
import org.freertr.user.userHelp;

/**
 * spf layout
 *
 * @author matecsaba
 */
public class spfLayout {

    private final int fmt;

    private final boolean mets;

    private final List<String> res;

    private final tabGen<String> json;

    /**
     * format to string;
     *
     * @param o current value
     * @param a string
     * @return format
     */
    public static int string2format(int o, String a) {
        if (a.equals("fmt-dot")) {
            return 0;
        }
        if (a.equals("fmt-cli")) {
            return 1;
        }
        if (a.equals("fmt-json")) {
            return 2;
        }
        return o;
    }

    /**
     * get format help
     *
     * @param hl help
     * @param cur current
     */
    public static void format2help(userHelp hl, int cur) {
        hl.add(null, false, cur, new int[]{cur, -1}, "fmt-dot", "dot format");
        hl.add(null, false, cur, new int[]{cur, -1}, "fmt-cli", "cli format");
        hl.add(null, false, cur, new int[]{cur, -1}, "fmt-json", "json format");
    }

    /**
     * create instance
     *
     * @param fm fmt
     * @param px pix
     * @param met met
     */
    public spfLayout(int fm, String px, boolean met) {
        fmt = fm;
        mets = met;
        res = new ArrayList<String>();
        if (fm >= 2) {
            json = new tabGen<String>();
            res.add("{ \"links\": [");
            return;
        }
        json = null;
        if (fm >= 1) {
            res.add("dot -Tpng > net.png << EOF");
        }
        res.add("graph net {");
        if (px == null) {
            px = "";
        } else {
            px = ",labelloc=b,image=\"" + px + "\"";
        }
        res.add("node [fontname=ubuntu,shape=none" + px + "] edge [fontname=ubuntu,shape=none]");
    }

    /**
     * get result
     *
     * @return result
     */
    public List<String> getRes() {
        if (json == null) {
            res.add("}");
            if (fmt >= 1) {
                res.add("EOF");
            }
            return res;
        }
        stripComma();
        res.add("] , \"nodes\": [");
        for (int i = 0; i < json.size(); i++) {
            res.add("{\"id\": \"" + json.get(i) + "\"},");
        }
        stripComma();
        res.add("] }");
        return res;
    }

    private void stripComma() {
        int pos = res.size() - 1;
        if (pos < 0) {
            return;
        }
        String a = res.get(pos);
        if (!a.endsWith(",")) {
            return;
        }
        a = a.substring(0, a.length() - 1);
        res.set(pos, a);
    }

    /**
     * add node
     *
     * @param nam name
     * @param pos position
     */
    public void addNode(String nam, String pos) {
        if (json != null) {
            json.add(nam);
            return;
        }
        String a = "\"" + nam + "\"";
        if (pos != null) {
            a += " [pin=true pos=\"" + pos + "\"]";
        }
        res.add(a);
    }

    /**
     * add link
     *
     * @param src source
     * @param trg target
     * @param met metric
     * @param tal tail label
     * @param hed head label
     */
    public void addLink(String src, String trg, int met, String tal, String hed) {
        if (json != null) {
            json.add(src);
            json.add(trg);
            res.add("{\"source\": \"" + src + "\", \"target\": \"" + trg + "\", \"value\": " + met + "},");
            return;
        }
        String a = " [weight=" + met + "]";
        if (tal != null) {
            a += " [taillabel=\"" + tal + "\"]";
        }
        if (hed != null) {
            a += "[headlabel=" + hed + "]";
        }
        if (mets) {
            a += " [label=\"" + met + "\"]";
        }
        res.add("\"" + src + "\" -- \"" + trg + "\"" + a);
    }

}
