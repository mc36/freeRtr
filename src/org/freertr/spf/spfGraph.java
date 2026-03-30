package org.freertr.spf;

import java.util.ArrayList;
import java.util.List;
import org.freertr.tab.tabGen;

/**
 * spf graph
 *
 * @author matecsaba
 */
public class spfGraph {

    private final boolean cli;

    private final boolean mets;

    private final List<String> res;

    private final tabGen<String> json;

    /**
     * create instance
     *
     * @param js json
     * @param cl cli
     * @param sv svg
     * @param met met
     */
    public spfGraph(boolean js, boolean cl, String sv, boolean met) {
        cli = cl;
        mets = met;
        res = new ArrayList<String>();
        if (js) {
            json = new tabGen<String>();
            res.add("{ \"links\": [");
            return;
        }
        json = null;
        if (cli) {
            res.add("dot -Tpng > net.png << EOF");
        }
        res.add("graph net {");
        if (sv == null) {
            sv = "";
        } else {
            sv = ",labelloc=b,image=\"" + sv + "\"";
        }
        res.add("node [fontname=ubuntu,shape=none" + sv + "] edge [fontname=ubuntu,shape=none]");
    }

    /**
     * get result
     *
     * @return result
     */
    public List<String> getRes() {
        if (json == null) {
            res.add("}");
            if (cli) {
                res.add("EOF");
            }
            return res;
        }
        stripComma();
        res.add("] , \"nodes\": [");
        for (int i = 0; i < json.size(); i++) {
            res.add("{ \"id\": \"" + json.get(i) + "\" },");
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
