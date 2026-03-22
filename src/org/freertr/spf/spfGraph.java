package org.freertr.spf;

import java.util.ArrayList;
import java.util.List;

/**
 * spf graph
 *
 * @author matecsaba
 */
public class spfGraph {

    private final boolean cli;

    private final boolean mets;

    private final List<String> res;

    /**
     * create instance
     *
     * @param cl cli
     * @param sv svg
     * @param met met
     */
    public spfGraph(boolean cl, String sv, boolean met) {
        cli = cl;
        mets = met;
        res = new ArrayList<String>();
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
        res.add("}");
        if (cli) {
            res.add("EOF");
        }
        return res;
    }

    /**
     * add node
     *
     * @param nam name
     * @param pos position
     */
    public void addNode(String nam, String pos) {
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
