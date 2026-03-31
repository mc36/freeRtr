package org.freertr.spf;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeScreen;
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

    private final tabGen<spfLayoutNode> nodes;

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
        if (a.equals("fmt-text")) {
            return 3;
        }
        if (a.equals("fmt-svg")) {
            return 4;
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
        hl.add(null, false, cur, new int[]{cur, -1}, "fmt-text", "text format");
        hl.add(null, false, cur, new int[]{cur, -1}, "fmt-svg", "text format");
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
        if (fm >= 3) {
            json = null;
            nodes = new tabGen<spfLayoutNode>();
            return;
        }
        nodes = null;
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
        if (nodes == null) {
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
        for (int o = 0; o < nodes.size(); o++) {
            spfLayoutNode cur = nodes.get(o);
            double a = 2 * o * Math.PI;
            a /= nodes.size();
            cur.cx = Math.cos(a) / 3;
            cur.cy = Math.sin(a) / 3;
            cur.vx = 0;
            cur.vy = 0;
        }
        for (int r = 0; r < 1000; r++) {
            double t = 10.0 + (double) r;
            t = 1.0 / t;
            double k = 0.4 * Math.sqrt(1.0 / nodes.size());
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                cur.vx = 0.0;
                cur.vy = 0.0;
                for (int i = 0; i < nodes.size(); i++) {
                    if (o == i) {
                        continue;
                    }
                    spfLayoutNode oth = nodes.get(i);
                    double x = cur.cx - oth.cx;
                    double y = cur.cy - oth.cy;
                    double d = Math.sqrt((x * x) + (y * y));
                    if (d > 0.0) {
                        x /= d;
                        y /= d;
                        d = k * k / d;
                    }
                    x *= d;
                    y *= d;
                    cur.vx += x;
                    cur.vy += y;
                }
            }
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                for (int i = 0; i < cur.lnk.size(); i++) {
                    if (o == i) {
                        continue;
                    }
                    spfLayoutLink lnk = cur.lnk.get(i);
                    spfLayoutNode oth = lnk.nam;
                    double x = cur.cx - oth.cx;
                    double y = cur.cy - oth.cy;
                    double d = Math.sqrt((x * x) + (y * y));
                    if (d > 0.0) {
                        x /= d;
                        y /= d;
                    }
                    d = d * d / k;
                    x *= d;
                    y *= d;
                    cur.vx -= x;
                    cur.vy -= y;
                    oth.vx += x;
                    oth.vy += y;
                }
            }
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                double x = cur.vx;
                double y = cur.vy;
                double d = Math.sqrt((x * x) + (y * y));
                if (d > 0.0) {
                    x /= d;
                    y /= d;
                }
                d = limitDouble(d, -t, t);
                x *= d;
                y *= d;
                cur.cx += x;
                cur.cy += y;
                cur.cx = limitDouble(cur.cx, -0.45, +0.45);
                cur.cy = limitDouble(cur.cy, -0.45, +0.45);
            }
        }
        if (fmt < 4) {
            pipeScreen scr = new pipeScreen(pipeDiscard.needAny(null));
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                int cx = cur.getX(scr.sizX);
                int cy = cur.getY(scr.sizY);
                for (int i = 0; i < cur.lnk.size(); i++) {
                    spfLayoutNode oth = cur.lnk.get(i).nam;
                    int ox = oth.getX(scr.sizX);
                    int oy = oth.getY(scr.sizY);
                    scr.drawLine(cx, cy, ox, oy, pipeScreen.colBlack, pipeScreen.colWhite, '*');
                }
                scr.putStr(cx, cy, pipeScreen.colBlack, pipeScreen.colBrGreen, false, cur.nam);
            }
            return scr.getAscii();
        }
        res.add("<?xml version=\"1.0\" ?>");
        res.add("<!DOCTYPE svg>");
        final int max = 1000;
        res.add("<svg width=\"" + max + "\" height=\"" + max + "\" viewBox=\"0 0 " + max + " " + max + "\">");
        res.add("<rect fill=\"black\" x=\"0\" y=\"0\" width=\"" + max + "\" height=\"" + max + "\"/>");
        for (int o = 0; o < nodes.size(); o++) {
            spfLayoutNode cur = nodes.get(o);
            int cx = cur.getX(max);
            int cy = cur.getY(max);
            for (int i = 0; i < cur.lnk.size(); i++) {
                spfLayoutNode oth = cur.lnk.get(i).nam;
                int ox = oth.getX(max);
                int oy = oth.getY(max);
                res.add("<line x1=\"" + cx + "\" y1=\"" + cy + "\" x2=\"" + ox + "\" y2=\"" + oy + "\" stroke=\"gray\"/>");
            }
            res.add("<text x=\"" + cx + "\" y=\"" + cy + "\" font-size=\"14.0\" fill=\"white\">" + cur.nam + "</text>");
        }
        res.add("</svg>");
        return res;
    }

    private double limitDouble(double val, double min, double max) {
        if (val < min) {
            val = min;
        }
        if (val > max) {
            val = max;
        }
        return val;
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
        if (nodes != null) {
            nodes.add(new spfLayoutNode(nam));
            return;
        }
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
        if (nodes != null) {
            spfLayoutNode sn = new spfLayoutNode(src);
            spfLayoutNode tn = new spfLayoutNode(trg);
            spfLayoutNode so = nodes.add(sn);
            spfLayoutNode to = nodes.add(tn);
            if (so == null) {
                so = sn;
            }
            if (to == null) {
                to = tn;
            }
            so.lnk.add(new spfLayoutLink(to, met));
            return;
        }
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

class spfLayoutNode implements Comparable<spfLayoutNode> {

    public final String nam;

    public final List<spfLayoutLink> lnk;

    public double cx;

    public double cy;

    public double vx;

    public double vy;

    public spfLayoutNode(String n) {
        nam = n;
        lnk = new ArrayList<spfLayoutLink>();
    }

    public int compareTo(spfLayoutNode o) {
        return nam.compareTo(o.nam);
    }

    public int getX(int max) {
        return (max >>> 1) + (int) (cx * (double) max);
    }

    public int getY(int max) {
        return (max >>> 1) + (int) (cy * (double) max);
    }

}

class spfLayoutLink implements Comparable<spfLayoutLink> {

    public final spfLayoutNode nam;

    public final int val;

    public spfLayoutLink(spfLayoutNode n, int v) {
        nam = n;
        val = v;
    }

    public int compareTo(spfLayoutLink o) {
        return nam.compareTo(o.nam);
    }

}
