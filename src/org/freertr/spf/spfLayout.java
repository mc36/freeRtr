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

    private final tabGen<spfLayoutLink> links;

    /**
     * format to string;
     *
     * @param o current value
     * @param a string
     * @return format
     */
    public static int string2format(int o, String a) {
        if (a.equals("as-dot")) {
            return 0;
        }
        if (a.equals("as-cli")) {
            return 1;
        }
        if (a.equals("as-json")) {
            return 2;
        }
        if (a.equals("as-text")) {
            return 3;
        }
        if (a.equals("as-svg")) {
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
        hl.add(null, false, cur, new int[]{cur, -1}, "as-dot", "dot format");
        hl.add(null, false, cur, new int[]{cur, -1}, "as-cli", "cli format");
        hl.add(null, false, cur, new int[]{cur, -1}, "as-json", "json format");
        hl.add(null, false, cur, new int[]{cur, -1}, "as-text", "text format");
        hl.add(null, false, cur, new int[]{cur, -1}, "as-svg", "svg format");
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
            links = new tabGen<spfLayoutLink>();
            return;
        }
        nodes = null;
        links = null;
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
            double a = (double) (2 * o) * Math.PI;
            a /= (double) nodes.size();
            cur.cx = Math.cos(a);
            cur.cy = Math.sin(a);
            cur.vx = 0.0;
            cur.vy = 0.0;
        }
        for (int r = 0; r < 1000; r++) {
            double t = 10.0 + (double) r;
            t = 1.0 / t;
            double k = 0.4 * Math.sqrt(1.0 / nodes.size());
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                for (int i = o + 1; i < nodes.size(); i++) {
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
                    oth.vx -= x;
                    oth.vy -= y;
                }
            }
            for (int o = 0; o < links.size(); o++) {
                spfLayoutLink lnk = links.get(o);
                double x = lnk.src.cx - lnk.trg.cx;
                double y = lnk.src.cy - lnk.trg.cy;
                double d = Math.sqrt((x * x) + (y * y));
                if (d > 0.0) {
                    x /= d;
                    y /= d;
                }
                d = d * d / k;
                x *= d;
                y *= d;
                lnk.src.vx -= x;
                lnk.src.vy -= y;
                lnk.trg.vx += x;
                lnk.trg.vy += y;
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
                if (d < -t) {
                    d = -t;
                }
                if (d > +t) {
                    d = +t;
                }
                x *= d;
                y *= d;
                cur.cx += x;
                cur.cy += y;
                cur.vx = 0.0;
                cur.vy = 0.0;
            }
        }
        double minX = Double.MAX_VALUE;
        double minY = Double.MAX_VALUE;
        double maxX = Double.MIN_VALUE;
        double maxY = Double.MIN_VALUE;
        for (int o = 0; o < nodes.size(); o++) {
            spfLayoutNode cur = nodes.get(o);
            double x = cur.cx;
            double y = cur.cy;
            if (x < minX) {
                minX = x;
            }
            if (y < minY) {
                minY = y;
            }
            if (x > maxX) {
                maxX = x;
            }
            if (y > maxY) {
                maxY = y;
            }
        }
        maxX -= minX;
        maxY -= minY;
        if (!(maxX > 0.0)) {
            maxX = 1.0;
        }
        if (!(maxY > 0.0)) {
            maxY = 1.0;
        }
        minX -= maxX * 0.1;
        minY -= maxY * 0.1;
        maxX *= 1.2;
        maxY *= 1.2;
        for (int o = 0; o < nodes.size(); o++) {
            spfLayoutNode cur = nodes.get(o);
            double x = cur.cx;
            double y = cur.cy;
            x -= minX;
            y -= minY;
            x /= maxX;
            y /= maxY;
            cur.cx = x;
            cur.cy = y;
        }
        if (fmt < 4) {
            pipeScreen scr = new pipeScreen(pipeDiscard.needAny(null));
            for (int i = 0; i < links.size(); i++) {
                spfLayoutLink lnk = links.get(i);
                int cx = lnk.src.getX(scr.sizX);
                int cy = lnk.src.getY(scr.sizY);
                int ox = lnk.trg.getX(scr.sizX);
                int oy = lnk.trg.getY(scr.sizY);
                scr.drawLine(cx, cy, ox, oy, pipeScreen.colBlack, pipeScreen.colWhite, '*');
            }
            for (int o = 0; o < nodes.size(); o++) {
                spfLayoutNode cur = nodes.get(o);
                int cx = cur.getX(scr.sizX);
                int cy = cur.getY(scr.sizY);
                scr.putStr(cx, cy, pipeScreen.colBlack, pipeScreen.colBrGreen, false, cur.nam);
            }
            return scr.getAscii();
        }
        res.add("<?xml version=\"1.0\" ?>");
        final int sizX = 2000;
        final int sizY = 1000;
        res.add("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " + sizX + " " + sizY + "\">");
        res.add("<rect fill=\"black\" x=\"0\" y=\"0\" width=\"" + sizX + "\" height=\"" + sizY + "\"/>");
        for (int i = 0; i < links.size(); i++) {
            spfLayoutLink lnk = links.get(i);
            int cx = lnk.src.getX(sizX);
            int cy = lnk.src.getY(sizY);
            int ox = lnk.trg.getX(sizX);
            int oy = lnk.trg.getY(sizY);
            res.add("<line x1=\"" + cx + "\" y1=\"" + cy + "\" x2=\"" + ox + "\" y2=\"" + oy + "\" stroke=\"gray\"/>");
        }
        for (int o = 0; o < nodes.size(); o++) {
            spfLayoutNode cur = nodes.get(o);
            int cx = cur.getX(sizX);
            int cy = cur.getY(sizY);
            res.add("<text x=\"" + cx + "\" y=\"" + cy + "\" font-size=\"14.0\" fill=\"white\">" + cur.nam + "</text>");
        }
        res.add("</svg>");
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
            spfLayoutNode n = new spfLayoutNode(src);
            spfLayoutNode s = nodes.add(n);
            if (s == null) {
                s = n;
            }
            n = new spfLayoutNode(trg);
            spfLayoutNode t = nodes.add(n);
            if (t == null) {
                t = n;
            }
            if (s.compareTo(t) == 0) {
                return;
            }
            spfLayoutLink l;
            l = new spfLayoutLink(t, s);
            if (links.find(l) != null) {
                return;
            }
            l = new spfLayoutLink(s, t);
            links.add(l);
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

    public double cx;

    public double cy;

    public double vx;

    public double vy;

    public spfLayoutNode(String n) {
        nam = n;
    }

    public int compareTo(spfLayoutNode o) {
        return nam.compareTo(o.nam);
    }

    public int getX(int max) {
        return (int) (cx * (double) max);
    }

    public int getY(int max) {
        return (int) (cy * (double) max);
    }
}

class spfLayoutLink implements Comparable<spfLayoutLink> {

    public final spfLayoutNode src;

    public final spfLayoutNode trg;

    public spfLayoutLink(spfLayoutNode s, spfLayoutNode t) {
        src = s;
        trg = t;
    }

    public int compareTo(spfLayoutLink o) {
        int i = src.compareTo(o.src);
        if (i != 0) {
            return i;
        }
        return trg.compareTo(o.trg);
    }

}
