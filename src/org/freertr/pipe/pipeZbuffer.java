package org.freertr.pipe;

import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * z buffered screen
 *
 * @author matecsaba
 */
public class pipeZbuffer {

    /**
     * screen to size
     */
    public final pipeScreen scr;

    /**
     * depths
     */
    protected final double[][] dep;

    /**
     * max size
     */
    public final int max;

    /**
     * rotation vector
     */
    protected double a = 0;

    /**
     * rotation vector
     */
    protected double b = 0;

    /**
     * rotation vector
     */
    protected double c = 0;

    /**
     * model coordinates
     */
    protected List<Double> rx;

    /**
     * model coordinates
     */
    protected List<Double> ry;

    /**
     * model coordinates
     */
    protected List<Double> rz;

    /**
     * create instance
     *
     * @param s screen to use
     */
    public pipeZbuffer(pipeScreen s) {
        scr = s;
        dep = new double[scr.sizY][scr.sizX];
        if (scr.sizX > scr.sizY) {
            max = scr.sizY;
        } else {
            max = scr.sizX;
        }
        clear();
    }

    /**
     * clear depths
     */
    public void clear() {
        for (int i = 0; i < scr.sizY; i++) {
            for (int o = 0; o < scr.sizX; o++) {
                dep[i][o] = Double.MIN_VALUE;
            }
        }
        scr.doClear();
    }

    /**
     * clear screen
     */
    public void putCls() {
        clear();
        scr.doClear();
    }

    /**
     * rotate
     */
    public void rotate() {
        a += bits.random(10, 20) / 100.0;
        b += bits.random(10, 20) / 100.0;
        c += bits.random(20, 40) / 100.0;
    }

    /**
     * refresh screen
     */
    public void refresh() {
        scr.refresh();
    }

    /**
     * draw pixe
     *
     * @param cx x coordinate
     * @param cy y coordinate
     * @param cz z coordinate
     * @param bg background color
     * @param fg foreground color
     * @param ch character to write
     */
    public void pixel(double cx, double cy, double cz, int bg, int fg, int ch) {
        double x = cy * Math.sin(a) * Math.sin(b) * Math.cos(c)
                - cz * Math.cos(a) * Math.sin(b) * Math.cos(c)
                + cy * Math.cos(a) * Math.sin(c)
                + cz * Math.sin(a) * Math.sin(c)
                + cx * Math.cos(b) * Math.cos(c);
        double y = cy * Math.cos(a) * Math.cos(c)
                + cz * Math.sin(a) * Math.cos(c)
                - cy * Math.sin(a) * Math.sin(b) * Math.sin(c)
                + cz * Math.cos(a) * Math.sin(b) * Math.sin(c)
                - cx * Math.cos(b) * Math.sin(c);
        double z = cz * Math.cos(a) * Math.cos(b)
                - cy * Math.sin(a) * Math.cos(b)
                + cx * Math.sin(b) + max * 3;
        if (z == 0.0) {
            z = 0.000001;
        }
        double ooz = max / z / 1.6;
        int px = (int) (scr.sizX / 2 + ooz * x * 2);
        int py = (int) (scr.sizY / 2 + ooz * y);
        if (px < 0) {
            return;
        }
        if (py < 0) {
            return;
        }
        if (px >= scr.sizX) {
            return;
        }
        if (py >= scr.sizY) {
            return;
        }
        if (ooz <= dep[py][px]) {
            return;
        }
        dep[py][px] = ooz;
        scr.putInt(px, py, bg, fg, false, ch);
    }

    /**
     * new model
     */
    public void objFresh() {
        rx = new ArrayList<Double>();
        ry = new ArrayList<Double>();
        rz = new ArrayList<Double>();
    }

    /**
     * read up model
     *
     * @param lst obj text file
     */
    public void objReadUp(List<String> lst) {
        if (lst == null) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            String s = lst.get(i);
            cmds cmd = new cmds("f", s);
            if (!cmd.word().equals("v")) {
                continue;
            }
            double cx = objReadOne(cmd.word());
            double cy = objReadOne(cmd.word());
            double cz = objReadOne(cmd.word());
            rx.add(cx);
            ry.add(cy);
            rz.add(cz);
        }
    }

    private static double objReadOne(String a) {
        try {
            return Double.parseDouble(a);
        } catch (Exception e) {
            return 0.0;
        }
    }

    /**
     * read up model
     *
     * @param lst text to read
     */
    public void objFromTxt(List<String> lst) {
        for (int o = 0; o < lst.size(); o++) {
            String a = lst.get(o);
            byte[] b = a.getBytes();
            for (int i = 0; i < b.length; i++) {
                if (b[i] == 32) {
                    continue;
                }
                rx.add((double) i);
                ry.add((double) o);
                rz.add(-1.0);
            }
        }
    }

    /**
     * resize model
     */
    public void objReSize() {
        if (rx.size() < 1) {
            return;
        }
        double minX = Double.MAX_VALUE;
        double minY = Double.MAX_VALUE;
        double minZ = Double.MAX_VALUE;
        double maxX = Double.MIN_VALUE;
        double maxY = Double.MIN_VALUE;
        double maxZ = Double.MIN_VALUE;
        for (int i = 0; i < rx.size(); i++) {
            double cx = rx.get(i);
            double cy = ry.get(i);
            double cz = rz.get(i);
            if (cx < minX) {
                minX = cx;
            }
            if (cy < minY) {
                minY = cy;
            }
            if (cz < minZ) {
                minZ = cz;
            }
            if (cx > maxX) {
                maxX = cx;
            }
            if (cy > maxY) {
                maxY = cy;
            }
            if (cz > maxZ) {
                maxZ = cz;
            }
        }
        maxX -= minX;
        maxY -= minY;
        maxZ -= minZ;
        maxX /= 2.0;
        maxY /= 2.0;
        maxZ /= 2.0;
        minX += maxX;
        minY += maxY;
        minZ += maxZ;
        for (int i = 0; i < rx.size(); i++) {
            rx.set(i, (double) scr.sizX * (rx.get(i) - minX) / maxX);
            ry.set(i, (double) scr.sizY * (ry.get(i) - minY) / maxY);
            rz.set(i, (double) max * (rz.get(i) - minZ) / maxZ);
        }
    }

    /**
     * draw model
     *
     * @param bg background color
     * @param fg foreground color
     * @param ch character to write
     */
    public void objDraw(int bg, int fg, int ch) {
        for (int i = 0; i < rx.size(); i++) {
            pixel(rx.get(i), ry.get(i), rz.get(i), bg, fg, ch);
        }
    }

}
