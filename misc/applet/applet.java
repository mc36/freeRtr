package org.freertr;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeImage;
import org.freertr.pipe.pipeWindow;

/**
 * main class for the applet
 *
 * @author matecsaba
 */
@SuppressWarnings("deprecation")
public class applet extends java.applet.Applet {

    private final static long serialVersionUID = 19791025;

    /**
     * pipe image
     */
    protected pipeImage img;

    /**
     * buffered image
     */
    protected BufferedImage img3;

    public void init() {
        img = cfgInit.doApplet(getParameter("config"));
        img3 = pipeWindow.createImage(img);
        new appletDoer(this);
        setBackground(Color.BLACK);
        addKeyListener(new pipeWindowKey(img.pipe));
    }

    public void stop() {
        cfgInit.stopRouter(true, 20, "applet exited");
    }

    public void paint(Graphics g) {
        if (img == null) {
            return;
        }
        pipeWindow.updateImage(img3, img);
        g.drawImage(img3, 0, 0, null);
    }

}

class appletDoer implements Runnable {

    private final applet lower;

    public appletDoer(applet parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            boolean b = lower.img.scr.doRound(true);
            lower.img.doImage();
            lower.repaint();
            if (b) {
                return;
            }
        }
    }

}
