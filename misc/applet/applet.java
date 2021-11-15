
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import net.freertr.cfg.cfgInit;
import net.freertr.pipe.pipeImage;
import net.freertr.pipe.pipeWindow;

/**
 * main class for the applet
 *
 * @author matecsaba
 */
@SuppressWarnings("deprecation")
public class applet extends java.applet.Applet {

    private static final long serialVersionUID = 19791025;

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
        setBackground(pipeWindow.getBckgrd());
        addKeyListener(pipeWindow.getKeyLstnr(img.pipe));
    }

    public void stop() {
        cfgInit.stopRouter(true, 1, "stopped");
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
            boolean b = lower.img.doRound(true);
            lower.img.doImage();
            lower.repaint();
            if (b) {
                return;
            }
        }
    }

}
