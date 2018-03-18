
import cfg.cfgInit;
import java.awt.Graphics;
import pipe.pipeImage;

/**
 * main class for the applet
 *
 * @author matecsaba
 */
@SuppressWarnings( "deprecation" )
public class applet extends java.applet.Applet {

    private static final long serialVersionUID = 19791025;

    /**
     * pipe image
     */
    protected pipeImage img;

    public void init() {
        img = cfgInit.doApplet(getParameter("config"));
        new appletDoer(this);
        setBackground(img.getBackground());
        addKeyListener(img.getKeyLstnr());
    }

    public void stop() {
        cfgInit.stopRouter(true, 1, "stopped");
    }

    public void paint(Graphics g) {
        if (img == null) {
            return;
        }
        g.drawImage(img.img3, 0, 0, null);
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
