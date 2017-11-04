package pipe;

import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * one terminal window
 *
 * @author matecsaba
 */
public class pipeWindow extends JPanel {

    private static final long serialVersionUID = 1L;

    /**
     * pipe image
     */
    protected pipeImage img;

    /**
     * window handler
     */
    protected JFrame win;

    /**
     * create window
     *
     * @param x x size
     * @param y y size
     * @param fnt font to use
     * @param plt palette to use
     * @return pipeline to use
     */
    public static pipeSide create(int x, int y, short[] fnt, int[] plt) {
        pipeLine pip = new pipeLine(65536, false);
        pipeWindow win = new pipeWindow(pip.getSide(), x, y, fnt, plt);
        win.startWindow();
        return pip.getSide();
    }

    public void paint(Graphics g) {
        g.drawImage(img.img3, 0, 0, null);
    }

    /**
     * create window
     *
     * @param pip pipeline to use
     * @param x x size
     * @param y y size
     * @param fnt font to use
     * @param plt palette to use
     */
    public pipeWindow(pipeSide pip, int x, int y, short[] fnt, int[] plt) {
        img = new pipeImage(pip, x, y, fnt, plt);
        win = new JFrame("console");
        win.setBackground(img.getBackground());
        win.addKeyListener(img.getKeyLstnr());
        win.add(this);
        win.setSize(img.img3.getWidth() + 30, img.img3.getHeight() + 60);
        win.setVisible(true);
    }

    /**
     * start new window
     */
    public void startWindow() {
        new pipeWindowDoer(this);
    }

    /**
     * stop new window
     */
    public void stopWindow() {
        img.setClose();
    }

}

class pipeWindowDoer implements Runnable {

    private pipeWindow lower;

    public pipeWindowDoer(pipeWindow parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            boolean b = lower.img.doRound(true);
            lower.img.doImage();
            lower.repaint();
            if (b) {
                break;
            }
        }
        lower.win.setVisible(false);
    }

}
