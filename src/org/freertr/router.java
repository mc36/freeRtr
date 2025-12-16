package org.freertr;

import org.freertr.cfg.cfgInit;

/**
 * main class for the router
 *
 * @author matecsaba
 */
public class router {

    /**
     * create instance
     */
    public router() {
    }

    /**
     * this is the main method for the jar file
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        cfgInit.doMain(args);
    }

}
