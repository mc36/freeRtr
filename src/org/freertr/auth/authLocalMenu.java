package org.freertr.auth;

import org.freertr.pipe.pipeScreen;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userReader;

/**
 * run tui based menu
 *
 * @author matecsaba
 */
public class authLocalMenu {

    private final authLocal database;

    private final pipeScreen console;

    private boolean changed;

    /**
     * create instance
     *
     * @param loc database to use
     * @param pipe pipe to use
     */
    public authLocalMenu(authLocal loc, pipeSide pipe) {
        database = loc;
        console = new pipeScreen(pipe);
    }

    /**
     * do menu
     *
     * @param prv privileged
     * @param true to save config, false if no changes made
     */
    public boolean doMenu(boolean prv) {
        if (!database.menuEna) {
            return false;
        }
        if (!prv && !database.menuGst) {
            return false;
        }
        console.putCls();
        /**
         * doReset(); doFilter(); for (;;) { doRange(); doDraw(false); if
         * (doKey()) { break; } } doClear();
         */
        return changed & database.menuAwr;
    }

}
