package org.freertr.cfg;

import java.util.List;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * generic config item
 *
 * @author matecsaba
 */
public interface cfgGeneric {

    /**
     * get help text
     *
     * @param l help text
     */
    public void getHelp(userHelp l);

    /**
     * get configuration of this item
     *
     * @param filter mode, 1=filter defaults, 2=hide secrets
     * @return string list
     */
    public List<String> getShRun(int filter);

    /**
     * convert one command line
     *
     * @param cmd command to process
     */
    public void doCfgStr(cmds cmd);

    /**
     * get prompt text
     *
     * @return prompt text
     */
    public String getPrompt();

}
