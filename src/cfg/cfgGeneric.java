package cfg;

import java.util.List;

import user.userHelping;
import util.cmds;

/**
 * generic config item
 *
 * @author matecsaba
 */
public interface cfgGeneric {

    /**
     * get help text
     *
     * @return help text
     */
    public userHelping getHelp();

    /**
     * get configuration of this item
     *
     * @param filter true to filter defaults, false to not
     * @return string list
     */
    public List<String> getShRun(boolean filter);

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
