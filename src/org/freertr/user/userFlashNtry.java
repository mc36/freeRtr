package org.freertr.user;

import java.io.File;

/**
 * represents one file
 *
 * @author matecsaba
 */
public class userFlashNtry implements Comparable<userFlashNtry> {

    /**
     * file entry
     */
    protected final File f;

    /**
     * create instance
     *
     * @param fl file
     */
    protected userFlashNtry(File fl) {
        f = fl;
    }

    public int compareTo(userFlashNtry o) {
        return f.getName().compareTo(o.f.getName());
    }

}
