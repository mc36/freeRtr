package org.freertr.enc;

import java.io.File;
import java.io.RandomAccessFile;

/**
 * ide server
 *
 * @author matecsaba
 */
public class encIde implements Comparable<encIde> {

    /**
     * name of this file
     */
    public final String name;

    private String file;

    private RandomAccessFile hndl;

    /**
     * block size
     */
    public long blkSiz;

    private long filSiz;

    /**
     * create new handler
     */
    public encIde(String n) {
        name = "" + n;
        blkSiz = 512;
    }

    public int compareTo(encIde o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return name + ", file=" + file + ", size=" + filSiz + " block=" + blkSiz;
    }

    /**
     * update this target
     *
     * @param close true to close
     */
    public void doUpdate(boolean close) {
        try {
            hndl.close();
        } catch (Exception e) {
        }
        hndl = null;
        filSiz = 0;
        if (close) {
            return;
        }
        try {
            File fh = new File(file);
            if (!fh.isFile()) {
                return;
            }
            hndl = new RandomAccessFile(fh, "rw");
            filSiz = hndl.length() / blkSiz;
            hndl.setLength(filSiz * blkSiz);
        } catch (Exception e) {
            hndl = null;
            return;
        }
    }

}
