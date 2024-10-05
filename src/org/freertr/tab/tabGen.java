package org.freertr.tab;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public final class tabGen<T extends Comparable<? super T>> extends tabGenV2<T> {

    /**
     * create one generic table
     */
    public tabGen() {
        super();
    }

    /**
     * clone one generic table
     *
     * @param old where from copy
     */
    public tabGen(tabGen<T> old) {
        super(old);
    }

}
