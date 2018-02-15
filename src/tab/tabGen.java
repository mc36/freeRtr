package tab;

import java.util.Comparator;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGen<T extends Comparator<? super T>> extends tabGenV2<T> {

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
