struct table_head {
    int reclen;
    unsigned char *buffer;
    int size;
    int (*comparer) (void *, void *);
};


#define table_get(tab, idx)   (void*) ((tab)->buffer + ((idx) * (tab)->reclen))


void table_init(struct table_head *tab, int reclen, int comparer(void *, void *)) {
    tab->reclen = reclen;
    tab->comparer = comparer;
    tab->size = 0;
    tab->buffer = malloc(reclen * 16);
}


int table_find(struct table_head *tab, void *ntry) {
    int lower = 0;
    int upper = tab->size - 1;
    int mid;
    int cmp;
    while (lower <= upper) {
        mid = (lower + upper) >> 1;
        cmp = tab->comparer(table_get(tab, mid), ntry);
        if (cmp < 0) {
            lower = mid + 1;
            continue;
        }
        if (cmp > 0) {
            upper = mid - 1;
            continue;
        }
        return mid;
    }
    return - lower - 1;
}


void table_add(struct table_head *tab, void *ntry) {
    int idx = table_find(tab, ntry);
    if (idx >= 0) {
        memmove(table_get(tab, idx), ntry, tab->reclen);
        return;
    }
    tab->buffer = realloc(tab->buffer, tab->reclen * (tab->size+16));
    idx = -idx - 1;
    memmove(table_get(tab, idx + 1), table_get(tab, idx), (tab->size - idx) * tab->reclen);
    tab->size++;
    memmove(table_get(tab, idx), ntry, tab->reclen);
}


void table_del(struct table_head *tab, void *ntry) {
    int idx = table_find(tab, ntry);
    if (idx < 0) return;
    if (idx < (tab->size - 1)) memmove(table_get(tab, idx), table_get(tab, idx + 1), (tab->size - idx - 1) * tab->reclen);
    tab->size--;
    tab->buffer = realloc(tab->buffer, tab->reclen * (tab->size+16));
}
