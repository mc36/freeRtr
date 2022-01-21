struct table_head {
    int reclen;
    unsigned char *buffer;
    int size;
    int alloc;
    int (*comparer) (void *, void *);
};


#define table_get(tab, idx)   (void*) ((tab)->buffer + ((idx) * (tab)->reclen))


void table_init(struct table_head *tab, int reclen, int comparer(void *, void *)) {
    tab->reclen = reclen;
    tab->comparer = comparer;
    tab->size = 0;
    tab->alloc = 1;
    tab->buffer = malloc(reclen);
    if (tab->buffer == NULL) err("error allocating memory");
}


void table_resize(struct table_head *tab, int ned) {
    int siz = -1;
    if ((ned + 512) < tab->alloc) siz = ned + 128;
    if (ned > tab->alloc) siz = ned + 512;
    if (siz < 0) return;
    unsigned char *old = tab->buffer;
    unsigned char *new = malloc(tab->reclen * siz);
    if (new == NULL) err("error allocating memory");
    memcpy(new, old, tab->reclen * tab->size);
    tab->buffer = new;
    tab->alloc = siz;
    usleep(10000);
    free(old);
}


void table_deinit(struct table_head *tab) {
    tab->reclen = 0;
    tab->comparer = NULL;
    tab->size = 0;
    free(tab->buffer);
    tab->buffer = NULL;
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
    table_resize(tab, tab->size + 1);
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
    table_resize(tab, tab->size);
}


void* table_addinited(struct table_head *tab, void *ntry, struct table_head *tab2, int reclen, int comparer(void *, void *)) {
    int index = table_find(tab, ntry);
    if (index < 0) {
        table_add(tab, ntry);
        index = table_find(tab, ntry);
    }
    void *res = table_get(tab, index);
    struct table_head *tab3 = res + ((char*)tab2 - (char*)ntry);
    if (tab3->reclen == reclen) return res;
    table_init(tab3, reclen, comparer);
    return res;
}
