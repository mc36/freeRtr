struct table_head {
    int reclen;
    int cmplen;
    unsigned char *buffer;
    int size;
    int alloc;
};


#define table_get(tab, idx)   (void*) ((tab)->buffer + ((idx) * (tab)->reclen))

#define table_empty(tab)      ((tab)->size == 0)


void table_init(struct table_head *tab, int reclen, int cmplen) {
    tab->reclen = reclen;
    tab->cmplen = cmplen;
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
    unsigned char *new = malloc((size_t)tab->reclen * siz);
    if (new == NULL) err("error allocating memory");
    memcpy(new, old, (size_t)tab->reclen * tab->size);
    tab->buffer = new;
    tab->alloc = siz;
    usleep(10000);
    free(old);
}


void table_deinit(struct table_head *tab) {
    tab->reclen = 0;
    tab->cmplen = 0;
    tab->size = 0;
    free(tab->buffer);
    tab->buffer = NULL;
}


int table_find(struct table_head *tab, void *ntry) {
    int lower = 0;
    int upper = tab->size - 1;
    unsigned int*entry = (unsigned int*)ntry;
    int cmpln = tab->cmplen;
    while (lower <= upper) {
        int mid = (lower + upper) >> 1;
        unsigned int*curr = (unsigned int*)table_get(tab, mid);
        long cmp;
        for (int i = 0;;) {
#if UINT_MAX >= ULONG_MAX
            int val1 = curr[i];
            int val2 = entry[i];
            cmp = (val1 > val2) - (val1 < val2);
#else
            cmp = (long)curr[i] - (long)entry[i];
#endif
            if (cmp != 0) break;
            i++;
            if (i >= cmpln) return mid;
        }
        if (cmp < 0) {
            lower = mid + 1;
        } else {
            upper = mid - 1;
        }
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
    memmove(table_get(tab, idx + 1), table_get(tab, idx), (size_t)tab->reclen * (tab->size - idx));
    tab->size++;
    memmove(table_get(tab, idx), ntry, tab->reclen);
}


void table_del(struct table_head *tab, void *ntry) {
    int idx = table_find(tab, ntry);
    if (idx < 0) return;
    if (idx < (tab->size - 1)) memmove(table_get(tab, idx), table_get(tab, idx + 1), (size_t)tab->reclen * (tab->size - idx - 1));
    tab->size--;
    table_resize(tab, tab->size);
}


void* table_addinited(struct table_head *tab, void *ntry, struct table_head *tab2, int reclen, int cmplen) {
    int index = table_find(tab, ntry);
    if (index < 0) {
        table_add(tab, ntry);
        index = table_find(tab, ntry);
    }
    void *res = table_get(tab, index);
    struct table_head *tab3 = res + ((char*)tab2 - (char*)ntry);
    if (tab3->reclen == reclen) return res;
    table_init(tab3, reclen, cmplen);
    return res;
}
