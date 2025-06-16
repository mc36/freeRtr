struct table_head {
    int reclen;
    int cmplen;
    unsigned char *buffer;
    int size;
    int alloc;
};


struct hasht_head {
#ifdef HAVE_NOHASH
    struct table_head dat;
#else
    struct table_head dat[256];
#endif
};


#define table_get(tab, idx)   (void*) ((tab)->buffer + ((idx) * (tab)->reclen))

#define table_nonexist(tab)   ((tab)->buffer == NULL)



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
    if ((ned + 512) < tab->alloc) siz = ned + 256;
    if (ned > tab->alloc) siz = ned + 256;
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


void* table_add(struct table_head *tab, void *ntry) {
    int idx = table_find(tab, ntry);
    if (idx >= 0) {
        void* res=table_get(tab, idx);
        memmove(res, ntry, tab->reclen);
        return res;
    }
    table_resize(tab, tab->size + 1);
    idx = -idx - 1;
    void* res=table_get(tab, idx);
    memmove(res + tab->reclen, res, (size_t)tab->reclen * (tab->size - idx));
    tab->size++;
    memmove(table_get(tab, idx), ntry, tab->reclen);
    return res;
}


void table_del(struct table_head *tab, void *ntry) {
    int idx = table_find(tab, ntry);
    if (idx < 0) return;
    void* res=table_get(tab, idx);
    memmove(res, res + tab->reclen, (size_t)tab->reclen * (tab->size - idx - 1));
    tab->size--;
    table_resize(tab, tab->size);
}


void table_walk(struct table_head *tab, void doer(void *, int), int fixed) {
    for (int i=0; i<tab->size; i++) doer(table_get(tab, i), fixed);
}


#ifdef HAVE_NOHASH
void hasht_init(struct hasht_head *tab, int reclen, int cmplen) {
    table_init(&tab->dat, reclen, cmplen);
}


void hasht_deinit(struct hasht_head *tab) {
    table_deinit(&tab->dat);
}


#define hasht_nonexist(tab) table_nonexist(&((tab)->dat))


void* hasht_add(struct hasht_head *tab, void *ntry) {
    return table_add(&tab->dat, ntry);
}


void hasht_del(struct hasht_head *tab, void *ntry) {
    table_del(&tab->dat, ntry);
}


void* hasht_find(struct hasht_head *tab, void *ntry) {
    int idx = table_find(&tab->dat, ntry);
    if (idx < 0) return NULL;
    return table_get(&tab->dat, idx);
}


void hasht_walk(struct hasht_head *tab, void doer(void *, int), int fixed) {
    table_walk(&tab->dat, doer, fixed);
}

#else

void hasht_init(struct hasht_head *tab, int reclen, int cmplen) {
    for (int i=0; i<256; i++) table_init(&tab->dat[i], reclen, cmplen);
}


void hasht_deinit(struct hasht_head *tab) {
    for (int i=0; i<256; i++) table_deinit(&tab->dat[i]);
}


#define hasht_nonexist(tab) table_nonexist(&((tab)->dat[0]))


struct table_head* hash_bucket(struct hasht_head *tab, void *ntry) {
    unsigned int*entry = (unsigned int*)ntry;
    int len = tab->dat[0].cmplen;
    int hash = 0;
    for (int i=0; i<len; i++) hash ^= entry[i];
    hash = ((hash >> 16) ^ hash) & 0xffff;
    hash = ((hash >> 8) ^ hash) & 0xff;
    return &tab->dat[hash];
}


void* hasht_add(struct hasht_head *tab, void *ntry) {
    struct table_head* hash = hash_bucket(tab, ntry);
    return table_add(hash, ntry);
}


void hasht_del(struct hasht_head *tab, void *ntry) {
    struct table_head* hash = hash_bucket(tab, ntry);
    table_del(hash, ntry);
}


void* hasht_find(struct hasht_head *tab, void *ntry) {
    struct table_head* hash = hash_bucket(tab, ntry);
    int idx = table_find(hash, ntry);
    if (idx < 0) return NULL;
    return table_get(hash, idx);
}


void hasht_walk(struct hasht_head *tab, void doer(void *, int), int fixed) {
    for (int i=0; i<256; i++) table_walk(&tab->dat[i], doer, fixed);
}

#endif

void* hasht_addinited(struct hasht_head *tab, void *ntry, struct table_head *tab2, int reclen, int cmplen) {
    void *res = hasht_find(tab, ntry);
    if (res != NULL) return res;
    table_init(tab2, reclen, cmplen);
    res = hasht_add(tab, ntry);
    return res;
}
