struct tree_node {
    struct tree_node* zero;
    struct tree_node* one;
    unsigned char* value;
#ifndef HAVE_NOCACHE
    unsigned char* result;
    struct tree_node** cache;
#endif
};

struct tree_value {
    int mask;
    int addr[];
};

struct tree_head {
    int reclen;
    struct tree_node root;
};

int bitVals[] = {
    0x80000000, 0x40000000, 0x20000000, 0x10000000,
    0x8000000, 0x4000000, 0x2000000, 0x1000000,
    0x800000, 0x400000, 0x200000, 0x100000,
    0x80000, 0x40000, 0x20000, 0x10000,
    0x8000, 0x4000, 0x2000, 0x1000,
    0x800, 0x400, 0x200, 0x100,
    0x80, 0x40, 0x20, 0x10,
    0x8, 0x4, 0x2, 0x1
};


void tree_init(struct tree_head *tab, int reclen) {
    tab->reclen = reclen;
    memset(&tab->root, 0, sizeof(tab->root));
}

void tree_deinit(struct tree_head *tab) {
    tab->reclen = 0;
}

#define tree_bit(p) (val->addr[p >> 5] & bitVals[p & 0x1f])

#ifdef HAVE_NOCACHE

void* tree_lpm(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = &tab->root;
    struct tree_value* val = ntry;
    int vlmsk = val->mask;
    void* lst = NULL;
    for (int p = 0;; p++) {
        if (cur->value != NULL) lst = cur->value;
        if (p >= vlmsk) return lst;
        if (tree_bit(p) != 0) {
            cur = cur->one;
        } else {
            cur = cur->zero;
        }
        if (cur == NULL) return lst;
    }
}

void tree_cache(struct tree_node* bas) {
}

#else

void* tree_lpm(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = &tab->root;
    struct tree_value* val = ntry;
    int vlmsk = val->mask;
    void* lst = NULL;
    for (int p = 0;; p += 8) {
        if (cur->result != NULL) lst = cur->result;
        if (cur->cache == NULL) return lst;
        if (p >= vlmsk) return lst;
        int i = val->addr[p >> 5];
        i >>= 24 - (p & 0x1f);
        i &= 0xff;
        cur = cur->cache[i];
        if (cur == NULL) return lst;
    }
}

void tree_cacheNode(struct tree_node** res, struct tree_node* cur, unsigned char* lst, int beg, int end) {
    if (cur == NULL) return;
    if (cur->value != NULL) lst = cur->value;
    int mid = end - beg;
    if (mid < 256) cur->result = lst;
    for (int i = beg; i < end; i++) res[i] = cur;
    if (mid <= 1) return;
    mid >>= 1;
    mid += beg;
    tree_cacheNode(res, cur->zero, lst, beg, mid);
    tree_cacheNode(res, cur->one, lst, mid, end);
}

void tree_cache(struct tree_node* bas) {
    struct tree_node* res[256];
    memset(&res, 0, sizeof(res));
    tree_cacheNode(res, bas, NULL, 0, 256);
    if (bas->cache != NULL) {
        memcpy(bas->cache, &res, sizeof(res));
        return;
    }
    struct tree_node** buf = malloc(sizeof(res));
    if (buf == NULL) err("error allocating memory");
    memcpy(buf, &res, sizeof(res));
    bas->cache = buf;
}

#endif


void tree_add(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = &tab->root;
    struct tree_node* bas = &tab->root;
    struct tree_value* val = ntry;
    int vlmsk = val->mask;
    for (int p = 0;; p++) {
        if (p >= vlmsk) {
            if (cur->value != NULL) {
                memcpy(cur->value, ntry, tab->reclen);
                tree_cache(bas);
                return;
            }
            void* nxt = malloc(tab->reclen);
            if (nxt == NULL) err("error allocating memory");
            memcpy(nxt, ntry, tab->reclen);
            cur->value = nxt;
            tree_cache(bas);
            return;
        }
        if ((p & 7) == 0) {
            tree_cache(bas);
            bas = cur;
        }
        if (cur->zero == NULL) {
            struct tree_node* nxt = malloc(sizeof(struct tree_node));
            if (nxt == NULL) err("error allocating memory");
            memset(nxt, 0, sizeof(struct tree_node));
            cur->zero = nxt;
        }
        if (cur->one == NULL) {
            struct tree_node* nxt = malloc(sizeof(struct tree_node));
            if (nxt == NULL) err("error allocating memory");
            memset(nxt, 0, sizeof(struct tree_node));
            cur->one = nxt;
        }
        if (tree_bit(p) != 0) {
            cur = cur->one;
        } else {
            cur = cur->zero;
        }
    }
}

void tree_del(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = &tab->root;
    struct tree_node* bas = &tab->root;
    struct tree_value* val = ntry;
    int vlmsk = val->mask;
    for (int p = 0;; p++) {
        if (p >= vlmsk) {
            void* old = cur->value;
            if (old == NULL) return;
            cur->value = NULL;
            tree_cache(bas);
            free(old);
            return;
        }
        if ((p & 7) == 0) {
            bas = cur;
        }
        if (tree_bit(p) != 0) {
            cur = cur->one;
        } else {
            cur = cur->zero;
        }
        if (cur == NULL) return;
    }
}

void tree_walkNode(struct tree_node *cur, void doer(void *, int), int fixed) {
    if (cur == NULL) return;
    tree_walkNode(cur->zero, doer, fixed);
    tree_walkNode(cur->one, doer, fixed);
    if (cur->value == NULL) return;
    doer(cur->value, fixed);
}

void tree_walk(struct tree_head *tab, void doer(void *, int), int fixed) {
    struct tree_node* cur = &tab->root;
    tree_walkNode(cur, doer, fixed);
}
