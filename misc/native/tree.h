struct tree_node {
    struct tree_node* zero;
    struct tree_node* one;
    unsigned char* value;
};

struct tree_head {
    int reclen;
    struct tree_node* root;
    int (*masker) (void *);
    int (*bitter) (void *, int);
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


void tree_init(struct tree_head *tab, int reclen, int masker(void*), int bitter(void*, int)) {
    tab->reclen = reclen;
    tab->masker = masker;
    tab->bitter = bitter;
    tab->root = malloc(sizeof(struct tree_node));
    if (tab->root == NULL) err("error allocating memory");
    memset(tab->root, 0, sizeof(struct tree_node));
}

void tree_deinit(struct tree_head *tab) {
    tab->reclen = 0;
    tab->bitter = NULL;
    tab->masker = NULL;
    free(tab->root);
    tab->root = NULL;
}

void* tree_lpm(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = tab->root;
    void* lst = NULL;
    int msk = tab->masker(ntry);
    for (int p = 0;; p++) {
        if (cur->value != NULL) lst = cur->value;
        if (p >= msk) return lst;
        if (tab->bitter(ntry, p) != 0) {
            if (cur->one == NULL) return lst;
            cur = cur->one;
        } else {
            if (cur->zero == NULL) return lst;
            cur = cur->zero;
        }
    }
}

void* tree_add(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = tab->root;
    int msk = tab->masker(ntry);
    for (int p = 0;; p++) {
        if (p >= msk) {
            void* old = cur->value;
            if (old == NULL) {
                cur->value = malloc(tab->reclen);
                if (cur->value == NULL) err("error allocating memory");
            }
            memcpy(cur->value, ntry, tab->reclen);
            return old;
        }
        if (tab->bitter(ntry, p) != 0) {
            if (cur->one == NULL) {
                cur->one = malloc(sizeof(struct tree_node));
                if (cur->one == NULL) err("error allocating memory");
                memset(cur->one, 0, sizeof(struct tree_node));
            }
            cur = cur->one;
        } else {
            if (cur->zero == NULL) {
                cur->zero = malloc(sizeof(struct tree_node));
                if (cur->zero == NULL) err("error allocating memory");
                memset(cur->zero, 0, sizeof(struct tree_node));
            }
            cur = cur->zero;
        }
    }
}

void* tree_del(struct tree_head *tab, void *ntry) {
    struct tree_node* cur = tab->root;
    int msk = tab->masker(ntry);
    for (int p = 0;; p++) {
        if (p >= msk) {
            void* old = cur->value;
            if (old == NULL) return NULL;
            cur->value = NULL;
            free(old);
            return old;
        }
        if (tab->bitter(ntry, p) != 0) {
            if (cur->one == NULL) return NULL;
            cur = cur->one;
        } else {
            if (cur->zero == NULL) return NULL;
            cur = cur->zero;
        }
    }
}

void tree_walkNode(struct tree_node *cur, void doer(void *, int, void *), int fixed, void* param) {
    if (cur == NULL) return;
    tree_walkNode(cur->zero, doer, fixed, param);
    tree_walkNode(cur->one, doer, fixed, param);
    if (cur->value == NULL) return;
    doer(cur->value, fixed, param);
}

void tree_walk(struct tree_head *tab, void doer(void *, int, void *), int fixed, void* param) {
    struct tree_node* cur = tab->root;
    tree_walkNode(cur, doer, fixed, param);
}
