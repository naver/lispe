/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//mapbin.h


#ifndef i_mapbin
#define i_mapbin

#include <cstdint>

const uint16_t binBits = 6;
const uint16_t binSize = 1 << binBits;
const uint16_t binMin = 0x3F;
const uint64_t binOne = 1;
 
const uint64_t binVal64[] = { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
    binOne << 16, binOne << 17, binOne << 18, binOne << 19, binOne << 20, binOne << 21, binOne << 22, binOne << 23, binOne << 24,
    binOne << 25, binOne << 26, binOne << 27, binOne << 28,
    binOne << 29, binOne << 30, binOne << 31, binOne << 32, binOne << 33, binOne << 34, binOne << 35, binOne << 36, binOne << 37,
    binOne << 38, binOne << 39, binOne << 40, binOne << 41,
    binOne << 42, binOne << 43, binOne << 44, binOne << 45, binOne << 46, binOne << 47, binOne << 48, binOne << 49, binOne << 50,
    binOne << 51, binOne << 52, binOne << 53, binOne << 54,
binOne << 55, binOne << 56, binOne << 57, binOne << 58, binOne << 59, binOne << 60, binOne << 61, binOne << 62, binOne << 63 };

template <class Z> class binHash;
template <class Z> class binHashe;
template <class Z> class binhash;

template<class S, class Z> class binIter : public std::iterator<std::forward_iterator_tag, Z> {
    public:
    Z** table;
    Z second;
    S first;
    int16_t base;
    uint16_t i, j;

    private:
    uint64_t* indexes;
    uint64_t filter;
    uint64_t mask;
    int16_t inc;
    int16_t tsize;

    public:
    binIter<S, Z>() {
        table = NULL;
    }

    binIter<S, Z>(Z** n, uint64_t* idx, int16_t sz, int16_t b = 0) {
        base = b;
        tsize = sz;
        table = n;
        indexes = idx;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = base << binBits;
        if (n != NULL)
            next();
    }

    binIter<S, Z>(Z** n, uint64_t* idx, int16_t sz, uint16_t ix, uint16_t jx, int16_t b = 0) {
        base = b;
        indexes = idx;
        table = n;
        tsize = sz;
        i = ix;
        j = jx;
        first = (((i + base) << binBits) + j);
        second = table[i][j];
        filter = indexes[i] >> (j + 1);
    }


    binIter<S, Z>& operator=(const binIter<S, Z>& raw) {
        base = raw.base;
        filter = raw.filter;
        table = raw.table;
        indexes = raw.indexes;
        tsize = raw.tsize;
        i = raw.i;
        j = raw.j;
        first = raw.first;
        second = raw.second;
        return (*this);
    }

    binIter<S, Z>(binHash<Z>& p) {
        base = p.base;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = base << binBits;
        if (table != NULL)
            next();
    }
    binIter<S, Z>(binhash<Z>& p) {
        base = 0;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = base << binBits;
        if (table != NULL)
            next();

    }
    binIter<S, Z>(binHashe<Z>& p) {
        base = p.base;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = base << binBits;
        if (table != NULL)
            next();

    }

    void set(binHash<Z>* p) {
        base = p->base;
        tsize = p->tsize;
        table = p->table;
        indexes = p->indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }
    
    void set(binHashe<Z>* p) {
        base = p->base;
        tsize = p->tsize;
        table = p->table;
        indexes = p->indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }
    
    void set(binhash<Z>* p) {
        base = 0;
        tsize = p->tsize;
        table = p->table;
        indexes = p->indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }

    void set(binHash<Z>& p) {
        base = p.base;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }
    
    void set(binHashe<Z>& p) {
        base = p.base;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }
    void set(binhash<Z>& p) {
        base = 0;
        tsize = p.tsize;
        table = p.table;
        indexes = p.indexes;
        i = 0;
        j = 0;
        filter = indexes[i];
        first = 0;
        if (table != NULL)
            next();
    }

    
    binIter& operator++() {
        next();
    }

    binIter<S, Z>* operator->()  {
        return this;
    }

    bool end() {
        return table == NULL;
    }
    
    inline void next() {
        while (!filter && i != tsize) {
            filter = indexes[++i];
            base++;
            j = 0;
        }
        
        if (i == tsize) {
            first = 0;
            table = NULL;
            tsize = -1;
            return;
        }
        
        mask = 0xFFFFFFFF;
        inc = 32;
        
        while (!(filter & 1)) {
            while (!(filter & mask)) {
                filter >>= inc;
                j += inc;
            }
            inc >>= 1;
            mask >>= inc;
        }

        first = (base << binBits) + j;
        second = table[i][j];
        j++;
        filter >>= 1;
    }
    
    inline void next_value() {
        while (i < tsize) {
            if (!j)
                filter = indexes[i];

            if (filter) {
                if (!(filter & 1)) {
                    while (!(filter & 65535)) {
                        filter >>= 16;
                        j += 16;
                    }
                    while (!(filter & 255)) {
                        filter >>= 8;
                        j += 8;
                    }
                    while (!(filter & 15)) {
                        filter >>= 4;
                        j = j + 4;
                    }
                    while (!(filter & 1)) {
                        filter >>= 1;
                        j++;
                    }
                }
                first = (((i + base) << binBits) + j);
                second = table[i][j];
                filter >>= 1;
                if (!filter) {
                    j = 0;
                    i++;
                }
                else
                    j++;
                return;
            }
            j = 0;
            i++;
        }

        first = 0;
        table = NULL;
        tsize = -1;
    }

    binIter<S, Z>& operator++(int) {
        next();
        return *this;
    }

    friend bool operator!=(binIter<S, Z> a, binIter<S, Z> b) {
        return (a.table != b.table);
    }

    friend bool operator==(binIter<S, Z> a, binIter<S, Z> b) {
        return (a.table == b.table);
    }
};

class binSet;

class binSetIter {
public:
    uint64_t* indexes;
    uint64_t filter;
    uint64_t mask;

    int16_t inc;
    int16_t tsize;
    int16_t base;
    
    int16_t first;
    int16_t nb;
    int16_t idx;

    binSetIter() {
        indexes = NULL;
        tsize = 0;
        base = -1;
        filter = 0;
        first = 0;
    }
    
    binSetIter(binSet& b);
    
    void set(binSet& b);
    
    bool next() {
        while (!filter && idx != tsize) {
            filter = indexes[++idx];
            base++;
            nb = base << binBits;
        }
        
        if (idx == tsize)
            return false;
        
        first = nb;
        mask = 0xFFFFFFFF;
        inc = 32;
        
        while (!(filter & 1)) {
            while (!(filter & mask)) {
                filter >>= inc;
                first += inc;
            }
            inc >>= 1;
            mask >>= inc;
        }
        
        nb = first + 1;
        filter >>= 1;
        return true;
    }
};

class binSet {
public:
    uint64_t* indexes;
    uint16_t tsize;
    int16_t base;
    
    bool table;

    binSet()  {
        table = true;
        base = -1;
        tsize = 1;
        indexes = (uint64_t*)malloc(sizeof(uint64_t)*tsize);
        indexes[0] = 0;
    }

    binSet(binSet& t) {
        table = true;
        tsize = t.tsize;
        base = t.base;
        indexes = (uint64_t*)malloc(sizeof(uint64_t)*tsize);

        for (int16_t i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                indexes[i] = t.indexes[i];
            }
            else {
                indexes[i] = 0;
            }
        }
    }

    ~binSet() {
        free(indexes);
    }

    inline bool check(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        return (i < tsize && (indexes[i] & binVal64[r & binMin]));
    }

    inline int16_t checkvalue(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        return (i < tsize && (indexes[i] & binVal64[r & binMin]))?r:-1;
    }

    inline bool checkanderase(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        bool ret = (i < tsize && (indexes[i] & binVal64[r]));
        indexes[i] &= ~binVal64[r];
        return ret;
    }

    bool empty() {
        for (int16_t i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }

    void erase(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i < tsize && indexes[i])
            indexes[i] &= ~binVal64[r];
    }
    
    size_t size() {
        size_t nb = 0;
        uint64_t filter;
        
        for (int16_t i = 0; i < tsize; i++) {
            filter = indexes[i];
            if (filter) {
                while (filter) {
                    if (!(filter & 1)) {
                        while (!(filter & 65535))
                            filter >>= 16;
                        while (!(filter & 255))
                            filter >>= 8;
                        while (!(filter & 15))
                            filter >>= 4;
                        while (!(filter & 1))
                            filter >>= 1;
                    }
                    nb++;
                    filter >>= 1;
                }
            }
        }
        
        return nb;
    }

    //nettoyage
    void clear() {
        memset(indexes, 0, sizeof(uint64_t)*tsize);
    }
    
    void resize(int16_t sz) {
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*sz);
        for (int16_t i = tsize; i < sz; i++) {
            indexes[i] = 0;
        }
        tsize = sz;
    }

    //We insert some new boxes before the position 0
    void insert(uint16_t p) {
        uint16_t inc = base - p;
        uint16_t sz = inc + tsize;
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*sz);

        int16_t i;
        for (i = sz - 1; i >= inc; i--)  {
            indexes[i] = indexes[i - inc];
        }
        
        for (;i >=0; i--) {
            indexes[i] = 0;
        }
        tsize = sz;
        base = p; //this the new zero position
    }

    void push(uint16_t r) {
        if (base == -1) {
            base = r >> binBits;
            r &= binMin;
            indexes[0] |= binVal64[r];
            return;
        }
        
        uint16_t i = r >> binBits;
        r &= binMin;
        if (i < base) {
            insert(i);
            indexes[0] |= binVal64[r];
            return;
        }

        i -= base;
        if (i >= tsize) {
            resize(i + (i >> 1) + 1);
            indexes[i] |= binVal64[r];
            return;
        }
        
        indexes[i] |= binVal64[r];
    }

    void push_back(uint16_t r) {
        if (base == -1) {
            base = r >> binBits;
            r &= binMin;
            indexes[0] |= binVal64[r];
            return;
        }
        
        uint16_t i = r >> binBits;
        r &= binMin;
        if (i < base) {
            insert(i);
            indexes[0] |= binVal64[r];
            return;
        }

        i -= base;
        if (i >= tsize) {
            resize(i + (i >> 1) + 1);
            indexes[i] |= binVal64[r];
            return;
        }
        
        indexes[i] |= binVal64[r];
    }

    bool& operator [](uint16_t r) {
        if (base == -1) {
            base = r >> binBits;
            r &= binMin;
            indexes[0] |= binVal64[r];
            return table;
        }
        
        uint16_t i = r >> binBits;
        r &= binMin;
        if (i < base) {
            insert(i);
            indexes[0] |= binVal64[r];
            return table;
        }

        i -= base;
        if (i >= tsize) {
            resize(i + (i >> 1) + 1);
            indexes[i] |= binVal64[r];
            return table;
        }
        indexes[i] |= binVal64[r];
        return table;
    }


    //affectation
    void operator =(binSet& t) {
        int16_t i;
        for (i = 0; i < tsize; i++) {
            if (indexes[i]) {
                indexes[i] = 0;
            }
        }

        if (tsize != t.tsize) {
            tsize = t.tsize;
            indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*tsize);
        }

        base = t.base;

        for (i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                indexes[i] = t.indexes[i];
            }
            else {
                indexes[i] = 0;
            }
        }
    }
    
    void set(int16_t b, int16_t sz, uint64_t* index) {
        tsize = sz;
        base = b;
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*tsize);
        for (long i = 0; i < sz; i++) {
            if (index[i]) {
                indexes[i] = index[i];
            }
            else {
                indexes[i] = 0;
            }
        }
    }
    
};

template <class Z> class binhash {
public:
    Z** table;
    uint64_t* indexes;
    uint16_t tsize;

    typedef binIter<int16_t, Z> iterator;

    binhash()  {
        tsize = 1;
        table = (Z**)malloc(sizeof(Z*)*tsize);
        indexes = (uint64_t*)malloc(sizeof(uint64_t)*tsize);

        table[0] = NULL;
        indexes[0] = 0;
    }

    ~binhash() {
        for (int16_t i = 0; i < tsize; i++) {
            if (table[i] != NULL)
                delete[] table[i];
        }
        free(table);
        free(indexes);
    }

    inline bool check(uint16_t r) {
        uint16_t i = (r >> binBits);
        return (i < tsize && (indexes[i] & binVal64[r & binMin]));
    }

    inline int16_t checkvalue(uint16_t r) {
        uint16_t i = (r >> binBits);
        return (i < tsize && (indexes[i] & binVal64[r & binMin]))?r:-1;
    }

    inline Z search(uint16_t r) {
        uint16_t i = (r >> binBits);
        r &= binMin;
        return (i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
    }

    void resize(int16_t sz) {
        table = (Z**)realloc(table, sizeof(Z*)*sz);
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*sz);
        for (int16_t i = tsize; i < sz; i++) {
            table[i] = NULL;
            indexes[i] = 0;
        }
        tsize = sz;
    }

    Z& operator [](uint16_t r) {
        uint16_t i = r >> binBits;
        r &= binMin;
        if (i >= tsize) {
            resize(i + (i >> 1) + 1);
            table[i] = new Z[binSize];
            indexes[i] |= binVal64[r];
            return table[i][r];
        }
        if (table[i] == NULL)
            table[i] = new Z[binSize];
        indexes[i] |= binVal64[r];
        return table[i][r];
    }

};

template <class Z> class binHash {
    public:
    Z** table;
    uint64_t* indexes;
    uint16_t tsize;
    int16_t base;

    typedef binIter<int16_t, Z> iterator;
    
    binHash()  {
        base = -1;
        tsize = 1;
        table = (Z**)malloc(sizeof(Z*)*tsize);
        indexes = (uint64_t*)malloc(sizeof(uint64_t)*tsize);

        table[0] = NULL;
        indexes[0] = 0;
    }

    binHash(binHash<Z>& t) {
        static int32_t sz = sizeof(Z) << binBits;
        tsize = t.tsize;
        base = t.base;
        table = (Z**)malloc(sizeof(Z*)*tsize);
        indexes = (uint64_t*)malloc(sizeof(uint64_t)*tsize);

        for (int16_t i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binSize];
                indexes[i] = t.indexes[i];
                memcpy(table[i], t.table[i], sz);
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }

    ~binHash() {
        for (int16_t i = 0; i < tsize; i++) {
            if (table[i] != NULL)
                delete[] table[i];
        }
        free(table);
        free(indexes);
    }

    inline bool check(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        return (i < tsize && (indexes[i] & binVal64[r & binMin]));
    }

    inline int16_t checkvalue(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        return (i < tsize && (indexes[i] & binVal64[r & binMin]))?r:-1;
    }

    void put(uint16_t r, Z a) {
        table[(r >> binBits)-base][r & binMin] = a;
    }
    
    //Only if you are sure that the element exists...
    inline Z&  at(uint16_t r) {
        return table[(r >> binBits)-base][r & binMin];
    }

    inline Z  at(int16_t i, int16_t r) {
        return table[i][r];
    }

    bool empty() {
        for (int16_t i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }

    inline Z search(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        return (i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
    }

    inline bool search(uint16_t r, Z* e) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        *e = (i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
        return *e;
    }

    iterator find(uint16_t r) {
        if (base == -1)
            return iterator();

        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i < tsize && (indexes[i] & binVal64[r]))
            return iterator(table, indexes, tsize, i, r);
        
        return iterator();
    }

    void erase(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i < tsize && indexes[i])
            indexes[i] &= ~binVal64[r];
    }
    
    size_t size() {
        size_t nb = 0;
        uint64_t filter;
        
        for (int16_t i = 0; i < tsize; i++) {
            if (table[i] != NULL) {
                filter = indexes[i];
                while (filter) {
                    if (!(filter & 1)) {
                        while (!(filter & 65535))
                            filter >>= 16;
                        while (!(filter & 255))
                            filter >>= 8;
                        while (!(filter & 15))
                            filter >>= 4;
                        while (!(filter & 1))
                            filter >>= 1;
                    }
                    nb++;
                    filter >>= 1;
                }
            }
        }
        
        return nb;
    }

    //nettoyage
    void clear() {
        if (tsize >= 10) {
            for (int16_t i = 0; i < tsize; i++) {
                if (table[i] != NULL)
                    delete[] table[i];
            }
            base = -1;
            tsize = 1;
            table = (Z**)realloc(table, sizeof(Z*)*tsize);
            indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*tsize);
            table[0] = NULL;
            indexes[0] = 0;
        }
        else {
            memset(indexes, 0, sizeof(uint64_t)*tsize);
        }
    }
    
    void resize(int16_t sz) {
        table = (Z**)realloc(table, sizeof(Z*)*sz);
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*sz);
        for (int16_t i = tsize; i < sz; i++) {
            table[i] = NULL;
            indexes[i] = 0;
        }
        tsize = sz;
    }

    //We insert some new boxes before the position 0
    void insert(uint16_t p) {
        uint16_t inc = base - p;
        uint16_t sz = inc + tsize;
        table = (Z**)realloc(table, sizeof(Z*)*sz);
        indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*sz);

        int16_t i;
        for (i = sz - 1; i >= inc; i--)  {
            table[i] = table[i - inc];
            indexes[i] = indexes[i - inc];
        }
        
        for (;i >=0; i--) {
            table[i] = NULL;
            indexes[i] = 0;
        }
        tsize = sz;
        base = p; //this the new zero position
    }


    void setBase(int16_t i) {
        if (i < base) {
            insert(i);
            table[0] = new Z[binSize];
        }
        else {
            i -= base;
            resize(i + 3);
            table[i] = new Z[binSize];
        }
    }

    Z& operator [](uint16_t r) {
        if (base == -1) {
            base = r >> binBits;
            r &= binMin;
            table[0] = new Z[binSize];
            indexes[0] |= binVal64[r];
            return table[0][r];
        }
        
        uint16_t i = r >> binBits;
        r &= binMin;
        if (i < base) {
            insert(i);
            table[0] = new Z[binSize];
            indexes[0] |= binVal64[r];
            return table[0][r];
        }

        i -= base;
        if (i >= tsize) {
            resize(i + (i >> 1) + 1);
            table[i] = new Z[binSize];
            indexes[i] |= binVal64[r];
            return table[i][r];
        }
        if (table[i] == NULL)
            table[i] = new Z[binSize];
        indexes[i] |= binVal64[r];
        return table[i][r];
    }

    //intersection
    //We only copy elements that are not in t
    void andnot(binHash<Z>& t) {
        int16_t j, t_i;
        uint64_t filter;

        for (int16_t i = 0; i < tsize; i++) {
            filter = indexes[i];
            if (!filter)
                continue;
            
            t_i = i + base - t.base;
            if (t_i >= 0 && t_i < t.tsize) {
                filter &= ~t.indexes[t_i];
                if (!t.table[t_i])
                    t.table[t_i] = new Z[binSize];
                t.indexes[t_i] |= filter;
            }
            else {
                t_i = i + base;
                t.setBase(t_i);
                t_i -= t.base;
                t.indexes[t_i] = filter;
            }
            
            j = 0;
            while (filter) {
                if (!(filter & 1)) {
                    while (!(filter & 65535)) {
                        filter >>= 16;
                        j += 16;
                    }
                    while (!(filter & 255)) {
                        filter >>= 8;
                        j += 8;
                    }
                    while (!(filter & 15)) {
                        filter >>= 4;
                        j += 4;
                    }
                    while (!(filter & 1)) {
                        filter >>= 1;
                        j++;
                    }
                }
                t.table[t_i][j] = table[i][j];
                filter >>= 1;
                j++;
            }
        }
    }
    
    //affectation
    void operator =(binHash<Z>& t) {
        static int32_t sz = sizeof(Z) << binBits;

        int16_t i;
        for (i = 0; i < tsize; i++) {
            if (indexes[i]) {
                delete[] table[i];
                indexes[i] = 0;
                table[i] = NULL;
            }
        }

        if (tsize != t.tsize) {
            tsize = t.tsize;
            table = (Z**)realloc(table, sizeof(Z*)*tsize);
            indexes = (uint64_t*)realloc(indexes, sizeof(uint64_t)*tsize);
        }

        base = t.base;

        for (i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binSize];
                indexes[i] = t.indexes[i];
                memcpy(table[i], t.table[i], sz);
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
};

template <class Z> class binHashe {
public:
    Z** table;
    uint64_t* indexes;
    uint16_t tsize;
    int16_t base;

    typedef binIter<int16_t, Z> iterator;
        
    binHashe()  {
        base = -1;
        tsize = 1;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];
        
        table[0] = NULL;
        indexes[0] = 0;
    }
    
    binHashe(binHashe<Z>& t) {
        tsize = t.tsize;
        base = t.base;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];
        
        for (int16_t i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binSize];
                indexes[i] = t.indexes[i];
                for (int16_t j = 0; j < binSize; j++) {
                    table[i][j] = t.table[i][j];
                }
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
    
    ~binHashe() {
        for (int16_t i = 0; i < tsize; i++) {
            if (table[i] != NULL)
                delete[] table[i];
        }
        delete[] table;
        delete[] indexes;
    }
    
    inline bool check(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        return (i < tsize && (indexes[i] & binVal64[r & binMin]));
    }
    
    bool empty() {
        for (int16_t i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }
    
    inline Z search(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        return (i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
    }
    
    void erase(uint16_t r) {
        uint16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i < tsize && indexes[i])
            indexes[i] &= ~binVal64[r];
    }
    
    
    size_t size() {
        int32_t nb = 0;
        uint64_t filter;
        
        for (int16_t i = 0; i < tsize; i++) {
            if (table[i] != NULL) {
                filter = indexes[i];
                while (filter) {
                    if (!(filter & 1)) {
                        while (!(filter & 65535))
                            filter >>= 16;
                        while (!(filter & 255))
                            filter >>= 8;
                        while (!(filter & 15))
                            filter >>= 4;
                        while (!(filter & 1))
                            filter >>= 1;
                    }
                    nb++;
                    filter >>= 1;
                }
            }
        }
        
        return nb;
    }
    
    //nettoyage
    void clear() {
        if (tsize >= 10) {
            for (int16_t i = 0; i < tsize; i++) {
                if (table[i] != NULL)
                    delete[] table[i];
            }
            base = -1;
            tsize = 1;
            delete[] table;
            delete[] indexes;
            table = new Z*[tsize];
            indexes = new uint64_t[tsize];
            table[0] = NULL;
            indexes[0] = 0;
        }
        else {
            for (int16_t i = 0; i < tsize; i++) {
                indexes[i] = 0;
            }
        }
    }
    
    //We insert some new boxes before the position 0
    void insert(uint16_t p) {
        uint16_t inc = base - p;
        int32_t sz = inc + tsize;
        Z** ntable = new Z*[sz];
        uint64_t* nindexes = new uint64_t[sz];
        
        tsize = sz;
        for (int16_t i = 0; i < tsize; i++)  {
            if (i >= inc) {
                ntable[i] = table[i - inc];
                nindexes[i] = indexes[i - inc];
            }
            else {
                ntable[i] = NULL;
                nindexes[i] = 0;
            }
        }
        
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
        
        base = p; //this the new zero position
    }
    
    inline void resize(int16_t sz) {
        Z** ntable = new Z*[sz];
        uint64_t* nindexes = new uint64_t[sz];
        
        int16_t i;
        for (i = 0; i < tsize; i++) {
            ntable[i] = table[i];
            nindexes[i] = indexes[i];
        }
        
        tsize = sz;
        for (; i < tsize; i++)  {
            ntable[i] = NULL;
            nindexes[i] = 0;
        }
        delete[] table;
        delete[] indexes;
        table = ntable;
        indexes = nindexes;
    }
    
    //Only if you are sure that the element exists...
    inline Z&  at(uint16_t r) {
        return table[(r >> binBits)-base][r & binMin];
    }
    
    Z& operator [](uint16_t r) {
        uint16_t i = r >> binBits;
        r &= binMin;
        if (base == -1) {
            base = i;
            i = 0;
        }
        else {
            if (i < base){
                insert(i);
                i = 0;
            }
            else {
                i -= base;
                if (i >= tsize)
                    resize(i + 2);
            }
        }
        if (table[i] == NULL) {
            table[i] = new Z[binSize];
        }
        indexes[i] |= binVal64[r];
        return table[i][r];
    }
    
    //affectation
    void operator =(binHashe<Z>& t) {
        int16_t i;
        for (i = 0; i < tsize; i++) {
            if (indexes[i]) {
                delete[] table[i];
                indexes[i] = 0;
                table[i] = NULL;
            }
        }

        if (tsize != t.tsize) {
            delete[] table;
            delete[] indexes;
            
            tsize = t.tsize;
            table = new Z*[tsize];
            indexes = new uint64_t[tsize];
        }
        
        base = t.base;
        
        for (i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binSize];
                indexes[i] = t.indexes[i];
                for (int16_t j = 0; j < binSize; j++) {
                    table[i][j] = t.table[i][j];
                }
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
};

#endif
