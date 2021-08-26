/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//mapbin.h


#ifndef i_mapbin
#define i_mapbin


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
    long tsize;

    public:
    binIter<S, Z>() {
        table = NULL;
    }

    binIter<S, Z>(Z** n, uint64_t* idx, long sz, short b = 0) {
        first = 0;
        base = b;
        tsize = sz;
        table = n;
        indexes = idx;
        i = 0;
        j = 0;
        filter = 0;
        if (n != NULL)
            next();
    }

    binIter<S, Z>(Z** n, uint64_t* idx, long sz, uint16_t ix, uint16_t jx, short b = 0) {
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


    binIter& operator++() {
        next();
    }

    binIter<S, Z>* operator->()  {
        return this;
    }

    inline void next() {
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
        if (a.table == b.table)
            return false;
        return true;
    }

    friend bool operator==(binIter<S, Z> a, binIter<S, Z> b) {
        if (a.table == b.table)
            return true;
        return false;
    }
};

template <class Z> class binHash {
    public:
    Z** table;
    uint64_t* indexes;
    long tsize;
    int16_t base;

    typedef binIter<short, Z> iterator;

    iterator begin(){ return iterator(table, indexes, tsize, base); }
    iterator end() {
        return iterator();
    }

    binHash()  {
        base = -1;
        tsize = 1;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];

        table[0] = NULL;
        indexes[0] = 0;
    }

    binHash(binHash<Z>& t) {
        static int32_t sz = sizeof(Z) << binBits;
        tsize = t.tsize;
        base = t.base;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];

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
        delete[] table;
        delete[] indexes;
    }

    inline bool check(uint16_t r) {
        int16_t i = (r >> binBits) - base;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r & binMin]));
    }

    void put(uint16_t r, Z a) {
        int16_t i = r >> binBits;
        table[i-base][r & binMin] = a;
    }
    
    //Only if you are sure that the element exists...
    inline Z&  at(uint16_t r) {
        return table[(r >> binBits)-base][r & binMin];
    }
    
    inline bool check(short i, short r) {
        i -= base;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]));
    }

    bool empty() {
        for (int16_t i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }

    inline Z search(uint16_t r) {
        int16_t i = (r >> binBits) - base;
        r &= binMin;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
    }


    iterator find(uint16_t r) {
        if (base == -1)
            return iterator();

        int16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i >= 0 && i < tsize && (indexes[i] & binVal64[r]))
            return iterator(table, indexes, tsize, i, r);
        
        return iterator();
    }

    bool get(uint16_t p, short& i, uint16_t& r) {
        i = (p >> binBits) - base;
        r = p & binMin;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]));
    }

    void erase(uint16_t r) {
        int16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i >= 0 && i < tsize && indexes[i])
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

    void resize(long sz) {
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
            else
                i -= base;
        }
        if (i >= tsize)
            resize(i + 2);
        if (table[i] == NULL) {
            table[i] = new Z[binSize];
            memset(table[i], NULL, sizeof(Z) << binBits);
        }
        indexes[i] |= binVal64[r];
        return table[i][r];
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
    long tsize;
    int16_t base;

    typedef binIter<short, Z> iterator;
    
    iterator begin(){ return iterator(table, indexes, tsize, base); }
    iterator end() {
        return iterator();
    }
    
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
        int16_t i = (r >> binBits) - base;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r & binMin]));
    }
    
    inline bool check(short i, short r) {
        i -= base;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]));
    }

    bool empty() {
        for (int16_t i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }
    
    inline Z search(uint16_t r) {
        int16_t i = (r >> binBits) - base;
        r &= binMin;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]))?table[i][r]:NULL;
    }
    
    iterator find(uint16_t r) {
        if (base == -1)
            return iterator();
        
        int16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i >= 0 && i < tsize && (indexes[i] & binVal64[r]))
            return iterator(table, indexes, tsize, i, r);
        
        return iterator();
    }
    
    bool get(uint16_t p, short& i, uint16_t& r) {
        i = (p >> binBits) - base;
        r = p & binMin;
        return (i >= 0 && i < tsize && (indexes[i] & binVal64[r]));
    }
    
    void erase(uint16_t r) {
        int16_t i = (r >> binBits) - base;
        r &= binMin;
        if (i >= 0 && i < tsize && indexes[i])
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
    
    void resize(long sz) {
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
            else
                i -= base;
        }
        if (i >= tsize)
            resize(i + 2);
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
