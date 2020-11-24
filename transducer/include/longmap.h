/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//longmap.h


#ifndef i_longmap
#define i_longmap

#include <math.h>
#include "mathtypes.h"

const unsigned long binlongbits = 6;
const unsigned long binlongsize = 1 << binlongbits;
const uint64_t binlongONE = 1;

const uint64_t binlongval64[] = { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
    binlongONE << 16, binlongONE << 17, binlongONE << 18, binlongONE << 19, binlongONE << 20, binlongONE << 21, binlongONE << 22, binlongONE << 23, binlongONE << 24,
    binlongONE << 25, binlongONE << 26, binlongONE << 27, binlongONE << 28,
    binlongONE << 29, binlongONE << 30, binlongONE << 31, binlongONE << 32, binlongONE << 33, binlongONE << 34, binlongONE << 35, binlongONE << 36, binlongONE << 37,
    binlongONE << 38, binlongONE << 39, binlongONE << 40, binlongONE << 41,
    binlongONE << 42, binlongONE << 43, binlongONE << 44, binlongONE << 45, binlongONE << 46, binlongONE << 47, binlongONE << 48, binlongONE << 49, binlongONE << 50,
    binlongONE << 51, binlongONE << 52, binlongONE << 53, binlongONE << 54,
    binlongONE << 55, binlongONE << 56, binlongONE << 57, binlongONE << 58, binlongONE << 59, binlongONE << 60, binlongONE << 61, binlongONE << 62, binlongONE << 63 };

template <class Z> class binlong_hash {
public:
    long tsize;
    Z** table;
    uint64_t* indexes;
    bool initialization;
    
    binlong_hash(bool init = true, long sz = 4)  {
        initialization = init;
        tsize = sz;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];
        
        for (long i=0; i<tsize;i++) {
            table[i] = NULL;
            indexes[i] = 0;
        }
    }
    
    binlong_hash(binlong_hash<Z>& t) {
        tsize = t.tsize;
        table = new Z*[tsize];
        indexes = new uint64_t[tsize];
        initialization = t.initialization;
        
        for (long i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binlongsize];
                indexes[i] = t.indexes[i];
                for (uint16_t j = 0; j < binlongsize; j++)
                    table[j] = t.table[j];
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
    
    ~binlong_hash() {
        for (long i = 0; i < tsize; i++) {
            if (table[i] != NULL)
                delete[] table[i];
        }
        delete[] table;
        delete[] indexes;
    }
    
    Z&  get(unsigned long r) {
        unsigned long i = r >> binlongbits;
        r = r - (i << binlongbits);
        return table[i][r];
    }
    
    bool get(unsigned long p, long& i, long& r) {
        i = p >> binlongbits;
        if (i >= tsize || !indexes[i])
            return false;
        r = p - (i << binlongbits);
        if (!(indexes[i] & binlongval64[r]))
            return false;
        
        return true;
    }
    
    bool check(unsigned long r) {
        unsigned long i = r >> binlongbits;
        if (i >= tsize || !indexes[i])
            return false;
        r = r - (i << binlongbits);
        if (!(indexes[i] & binlongval64[r]))
            return false;
        
        return true;
    }
    
    Z search(unsigned long r) {
        unsigned long i = r >> binlongbits;
        if (i >= tsize || !indexes[i])
            return NULL;
        
        r = r - (i << binlongbits);
        return table[i][r];
    }
    
    //nettoyage
    void clear() {
        for (long i = 0; i < tsize; i++) {
            if (table[i] != NULL) {
                delete[] table[i];
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
    
    bool empty() {
        for (long i = 0; i < tsize; i++) {
            if (indexes[i])
                return false;
        }
        return true;
    }
    
    void erase(unsigned long r) {
        unsigned long i = r >> binlongbits;
        if (table[i] == NULL)
            return;
        
        r = r - (i << binlongbits);
        indexes[i] &= ~binlongval64[r];
        table[i][r] = NULL;
    }
    
#ifdef INTELINTRINSICS
    size_t size() {
        uint16_t nb = 0;
        
        for (long i = 0; i < tsize; i++) {
            nb += bitcounter(indexes[i]);
        }
        return nb;
    }
#else
    size_t size() {
        uint16_t nb = 0;
        uint64_t filter;
        
        for (long i = 0; i < tsize; i++) {
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
#endif

    
    void resize(long sz) {        
        Z** ntable = new Z*[sz];
        uint64_t* nindexes = new uint64_t[sz];
        
        long i;
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
    
    Z& operator [](unsigned long r) {
        unsigned long i = r >> binlongbits;
        r = r - (i << binlongbits);
        if (i >= tsize)
            resize(i + (i>>2));
        if (table[i] == NULL) {
            table[i] = new Z[binlongsize];
            if (initialization)
                memset(table[i], NULL, sizeof(Z) << binlongbits);
        }
        indexes[i] |= binlongval64[r];
        //indexes[i] |= (1 << r);
        return table[i][r];
    }
    
    void put(unsigned long r, Z v) {
        unsigned long i = r >> binlongbits;
        r = r - (i << binlongbits);
        if (i >= tsize)
            resize(i + 2);
        if (table[i] == NULL) {
            table[i] = new Z[binlongsize];
            if (initialization)
                memset(table[i], NULL, sizeof(Z) << binlongbits);
        }
        indexes[i] |= binlongval64[r];
        //indexes[i] |= (1 << r);
        table[i][r] = v;
    }
    
    //affectation
    void operator =(binlong_hash<Z>& t) {
        long i;
        
        for (i = 0; i < tsize; i++) {
            if (indexes[i])
                delete[] table[i];
        }
        
        if (tsize != t.tsize) {
            delete[] table;
            delete[] indexes;
            
            tsize = t.tsize;
            table = new Z*[tsize];
            indexes = new uint64_t[tsize];
        }
        
        initialization = t.initialization;
        
        for (i = 0; i < t.tsize; i++) {
            if (t.indexes[i]) {
                table[i] = new Z[binlongsize];
                indexes[i] = t.indexes[i];
                for (uint16_t j = 0; j < binlongsize; j++)
                    table[j] = t.table[j];
            }
            else {
                table[i] = NULL;
                indexes[i] = 0;
            }
        }
    }
};

#endif
