/*
 *  LispE
 *
 * Copyright 2020-present NAVER Corp.
 * The 3-Clause BSD License
 */
//  vecte.h
//
//

#ifndef vecte_h
#define vecte_h

#include <cstdint>

template <class Z> class vecte {
public:

	Z* vecteur;
	//sz is the vector size
	long sz;
	//last element appended... 
	long last;

	vecte(long t = 3) {
        last = 0;
		sz = t + 1;
        vecteur = (Z*)malloc(sizeof(Z)*sz);
        memset(vecteur, NULL, sizeof(Z)*sz);
	}

    vecte(long nb, Z val) {
        sz = nb;
        vecteur = (Z*)malloc(sizeof(Z)*sz);
        for (last = 0; last < nb; last++)
            vecteur[last] = val;
    }
    
    vecte(vecte<Z>& v) {
        sz = v.sz;
        last = v.last;
        vecteur = (Z*)malloc(sizeof(Z)*sz);
        memcpy(vecteur, v.vecteur, sizeof(Z)*sz);
    }
    
	~vecte() {
        if (sz)
            free(vecteur);
	}

    
    inline double get_(long pos) {
        return vecteur[pos];
    }
    
    inline void set_(long pos, double v) {
        vecteur[pos] = v;
    }

    inline void setlast(Z val) {
        vecteur[last-1] = val;
    }

	inline long size() {
		return last;
	}

    inline void wipe() {
        while (last>0) {
            if (vecteur[--last] != NULL) {
                delete vecteur[last];
                vecteur[last] = NULL;
            }
        }
    }

    inline void cleaning() {
        while (last>0) {
            delete vecteur[--last];
        }
    }
    
    inline void clean() {
        for (last = 0; last < sz; last++) {
            if (vecteur[last] != NULL) {
                delete vecteur[last];
                vecteur[last] = NULL;
            }
		}
		last = 0;
	}

	void clear() {
		last = 0;
	}

	inline void reserve(long t) {
        if (t > sz) {
            sz = t;
            //We reallocate our vecteur
            vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
            memset(vecteur+last, NULL, sizeof(Z)*(sz-last));
        }
	}
    
    inline void trim() {
        if (!last) {
            free(vecteur);
            vecteur = NULL;
            sz = 0;
            return;
        }
        if (last == sz)
            return;
        
        Z* trimmed = (Z*)malloc(sizeof(Z)*last);
        memcpy(trimmed, vecteur, sizeof(Z)*last);
        free(vecteur);
        vecteur = trimmed;
        sz = last;
    }
    
    inline void setsize(long sze) {
        sz = sze;
        vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
        memset(vecteur+last, NULL, sizeof(Z)*sz);
    }
    
    inline void resize(long i) {
        if (i >= sz) {
            sz = i << 1;
            //We reallocate our vecteur
            vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
            memset(vecteur+last, NULL, sizeof(Z)*(sz-last));
        }
    }

    
	inline Z remove(long pos = -1) {
		Z v;
		if (pos < 0) {
			if (last == 0)
				return NULL;
			last--;
			v = vecteur[last];
			vecteur[last] = NULL;
			return v;
		}

		if (pos >= last)
			return NULL;

		v = vecteur[pos];
		//We move all the boxes by 1...
		last--;
		for (; pos < last; pos++)
			vecteur[pos] = vecteur[pos + 1];
		vecteur[last] = NULL;
		return v;
	}

    inline void swap(long i, long j) {
        Z e = vecteur[i];
        vecteur[i] = vecteur[j];
        vecteur[j] = e;
    }

    inline void remove_sub_back() {
        last--;
        vecteur[last - 1] = vecteur[last];
    }

    inline Z backpop() {
        return vecteur[--last];
    }

	inline void pop_back() {
		last--;
	}

	inline void insert(long pos, Z val) {
        resize(last);

        //In this case, this is a simple push
		if (pos >= last) {
			vecteur[last++] = val;
			return;
		}

        // the element is then added in its place
        // if the box is empty we place it at this place
        //If not, all elements are moved to the right.
        //sinon, on deplace tous les elements d'une case vers la droite
        for (long i = last - 1; i >= pos; i--)
            vecteur[i + 1] = vecteur[i];
        vecteur[pos] = val;
        last++;
	}

	inline Z back() {
		return vecteur[last - 1];
	}

    inline Z preback() {
        return vecteur[last - 2];
    }

    inline void push_max(int16_t mx, Z val) {
        if (last == mx)
            delete val;
        else
            vecteur[last++] = val;
    }
    
	inline void push_back(Z val) {
        resize(last);
		//sinon on ajoute l'element en queue...
		vecteur[last++] = val;
	}
    
    //On ajoute sans vérifier la taille
    inline void push_raw(Z val) {
        vecteur[last++] = val;
    }

    void padding(long nb) {
        if (sz < (nb + last)) {
            sz = (nb + last) << 1;
            //We reallocate our vecteur
            vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
            memset(vecteur+last, NULL, sizeof(Z)*(sz-last));
        }
    }

    void padding(long nb, Z v) {
        if (sz < (nb + last)) {
            sz = (nb + last) << 1;
            //We reallocate our vecteur
            vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
            for (nb = last; nb < sz; nb++)
                vecteur[nb] = v;
        }
    }

	inline Z operator [](long pos) {
		return vecteur[pos];
	}
    
    void operator =(vecte<Z>& t) {
        if (sz < t.sz) {
            sz = t.sz;
            vecteur = (Z*)realloc(vecteur, sizeof(Z)*sz);
        }
        last = t.last;
        memcpy(vecteur, t.vecteur, sizeof(Z)*last);
    }
    
    inline bool empty() {
        return !last;
    }
    
    inline Z get(long pos) {
        return (pos >= last)?NULL:vecteur[pos];
    }
    
    void reverse() {
        Z e;
        long stop = last >> 1;
        for (long i = 0; i < stop; i++) {
            e = vecteur[i];
            vecteur[i] = vecteur[last-i];
            vecteur[last-i] = e;
        }
    }
    
	void erase(long i) {
        if (i == last) {
            vecteur[last--] = NULL;
            return;
        }
        
        if (i >= 0 && i < last) {
            if (last == sz) {
                last--;
                while (i < last) {
                    vecteur[i] = vecteur[i+1];
                    i++;
                }
                vecteur[last] = NULL;
                return;
            }
            
            while (i < last) {
                vecteur[i] = vecteur[i+1];
                i++;
            }
            last--;
        }
	}

	inline Z removeElement(long i = -1) {
		if (!last)
			return NULL;

		long pos = i;

		if (i == -1)
			pos = last - 1;

		Z v = vecteur[pos];
		vecteur[pos] = NULL;

		//On deplace toutes les cases de 1...

		if (i != -1) {
			for (long k = i; k<last - 1; k++)
				vecteur[k] = vecteur[k + 1];
			if (last>0)
				vecteur[last - 1] = NULL;
		}

		last--;
		return v;
	}

    inline void atlast(Z val) {
        vecteur[last-1] = val;
    }
    
    inline void beforelast(Z val) {
        if (last) {
            reserve(last);
            vecteur[last] = vecteur[last-1];
            vecteur[last-1] = val;
            last++;
        }
        else
            vecteur[last++] = val;
    }
    
	inline void at(long pos, Z val) {
        reserve(pos + 1);
		vecteur[pos] = val;
	}

	inline long search(Z v) {
        long i = 0;
        for (; i< last && v != vecteur[i]; i++) {}
        return (i == last)?-1:i;
	}

    inline bool check(Z v) {
        long i = 0;
        for (; i< last && v != vecteur[i]; i++) {}
        return (i != last);
    }
    
    inline bool checkanderase(Z v) {
        long i = 0;
        for (; i< last && v != vecteur[i]; i++) {}
        
        if (i != last) {
            last--;
            while (i < last) {
                vecteur[i] = vecteur[i + 1];
                i++;
            }
            return true;
        }
        return false;
    }
    
    inline bool operator ==(vecte<Z>& v) {
        if (last != v.last)
            return false;

        long i = 0;
        for (; i< last && v.vecteur[i] == vecteur[i]; i++) {}
        return (i == last);
    }

    inline void to_vector(std::vector<Z>& v) {
        for (long i = 0; i < last; i++)
            v.push_back(vecteur[i]);
    }
    
};


//--------------------------------------------------------------------------------
//We use the alloc method in this case...
template <class Z> class item_a {
public:
    Z* buffer;
    long last;
    long sz;
    long status;

    item_a(long t) {
        status = 0; //this is the reference counter
        last = 0; //this is the last element
        sz = t; //this is the size
        //We always create one more element
        //to handle some room for exchange
        buffer = (Z*)malloc(sizeof(Z)*(sz + 1));
        //hence buffer[sz] does exist even though
        //it cannot be accessed through normal means
    }
    
    inline void reserve(long t) {
        if (t > sz) {
            sz = t;
            //We reallocate our vecteur
            buffer = (Z*)realloc(buffer, sizeof(Z)*(sz + 1));
        }
    }

    inline void resize(long t) {
        if (t >= sz) {
            sz = t << 1;
            //We reallocate our vecteur
            buffer = (Z*)realloc(buffer, sizeof(Z)*(sz + 1));
        }
    }
    
    inline void swap(long left, long right) {
        //We use the fact that the last element exists
        //but cannot be accessed...
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
    }

    inline bool reverse(long left, long right) {
        if (left >= right)
            return false;
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
        return true;
    }
    
    inline void erase(long i) {
        if (i >= 0 && i < last) {
            last--;
            for (;i < last; i++)
                buffer[i] = buffer[i+1];
        }
    }

    inline void at(long pos, Z val) {
        resize(pos + 1);
        buffer[pos] = val;
    }

    inline void beforelast(Z val) {
        if (last) {
            resize(last);
            buffer[last] = buffer[last-1];
            buffer[last-1] = val;
            last++;
        }
        else
            buffer[last++] = val;
    }

    inline void padding(long nb) {
        if (sz < (nb + last)) {
            sz = (nb + last) << 1;
            //We reallocate our vecteur
            buffer = (Z*)realloc(buffer, sizeof(Z)*sz + 1);
            memset(buffer+last, NULL, sizeof(Z)*(sz-last));
        }
    }

    inline void padding(long nb, Z v) {
        if (sz < (nb + last)) {
            sz = (nb + last) << 1;
            //We reallocate our vecteur
            buffer = (Z*)realloc(buffer, sizeof(Z)*sz + 1);
            for (nb = last; nb < sz; nb++)
                buffer[nb] = v;
        }
    }

    inline void atlast(Z val) {
        buffer[last-1] = val;
    }

    inline void insert(long pos, Z val) {
        resize(last);

        if (pos >= last) {
            buffer[last++] = val;
            return;
        }

        //All elements are moved to the right.
        for (long i = last; i > pos; i--)
            buffer[i] = buffer[i - 1];
        buffer[pos] = val;
        last++;
    }

    inline void extend(item_a* val, long val_home) {
        resize(last+val->last);
        for (long i = val_home; i < val->last; i++) {
            buffer[last++] = val->buffer[i];
        }
    }

    inline void push_back(Z val) {
        resize(last);
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }

    inline void push_raw(Z val) {
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }

    inline long counting(long home, Z v) {
        long count = 0;
        for (long i = home; i < last; i++)
            count += (buffer[i] == v);
        return count;
    }

    inline void plus(long home, long home_n, Z* n,  long nb) {
        home--;
        home_n--;
        while (nb > 0) {
            buffer[home+nb] += n[home_n + nb];
            nb--;
        }
    }

    inline void minus(long home, long home_n, Z* n,  long nb) {
        home--;
        home_n--;
        while (nb > 0) {
            buffer[home+nb] -= n[home_n + nb];
            nb--;
        }
    }

    inline void multiply(long home, long home_n, Z* n,  long nb) {
        home--;
        home_n--;
        while (nb > 0) {
            buffer[home+nb] *= n[home_n + nb];
            nb--;
        }
    }

    inline void plus(long home, Z v) {
        for (long i = home; i < last; i++)
            buffer[i] += v;
    }

    inline void minus(long home, Z v) {
        for (long i = home; i < last; i++)
            buffer[i] -= v;
    }

    inline void multiply(long home, Z v) {
        for (long i = home; i < last; i++)
            buffer[i] *= v;
    }

    inline void divide(long home, Z v) {
        for (long i = home; i < last; i++)
            buffer[i] /= v;
    }

    inline Z sum(long home) {
        Z& s = buffer[sz];
        s = 0;
        for (long i = home; i < last; i++)
            s += buffer[i];
        return s;
    }

    inline Z product(long home) {
        if (last == home)
            return 0;
        Z& p = buffer[sz];
        p = buffer[home];
        for (long i = home + 1; i < last && p; i++)
            p *= buffer[i];
        return p;
    }

    inline void minvalue(Z& v, Z m) {
        v = (v<m)?v:m;
    }

    inline void maxvalue(Z& v, Z m) {
        v = (v>m)?v:m;
    }

    inline Z mini(long home) {
        if (last == home)
            return 0;
        Z& m = buffer[sz];
        m = buffer[home];
        for (long i = home + 1; i < last; i++)
            minvalue(m, buffer[i]);
        return m;
    }

    inline Z maxi(long home) {
        if (last == home)
            return 0;
        Z& M = buffer[sz];
        M = buffer[home];
        for (long i = home + 1; i < last; i++)
            maxvalue(M, buffer[i]);
        return M;
    }

    inline bool minmax(long home, Z& m, Z& M) {
        if (last == home)
            return false;
        m = buffer[home];
        M = m;
        for (long i = home + 1; i < last; i++) {
            minvalue(m, buffer[i]);
            maxvalue(M, buffer[i]);
        }
        return true;
    }

    inline long replaceall(long home, Z test, Z value) {
        long nb = 0;
        for (long i = home; i < last; i++) {
            nb += (buffer[i] == test);
            buffer[i] = (buffer[i] == test)?value:buffer[i];
        }
        return nb;
    }
        
    ~item_a() {
        free(buffer);
    }
    
};

template <class Z> class vecte_a {
public:

    //It is always the same
    item_a<Z>* items;
    long home;

    vecte_a(vecte_a<Z>& l) {
        home = 0;
        items = new item_a<Z>(l.size());
        while (items->last < l.items->last - l.home) {
            items->buffer[items->last] = l[items->last];
            items->last++;
        }
    }

    vecte_a(vecte_a<Z>& l, long pos) {
        home  = pos + l.home;
        items = l.items;
        //We modify the common reference counter
        items->status++;
    }

    vecte_a(long nb, Z v) {
        home = 0;
        items = new item_a<Z>(nb);
        while (nb > 0) {
            items->push_raw(v);
            nb--;
        }
    }

    vecte_a(long t = 8) {
        home = 0;
        items = new item_a<Z>(t);
    }

    ~vecte_a() {
        if (!items->status)
            delete items;
        else
            items->status--;
    }

    inline void reserve(long t) {
        items->reserve(t);
    }

    inline void reset() {
        items->last = 0;
    }

    inline void put(long pos, Z val) {
        items->buffer[pos + home] = val;
    }

    uint16_t shared(uint16_t status) {
        return status + (items->status != 0);
    }

    inline Z exchange(long pos, Z val) {
        Z e = items->buffer[pos + home];
        items->buffer[pos + home] = val;
        return e;
    }

    inline Z exchangelast(Z val) {
        Z e = items->buffer[items->last - 1];
        items->buffer[items->last - 1] = val;
        return e;
    }

    inline long size() {
        return items->last - home;
    }

    void clear() {
        home = 0;
        if (items->status) {
            //In this case it is a shared buffer
            //We have no right to it anymore
            //We need to provide a new one
            items->status--;
            items = new item_a<Z>(8);
        }
        else
            items->last = 0;
    }

    void clean() {
        items->last = home;
    }
    
    inline void pop_back() {
        items->last--;
    }

    inline void swap(long i, long j) {
        items->swap(i, j);
    }
    
    inline void beforelast(Z val) {
        items->beforelast(val);
    }

    inline void insert(long pos, Z val) {
        items->insert(pos + home, val);
    }

    inline Z back() {
        return items->buffer[items->last - 1];
    }

    inline void push_back(Z val) {
        items->push_back(val);
    }

    inline void push_raw(Z val) {
        items->push_raw(val);
    }

    inline void extend(vecte_a<Z>* val) {
        items->extend(val->item, val->home);
    }

    inline Z& operator[](long pos) {
        return items->buffer[pos+home];
    }
    
    void erase(long pos) {
        items->erase(pos +home);
    }

    void reverse() {
        long sz = items->last - 1;
        for (long i = home; i < sz && items->reverse(i,sz); i++) {
            sz--;
        }
    }
        
    void operator =(vecte_a<Z>& z) {
        items->last = home;
        items->reserve(z.items->sz);
        for (long i = z.home; i < z.items->last; i++)
            items->buffer[items->last++] = z.items->buffer[i];
    }

    void operator =(vecte<Z>& z) {
        items->last = home;
        items->reserve(z.size());
        for (long i = 0; i < z.size(); i++)
            items->buffer[items->last++] = z[i];
    }

    inline bool empty() {
        return (home == items->last);
    }

    inline void at(long pos, Z val) {
        items->at(pos + home, val);
    }

    inline void atlast(Z val) {
        items->atlast(val);
    }

    inline void padding(long nb) {
        items->padding(nb + home);
    }

    inline void padding(long nb, Z v) {
        items->padding(nb + home, v);
    }
    
    inline long search(Z v, long i) {
        i += home;
        for (; i< items->last && v != items->buffer[i]; i++) {}
        return (i == items->last)?-1:i-home;
    }

    inline long search_back(Z v, long ix) {
        long i = items->last - 1;
        ix += home;
        for (; i >= ix && v != items->buffer[i]; i--) {}
        return (i < ix)?-1:i-home;
    }

    inline bool check(Z v) {
        long i = home;
        for (; i< items->last && v != items->buffer[i]; i++) {}
        return (i != items->last);
    }

    inline bool operator ==(vecte_a<Z>& v) {
        if (v.items != items && size() == v.size()) {
            long i = home;
            for (; i < items->last && v[i] == items->buffer[i]; i++) {}
            return (i == items->last);
        }
        return (v.items == items && v.home == home);
    }
    
    inline void to_vector(std::vector<Z>& v) {
        for (long i = home; i < items->last; i++) {
            v.push_back(items->buffer[home+i]);
        }
    }
        
    Z sum() {
        return items->sum(home);
    }

    Z product() {
        return items->product(home);
    }
    
    Z mini() {
        return items->mini(home);
    }

    Z maxi() {
        return items->maxi(home);
    }
    
    bool minmax(Z& m, Z& M) {
        return items->minmax(home, m, M);
    }
    
    long count(Z v) {
        return items->counting(home, v);
    }

    long replaceall(Z test, Z value) {
        return items->replaceall(home, test, value);
    }
    
    void plus(vecte_a<Z>& n, long nb) {
        items->plus(home, n.home, n.items->buffer, nb);
    }

    void minus(vecte_a<Z>& n, long nb) {
        items->minus(home, n.home, n.items->buffer, nb);
    }

    void multiply(vecte_a<Z>& n, long nb) {
        items->multiply(home, n.home, n.items->buffer, nb);
    }

    void plus(Z v) {
        items->plus(home, v);
    }

    void minus(Z v) {
        items->minus(home, v);
    }

    void multiply(Z v) {
        items->multiply(home, v);
    }

    void divide(Z v) {
        items->divide(home, v);
    }

    void searchall(vecte_a<long>& indexes, Z v, long ix) {
        for (long i = home + ix; i < items->last; i++) {
            if (items->buffer[i] == v)
                indexes.push_back(i - home);
        }
    }
    
};

//We use the new method here. The alloc cannot work for strings...
template <class Z> class item_n {
public:
    Z* buffer;
    Z vnull;
    long last;
    long sz;
    long status;

    item_n(long t) {
        status = 0; //this is the reference counter
        last = 0; //this is the last element
        sz = t; //this is the size
        //We always create one more element
        //to handle some room for exchange
        buffer = new Z[sz + 1];
        //hence buffer[sz] does exist even though
        //it cannot be accessed through normal means
    }
    
    inline void reserve(long t) {
        if (t > sz) {
            sz = t;
            //We reallocate our vecteur
            Z* b = new Z[sz + 1];
            for (long i = 0; i < last; i++)
                b[i] = buffer[i];
            delete[] buffer;
            buffer = b;
        }
    }
    
    inline void resize(long t) {
        if (t >= sz) {
            sz = t << 1;
            //We reallocate our vecteur
            Z* b = new Z[sz + 1];
            for (long i = 0; i < last; i++)
                b[i] = buffer[i];
            delete[] buffer;
            buffer = b;
        }
    }
    
    inline void swap(long left, long right) {
        //We use the fact that the last element exists
        //but cannot be accessed...
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
    }

    inline bool reverse(long left, long right) {
        if (left >= right)
            return false;
        buffer[sz] = buffer[left];
        buffer[left] = buffer[right];
        buffer[right] = buffer[sz];
        return true;
    }
    
    inline void erase(long i) {
        if (i >= 0 && i < last) {
            last--;
            for (;i < last; i++)
                buffer[i] = buffer[i+1];
        }
    }

    inline void at(long pos, Z& val) {
        resize(pos + 1);
        buffer[pos] = val;
    }

    inline void beforelast(Z& val) {
        if (last) {
            resize(last);
            buffer[last] = buffer[last-1];
            buffer[last-1] = val;
            last++;
        }
        else
            buffer[last++] = val;
    }

    inline void atlast(Z& val) {
        buffer[last-1] = val;
    }

    inline void insert(long pos, Z& val) {
        resize(last);

        if (pos >= last) {
            buffer[last++] = val;
            return;
        }

        //All elements are moved to the right.
        for (long i = last; i > pos; i--)
            buffer[i] = buffer[i - 1];
        buffer[pos] = val;
        last++;
    }

    inline void extend(item_n<Z>* val, long val_home) {
        resize(last+val->last);
        for (long i = val_home; i < val->last; i++) {
            buffer[last++] = val->buffer[i];
        }
    }

    inline void push_back(Z& val) {
        resize(last);
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }

    inline void push_raw(Z& val) {
        //sinon on ajoute l'element en queue...
        buffer[last++] = val;
    }

    void plus(long home, Z& v) {
        for (long i = home; i < last; i++)
            buffer[i] += v;
    }

    long counting(long home, Z& v) {
        long count = 0;
        for (long i = home; i < last; i++)
            count += (buffer[i] == v);
        return count;
    }

    Z sum(long home) {
        Z& s = buffer[sz];
        s = vnull;
        for (long i = home; i < last; i++)
            s += buffer[i];
        return s;
    }

    inline void minvalue(Z& v, Z m) {
        v = (v<m)?v:m;
    }

    inline void maxvalue(Z& v, Z m) {
        v = (v>m)?v:m;
    }

    Z mini(long home) {
        if (last == home)
            return vnull;
        
        Z& m = buffer[sz];
        m = buffer[home];
        for (long i = home + 1; i < last; i++)
            minvalue(m, buffer[i]);
        return m;
    }

    Z maxi(long home) {
        if (last == home)
            return vnull;

        Z& M = buffer[sz];
        M = buffer[home];
        for (long i = home + 1; i < last; i++)
            maxvalue(M, buffer[i]);
        return M;
    }

    long replaceall(long home, Z& test, Z& value) {
        long nb = 0;
        for (long i = home; i < last; i++) {
            nb += (buffer[i] == test);
            buffer[i] = (buffer[i] == test)?value:buffer[i];
        }
        return nb;
    }

    bool minmax(long home, Z& m, Z& M) {
        if (last == home)
            return false;
        
        m = buffer[home];
        M = m;
        for (long i = home + 1; i < last; i++) {
            minvalue(m, buffer[i]);
            maxvalue(M, buffer[i]);
        }
        return true;
    }

    ~item_n() {
        delete[] buffer;
    }
    
};

template <class Z> class vecte_n {
public:

    //It is always the same
    item_n<Z>* items;
    long home;

    vecte_n(vecte_n<Z>& l) {
        home = 0;
        items = new item_n<Z>(l.size());
        while (items->last < l.items->last - l.home) {
            items->buffer[items->last] = l[items->last];
            items->last++;
        }
    }

    vecte_n(vecte_n<Z>& l, long pos) {
        home  = pos + l.home;
        items = l.items;
        //We modify the common reference counter
        items->status++;
    }

    vecte_n(long nb, Z v) {
        home = 0;
        items = new item_n<Z>(nb);
        while (nb > 0) {
            items->push_raw(v);
            nb--;
        }
    }

    vecte_n(long t = 8) {
        home = 0;
        items = new item_n<Z>(t);
    }

    ~vecte_n() {
        if (!items->status)
            delete items;
        else
            items->status--;
    }

    inline void put(long pos, Z val) {
        items->buffer[pos + home] = val;
    }

    uint16_t shared(uint16_t status) {
        return status + (items->status != 0);
    }

    inline Z exchange(long pos, Z val) {
        Z e = items->buffer[pos + home];
        items->buffer[pos + home] = val;
        return e;
    }

    inline Z exchangelast(Z val) {
        Z e = items->buffer[items->last - 1];
        items->buffer[items->last - 1] = val;
        return e;
    }

    inline long size() {
        return items->last - home;
    }

    void clear() {
        home = 0;
        if (items->status) {
            //In this case it is a shared buffer
            //We have no right to it anymore
            //We need to provide a new one
            items->status--;
            items = new item_n<Z>(8);
        }
        else
            items->last = 0;
    }

    void clean() {
        items->last = home;
    }
    
    inline void pop_back() {
        items->last--;
    }

    inline void swap(long i, long j) {
        items->swap(i, j);
    }
    
    inline void beforelast(Z val) {
        items->beforelast(val);
    }

    inline void insert(long pos, Z val) {
        items->insert(pos + home, val);
    }

    inline Z& back() {
        return items->buffer[items->last - 1];
    }

    inline void push_back(Z val) {
        items->push_back(val);
    }

    inline void push_raw(Z val) {
        items->push_raw(val);
    }

    inline void extend(vecte_n<Z>* val) {
        items->extend(val->item, val->home);
    }

    inline Z& operator[](long pos) {
        return items->buffer[pos+home];
    }
    
    void erase(long pos) {
        items->erase(pos +home);
    }

    inline void reserve(long t) {
        items->reserve(t);
    }
    
    void reverse() {
        long sz = items->last - 1;
        for (long i = home; i < sz && items->reverse(i,sz); i++) {
            sz--;
        }
    }
        
    void operator =(vecte_n<Z>& z) {
        items->last = home;
        items->reserve(z.items->sz);
        for (long i = z.home; i < z.items->last; i++)
            items->buffer[items->last++] = z.items->buffer[i];
    }
    
    inline bool empty() {
        return (home == items->last);
    }

    inline void at(long pos, Z val) {
        items->at(pos + home, val);
    }

    inline void atlast(Z val) {
        items->atlast(val);
    }

    inline long search(Z& v, long i) {
        i += home;
        for (; i< items->last && v != items->buffer[i]; i++) {}
        return (i == items->last)?-1:i-home;
    }

    inline long search_back(Z& v, long ix) {
        long i = items->last - 1;
        ix += home;
        for (; i >= ix && v != items->buffer[i]; i--) {}
        return (i < ix)?-1:i-home;
    }

    inline bool check(Z& v) {
        long i = home;
        for (; i< items->last && v != items->buffer[i]; i++) {}
        return (i != items->last);
    }

    inline bool operator ==(vecte_n<Z>& v) {
        if (v.items != items && size() == v.size()) {
            long i = home;
            for (; i < items->last && v[i] == items->buffer[i]; i++) {}
            return (i == items->last);
        }
        return (v.items == items && v.home == home);
    }

    inline void to_vector(std::vector<Z>& v) {
        for (long i = home; i < items->last; i++) {
            v.push_back(items->buffer[home+i]);
        }
    }

    Z sum() {
        return items->sum(home);
    }

    Z mini() {
        return items->mini(home);
    }

    Z maxi() {
        return items->maxi(home);
    }

    bool minmax(Z& m, Z& M) {
        return items->minmax(home, m, M);
    }
    
    long count(Z& v) {
        return items->counting(home, v);
    }

    void plus(Z& v) {
        items->plus(home, v);
    }

    long replaceall(Z& test, Z& value) {
        return items->replaceall(home, test, value);
    }

    void searchall(vecte_a<long>& indexes, Z& v, long ix) {
        for (long i = home + ix; i < items->last; i++) {
            if (items->buffer[i] == v)
                indexes.push_back(i - home);
        }
    }

};
#endif






