#ifndef BIG_MATRIX_ACCESSOR
#define BIG_MATRIX_ACCESSOR

#include "BigMatrix.h"

//TODO: It would be nice to have accessors handle row and column offsets
//however, the only way to do this would be with a proxy.  Is there another
//solution so that the offsets don't have to go in Cbigmemory.cpp?

// The BigMatrixAccessor class allows the user to access non-separated
// big matrix data as matrix[i][j].
template<typename T>
class BigMatrixAccessor
{
  public:
    BigMatrixAccessor( T* pData, const unsigned long nrow)
    {
      _pMat = pData;
      _nrow = nrow;
    };

    BigMatrixAccessor( BigMatrix &bm )
    {
      _pMat = reinterpret_cast<T*>(reinterpret_cast<T*>(bm.matrix()));
      _nrow = bm.num_rows();
    };

    inline T* operator[](const unsigned long col) {return _pMat+_nrow*col;};

  protected:
    T *_pMat;
    long _nrow;
};

template<typename T>
class SepBigMatrixAccessor
{
  public:
    SepBigMatrixAccessor( BigMatrix &bm)
    {
      _ppMat = reinterpret_cast<T**>(bm.matrix());
    };

    inline T* operator[](const unsigned long col) {return _ppMat[col];}
  protected:
    T **_ppMat;
};

#endif //BIG_MATRIX_ACCESSOR
