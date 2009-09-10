#ifndef BIG_MATRIX_ACCESSOR
#define BIG_MATRIX_ACCESSOR

#include "BigMatrix.h"

// The BigMatrixAccessor class allows the user to access non-separated
// big matrix data as matrix[i][j].
template<typename T>
class BigMatrixAccessor
{
  public:
    BigMatrixAccessor( T* pData, const index_type nrow)
    {
      _pMat = pData;
      _nrow = nrow;
    }

    BigMatrixAccessor( BigMatrix &bm )
    {
      _pMat = reinterpret_cast<T*>(bm.matrix());
      _nrow = bm.num_rows();
    }

    inline T* operator[](const index_type col) {return _pMat+_nrow*col;}

  protected:
    T *_pMat;
    index_type _nrow;
};

template<typename T>
class SepBigMatrixAccessor
{
  public:
    SepBigMatrixAccessor( BigMatrix &bm)
    {
      _ppMat = reinterpret_cast<T**>(bm.matrix());
    }

    inline T* operator[](const index_type col) {return _ppMat[col];}
  protected:
    T **_ppMat;
};

#endif //BIG_MATRIX_ACCESSOR
