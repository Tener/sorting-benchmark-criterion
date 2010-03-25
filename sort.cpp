#include <algorithm>
#include <vector>

using namespace std;


// declaration:

#ifdef __cplusplus
extern "C" {
#endif

void sort_ints_intr( int * arr, int size );
void sort_ints_heap( int * arr, int size );
void sort_ints_stable( int * arr, int size );
void sort_ints_NOT( int * arr, int size );

#ifdef __cplusplus
} 
#endif

// implementation:

void sort_ints_intr( int * arr, int size )
{
  sort( arr, arr + size );
}

void sort_ints_heap( int * arr, int size )
{
  sort_heap( arr, arr + size );
}

void sort_ints_stable( int * arr, int size )
{
  stable_sort( arr, arr + size );
}

void sort_ints_NOT( int * arr, int size )
{
  return ;
}
