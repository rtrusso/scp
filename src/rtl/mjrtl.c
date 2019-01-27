// mjrtl.c
// Very tiny runtime library for MiniJava programs
#include <stdio.h>
#include <string.h>
#include <sasm.h>

extern void SYM(sasm_entry)(void);
extern void SYM(c_mm_heap_add_area)( void *base, int words );
extern void SYM(c_mm_heap_add_fixed_area)( void *base, int chunk, int words );
void *SYM(gc_root_stack_limit);

#define VARIABLE_HEAP_SIZE ( 128 * 1024 * 1024 * sizeof(int) )
#define FIXED_HEAP_SIZE ( 128 * 1024 * 1024 * sizeof(int) )
#define FIXED_HEAP_CHUNK 16
#define RTL_VARIABLE_HEAP  1
#define RTL_FIXED_HEAP     1
#define RTL_2ND_FIXED_HEAP 0

#if SCHEME_RTL
extern void rtlscheme_init_argv(int argc, char **argv);
#endif

int _tmain( int argc, TCHAR *argv[] )
{
  void *pHeap;
  void *pHeapFixed;
  void *pHeapFixed2;

  SYM(gc_root_stack_limit) = &pHeap;

#if RTL_VARIABLE_HEAP
  pHeap = malloc( VARIABLE_HEAP_SIZE );
  if( NULL == pHeap )
  {
    _tprintf( TEXT("error allocating heap\n") );
    ExitProcess( 1 );
  }

  memset( pHeap, 0, VARIABLE_HEAP_SIZE );

  SYM(c_mm_heap_add_area)( pHeap,
			   VARIABLE_HEAP_SIZE / sizeof(int) );
#endif /* RTL_VARIABLE_HEAP */

#if RTL_FIXED_HEAP
  pHeapFixed = malloc( FIXED_HEAP_SIZE );
  if( NULL == pHeapFixed )
  {
    _tprintf( TEXT("error allocating fixed heap\n") );
    ExitProcess( 1 );
  }
  memset( pHeapFixed, 0, FIXED_HEAP_SIZE );
  SYM(c_mm_heap_add_fixed_area)( pHeapFixed,
				 FIXED_HEAP_CHUNK,
				 FIXED_HEAP_SIZE / sizeof(int) );
#endif /* RTL_FIXED_HEAP */

#if RTL_2ND_FIXED_HEAP
  pHeapFixed2 = malloc( FIXED_HEAP_SIZE );
  if( NULL == pHeapFixed2 )
  {
    _tprintf( TEXT("error allocating second fixed heap\n") );
    ExitProcess( 1 );
  }
  memset( pHeapFixed2, 0, FIXED_HEAP_SIZE );
  SYM(c_mm_heap_add_fixed_area)( pHeapFixed2,
				 FIXED_HEAP_CHUNK,
				 FIXED_HEAP_SIZE / sizeof(int) );
#endif /* RTL_2ND_FIXED_HEAP */

#if SCHEME_RTL
  rtlscheme_init_argv(argc, argv);
#endif /* SCHEME_RTL */

  SYM(sasm_entry)();
  return 0;
}

int SYM(mj_system_out_println)( int arg )
{
  _tprintf( TEXT("%d\n"), arg );
  fflush( stdout );
  return arg;
}

int SYM(mj_system_out_println_string)( TCHAR *s )
{
  _tprintf( TEXT("MESSAGE [%s]\n"), s );
  fflush( stdout );
  return 0;
}

int SYM(c_fail_malloc)( int words )
{
  _tprintf( TEXT("failed to allocate %u words of memory!\n"), words );
  ExitProcess( 1 );
}

int SYM(c_fail_bad_heap_check)(void)
{
  _tprintf(TEXT("heap consistency check failed"));
  ExitProcess(1);
}
