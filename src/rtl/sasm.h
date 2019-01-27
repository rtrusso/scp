#ifndef SASM_H_
#define SASM_H_

#ifdef _WIN32
# include <windows.h>
# include <tchar.h>
# define SYM(x) x
#else
# define _tmain main
# define _tprintf printf
# define _ftprintf fprintf
# define _tfopen fopen
# define _fgettc fgetc
# define _fputtc fputc
# define _tcslen strlen
# define TEXT(x) x
# include <stdlib.h>
# define ExitProcess exit
# define TCHAR char
# define VOID  void
# ifdef __CYGWIN__
#  define SYM(x) x
# else
#  define SYM(x) _##x
# endif
#endif

#endif /* SASM_H_ */
