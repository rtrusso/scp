#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sasm.h>

int SYM(c_scheme_argc) = 0;
int **SYM(c_scheme_argv) = NULL;

extern void *SYM(c_scheme_rtl_init_argv)();

void rtlscheme_init_argv(int argc, char **argv) {
    int i, j, n;
    int *str;

    SYM(c_scheme_argc) = argc;
    SYM(c_scheme_argv) = (int**)malloc(sizeof(int*) * (argc + 2));
    SYM(c_scheme_argv)[0] = (int*)argc;
    SYM(c_scheme_argv)[1] = (int*)(5 | 0x80000000);
    for (i = 0; i < argc; ++i) {
        n = strlen(argv[i]);
        str = (int*)malloc(sizeof(int) * (n+3));
        SYM(c_scheme_argv)[i+2] = str;
        str[0] = n;
        str[1] = 6;
        for (j = 0; j < n; ++j) {
            str[j+2] = argv[i][j];
        }
        str[n+2] = 0;
    }

    SYM(c_scheme_rtl_init_argv)();
}

FILE* SYM(c_scheme_get_stdin)(void) {
    return stdin;
}

FILE* SYM(c_scheme_get_stdout)(void) {
    return stdout;
}

void SYM(c_scheme_type_error)(int expected_type, void *obj) {
    _ftprintf(stderr, TEXT("scheme type error: expected:%d %p\n"), expected_type, obj);
    _ftprintf(stderr, TEXT("                     actual:%d\n"), (obj ? ((int*)obj)[1] : 0));
    ExitProcess(1);
} /* c_scheme_type_error */

void SYM(c_scheme_arg_count_error)(int actual_args, int expected_args) {
    _ftprintf(stderr, TEXT("scheme arg count error; expected: %d actual: %d"), expected_args, actual_args);
    ExitProcess(1);
} /* c_scheme_arg_count_error */

void SYM(c_scheme_bounds_error)(int index, int length, int type) {
    _ftprintf(stderr, TEXT("scheme bounds check error; index: %d bound: %d type: %d"), index, length, type);
    ExitProcess(1);
}

int SYM(c_scheme_open_file)(const int *wide_fname, int len, int read) {
  TCHAR fname[1024], *iter;
  int i;

  if (len >= 1024) {
      _ftprintf(stderr, TEXT("file name is too long (%d)"), len);
      ExitProcess(1);
  }

  for (iter = fname, i = 0; i < len; ++i, ++iter, ++wide_fname)
    *iter = (TCHAR)*wide_fname;
  *iter = TEXT('\0');
  i = (int)_tfopen(fname, read ? TEXT("r") : TEXT("w"));
  return i;
} /* c_scheme_open_file */

int SYM(c_scheme_close_file)(FILE* file) {
  return fclose(file);
} /* c_scheme_close_file */

int SYM(c_scheme_read_file)(int file) {
    return _fgettc((FILE*)file);
} /* c_scheme_read_file */

int SYM(c_scheme_write_file)(int c, int file) {
  return _fputtc(c, (FILE*)file);
} /* c_scheme_write_file */

const char* SYM(c_scheme_getenv)(const int *wide_name, int len) {
    TCHAR name[1024], *iter;
    int i;
    const char *env;

    if (len >= 1024) {
        _ftprintf(stderr, TEXT("file name is too long (%d)"), len);
        ExitProcess(1);
    }

    for (iter = name, i = 0; i < len; ++i, ++iter, ++wide_name)
        *iter = (TCHAR)*wide_name;
    *iter = TEXT('\0');
    /*_ftprintf(stderr, TEXT("getenv (%d) [%s]=%s\n"), len, name, getenv(name));*/
    return getenv(name);
} /* c_scheme_getenv */

int SYM(c_scheme_delete_file)(const int *wide_fname, int len) {
  TCHAR fname[1024], *iter;
  int i;

  if (len >= 1024) {
      _ftprintf(stderr, TEXT("file name is too long (%d)"), len);
      ExitProcess(1);
  }

  for (iter = fname, i = 0; i < len; ++i, ++iter, ++wide_fname)
    *iter = (TCHAR)*wide_fname;
  *iter = TEXT('\0');
  i = (int)unlink(fname);
  return i;
} /* c_scheme_delete_file */

int SYM(c_scheme_rename_file)(const int *wide_fname, int len, const int *wide_fname2, int len2) {
  TCHAR fname[1024], fname2[1024], *iter;
  int i;

  if (len >= 1024) {
      _ftprintf(stderr, TEXT("file name is too long (%d)"), len);
      ExitProcess(1);
  }

  for (iter = fname, i = 0; i < len; ++i, ++iter, ++wide_fname)
    *iter = (TCHAR)*wide_fname;
  *iter = TEXT('\0');

  for (iter = fname2, i = 0; i < len2; ++i, ++iter, ++wide_fname2)
    *iter = (TCHAR)*wide_fname2;
  *iter = TEXT('\0');

  i = (int)rename(fname, fname2);
  return i;
} /* c_scheme_rename_file */

int SYM(c_scheme_stat_file)(const int *wide_fname, int len, int **vec, int vec_len) {
  TCHAR fname[1024], *iter;
  struct stat buf;
  int i;

  if (vec_len != 10) {
      _ftprintf(stderr, TEXT("c_scheme_stat_file vec_len must be 10 (%d)"), vec_len);
      ExitProcess(1);
  }

  if (len >= 1024) {
      _ftprintf(stderr, TEXT("file name is too long (%d)"), len);
      ExitProcess(1);
  }

  for (iter = fname, i = 0; i < len; ++i, ++iter, ++wide_fname)
    *iter = (TCHAR)*wide_fname;
  *iter = TEXT('\0');

  i = stat(fname, &buf);
  if (i == 0) {
      vec[7][0] = buf.st_size;
      vec[9][0] = buf.st_mtime;
  }

  return i;
} /* c_scheme_stat_file */

int SYM(c_scheme_current_seconds)() {
    static time_t t_base = 0;
    time_t t_now;
    if (t_base == 0) { time(&t_base); }
    time(&t_now);
    return t_now - t_base;
} /* c_scheme_current_seconds */

int SYM(c_scheme_current_milliseconds)() {
#ifdef _WIN32
    return (int)GetTickCount();
#else
    struct timespec ts;
    long nanosec;
    long microsec;
    long millisec;
    
    clock_gettime(CLOCK_MONOTONIC, &ts);
    nanosec = ts.tv_nsec;
    microsec = nanosec / 1000;
    millisec = microsec / 1000;
    millisec += ts.tv_sec * 1000;
    return millisec;
#endif
}

void SYM(c_scheme_error_stub)() {
    _ftprintf(stderr, TEXT("one of the scheme.java __sasm_impl functions was called, currently unsupported"));
    ExitProcess(1);
}
