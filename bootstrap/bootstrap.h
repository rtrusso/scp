#ifdef TS_BOOTSTRAP_MINGW32
#include <windows.h>
#include <sys/time.h>
#include <io.h>
#else
#include <time.h>
#include <sys/time.h>
#endif
