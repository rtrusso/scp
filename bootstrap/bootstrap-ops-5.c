
     /* ========== other i/o part ========== */
     case OP_GETENV: {
          char *env = getenv(strvalue(car(sc->args)));
          if (env) {
               x=mk_string(sc,env);
               s_return(sc,x);
          } else {
               s_retbool(0);
          }
     }

     case OP_SETENV: {
         char *name = strvalue(car(sc->args));
         char *val = strvalue(cadr(sc->args));
#ifdef TS_BOOTSTRAP_MINGW32
         BOOL b = SetEnvironmentVariableA(name, val);
         s_retbool(b);
#else
         int s = (0==setenv(name,val,1));
         s_retbool(s);
#endif
     }

     case OP_UNLINK: {
          int s = (0==unlink(strvalue(car(sc->args))));
          s_retbool(s);
     }

     case OP_MKDIR: {
#ifdef TS_BOOTSTRAP_MINGW32
         int s = (0==mkdir(strvalue(car(sc->args))));
#else
         int s = (0==mkdir(strvalue(car(sc->args)), 0777));
#endif
         s_retbool(s);
     }

     case OP_ELAPSED_S: {
          static time_t t_base = 0;
          time_t t_now;
          if (t_base == 0) { time(&t_base); }
          time(&t_now);
          s_return(sc,mk_integer(sc,(t_now - t_base)));
     }

     case OP_ELAPSED_MS: {
          static struct timeval tv_base = {0,0};
          struct timeval tv_now;
          int ms;
          if (tv_base.tv_sec==0) { gettimeofday(&tv_base, NULL); }
          gettimeofday(&tv_now, NULL);
          ms = (tv_now.tv_sec - tv_base.tv_sec) * 1000;
          ms += (tv_now.tv_usec - tv_base.tv_usec) / 1000;
          s_return(sc,mk_integer(sc,ms));
     }

     case OP_STAT: {
          struct stat buf;
          int s = (0==stat(strvalue(car(sc->args)),&buf));
          if (s) {
               pointer vec;
               vec = mk_vector(sc,10);
               set_vector_elem(vec,7,mk_integer(sc,buf.st_size));
               set_vector_elem(vec,9,mk_integer(sc,buf.st_mtime));
               s_return(sc,vec);
          } else {
               s_retbool(0);
          }
     }

     case OP_RENAME_FILE: {
          char *old = strvalue(car(sc->args));
          char *new = strvalue(cadr(sc->args));
          int s = (0==rename(old,new));
          s_retbool(s);
     }

     case OP_EVALCTR: {
         s_return(sc,mk_integer(sc,Eval_Cycle_Count));
     }
