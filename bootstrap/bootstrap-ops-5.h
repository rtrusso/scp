    _OP_DEF(opexe_5, "getenv",                         1,  1,       TST_STRING,                      OP_GETENV           )
    _OP_DEF(opexe_5, "setenv",                         2,  2,       TST_STRING TST_STRING,           OP_SETENV           )
    _OP_DEF(opexe_5, "delete-file",                    1,  1,       TST_STRING,                      OP_UNLINK           )
    _OP_DEF(opexe_5, "create-directory",               1,  1,       TST_STRING,                      OP_MKDIR            )
    _OP_DEF(opexe_5, "current-seconds",                0,  0,       0,                               OP_ELAPSED_S        )
    _OP_DEF(opexe_5, "current-milliseconds",           0,  0,       0,                               OP_ELAPSED_MS       )
    _OP_DEF(opexe_5, "stat",                           1,  1,       TST_STRING,                      OP_STAT             )
    _OP_DEF(opexe_5, "rename-file",                    2,  2,       TST_STRING TST_STRING,           OP_RENAME_FILE      )
    _OP_DEF(opexe_5, "current-evaluation-counter",     0,  0,       0,                               OP_EVALCTR          )
