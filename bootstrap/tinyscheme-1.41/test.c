#include "scheme-private.h"

int main(int argc, char **argv)
{
    pointer args, display;
    scheme *sc = scheme_init_new();
    if (!sc)
    {
        printf("scheme_init_new failed!\n");
        return 1;
    }

    scheme_set_input_port_file(sc, stdin);
    scheme_set_output_port_file(sc, stdout);
    args = _cons(sc, mk_integer(sc, 1), sc->NIL, 1);
    display = scheme_eval(sc, mk_symbol(sc, "display"));
    (void)scheme_call(sc, display, args);
    printf("\n");
    scheme_deinit(sc);
    return 0;
}
