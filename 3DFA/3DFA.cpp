#include "..\3DFC\3DFC.h"
#include "3DFH_stub.h"

int main(int argc, char **argv)
{
    hs_init(&argc, &argv);
    startwin(pixarrayC);
    hs_exit();
    return 0;
}