#include "HmApp.h"
#include "HeightMap.h"
#include <OGDT/Image.h>
#include <cstdio>


int main (int argc, char** argv)
{    
    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <heightmap>\n", argv[0]);
        return 0;
    }
    
    HmApp app (argv[1]);
    app.runCapped (30);
        
    return 0;
}
