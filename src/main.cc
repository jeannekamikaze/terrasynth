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
    
    HmApp app;
    app.setup (640, 480, "Heightmap Viewer 0.1", 4, 2);
    app.setHeightMap (argv[1]);
    app.runCapped (30);
    
    /*OGDT::Image image;
    OGDT::Image::from_file (argv[1], image);
    
    printf ("width: %d, height: %d\n", image.width(), image.height());*/
        
    return 0;
}
