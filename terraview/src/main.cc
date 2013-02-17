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
    
    using namespace OGDT;

    Image img;
    Image::from_file (argv[1], img);
    printf ("Image loaded\n");
    TImage<float,1> image = img.coerce<float,1>();

    int w = image.width();
    int h = image.height();

    for (int i = 0; i < 4; ++i) printf ("%f, ", image[i]);
    printf ("\n");
    for (int i = w*h-1; i >= w*h-4; --i) printf ("%f, ", image[i]);
    printf ("\n");
      
    return 0;
}
