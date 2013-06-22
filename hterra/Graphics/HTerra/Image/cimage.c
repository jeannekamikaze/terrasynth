#include <stdio.h>

typedef unsigned char U8;

void write_pgm (int w, int h, const float* image, const char* path)
{
    const unsigned n = (unsigned) w * (unsigned) h;
    int i;
    FILE* f = fopen (path, "w");
    fprintf (f, "P5 %d %d 255\n", w, h);
    for (i = 0; i < n; ++i) {
        U8 c = (U8) (*image++ * 255.0f);
        fputc (c, f);
    }
    fclose (f);
}
