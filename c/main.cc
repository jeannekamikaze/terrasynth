#include "gradient.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <time.h>

int write_pgm (const char* path, float* data, int w, int h)
{
    const int n = w*h;
    int i;
    FILE* f = fopen (path, "w");
    if (!f) {
        return 0;
    }
    fprintf (f, "P5 %d %d %d\n", w, h, 255);
    for (i = 0; i < n; ++i, ++data) {
        unsigned char c = (char) (*data * 255.0f);
        fwrite (&c, 1, 1, f);
    }
    fclose (f);
    return 1;
}

void barf (const char* err)
{
    fprintf (stderr, "%s\n", err);
    exit (-1);
}

enum exec_type_t { exec_fbm, exec_perlin };

exec_type_t parse_exec_type (const char* str)
{
    return (exec_type_t) (strcmp(str, "perlin") == 0);
}

int main (int argc, char** argv)
{
    int w = 64;
    int h = 64;
    int seed = time(NULL) * getpid();
    int freq = 1;
    int cell_size = 64;
    int octaves = 1;
    float lacunarity = 2.0f;
    float H = 0.5f;

    exec_type_t exec_type; 

    if (argc < 2 || strcmp(argv[1], "--help") == 0) {
        fprintf (stderr, "Usage: %s <output file> [options]\n", argv[0]);
        fprintf (stderr, "Options:\n");
        fprintf (stderr, "    -w <int>                            - Output image width\n");
        fprintf (stderr, "    -h <int>                            - Output image height\n");
        fprintf (stderr, "    -s <int>                            - Random seed\n");
        fprintf (stderr, "    -t <perlin | fBm> [noise options]   - Noise type\n");
        fprintf (stderr, "\n");
        fprintf (stderr, "Perlin options:\n");
        fprintf (stderr, "    -f <freq>                           - Frequency\n");
        fprintf (stderr, "    -c <cell size>                      - Size of a cell in the lattice\n");
        fprintf (stderr, "\n");
        fprintf (stderr, "fBm options:\n");
        fprintf (stderr, "    -n <int>                            - Number of octaves to add\n");
        fprintf (stderr, "    -l <float>                          - Lacunarity\n");
        fprintf (stderr, "    -H <float>                          - Hurst exponent\n");
        return 0;
    }

    const char* prog = argv[0];
    const char* f_out = argv[1];

    int c;
    while ((c = getopt (argc, argv, "w:h:s:t:f:c:n:l:h:")) != -1) {
        switch (c) {
        case 'w': w = atoi (optarg); break;
        case 'h': h = atoi (optarg); break;
        case 's': seed = atoi (optarg); break;
        case 't': exec_type = parse_exec_type (optarg); break;
        case 'f': freq = atoi (optarg); break;
        case 'c': cell_size = atoi (optarg); break;
        case 'n': octaves = atoi (optarg); break;
        case 'l': lacunarity = atof (optarg); break;
        case 'H': H = atof (optarg); break;
        case '?':
            fprintf (stderr, "Option -%c requires an argument.\n", optopt);
            fprintf (stderr, "Run '%s --help' for details\n", prog);
            return -1;
        default: return -2;
        }
    }

    float* bits = new float [w*h];
    if (exec_type == exec_perlin) {
        printf ("Running perlin noise\n");
        noise (seed, bits, w, h, cell_size, freq);
    }
    else {
        printf ("Running fBm\n");
        fbm (seed, bits, w, h, cell_size, lacunarity, H, octaves);
    }
    if (!write_pgm (f_out, bits, w, h)) {
        fprintf (stderr, "Failed writing to %s.\n", f_out);
    }
    printf ("Image successfully written to %s\n", f_out);
    delete[] bits;

    return 0;
}
