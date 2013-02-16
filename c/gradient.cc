#include "gradient.h"
#include <cstdio>
#include <cstdlib>
#include <cmath>

#define TABLE_SIZE 256
#define TABLE_MASK (TABLE_SIZE-1)

typedef unsigned char U8;

struct vec2
{
    float x;
    float y;

    vec2 () {}

    vec2 (float _x, float _y) : x (_x), y (_y) {}
};

vec2 operator- (vec2 a, vec2 b)
{
    return vec2 (a.x-b.x, a.y-b.y);
}

unsigned char perms [TABLE_SIZE];
vec2 grads [TABLE_SIZE];

#define PERM(x)       perms [(x) & TABLE_MASK]
#define INDEX(ix,iy)  PERM ((ix)+PERM((iy)))

void setup (int seed)
{
    srand (seed);
    
    // Construct a random permutaion table of values uniformly distributed
    // in the 0..255 range.
    for (int i = 0; i < TABLE_SIZE; ++i) perms[i] = i;
    for (int i = 0; i < TABLE_SIZE; ++i) {
        U8 j = rand ();
        perms[i] ^= perms[j] ^= perms[i];
    }

    // Construct a random gradient table of values uniformly distributed
    // along the unit circle.
    float step = 2*M_PI / (float) TABLE_SIZE;
    float angle = 0.0f;
    for (int i = 0; i < TABLE_SIZE; ++i, angle += step) {
        grads[i].x = cos (angle);
        grads[i].y = sin (angle);
    }

/*#ifdef DEBUG
    // Print perms and gradients tables.
    printf ("Permutations table\n");
    for (int i = 0; i < PERM_TABLE_SIZE; ++i) printf ("%d ", perms[i]);

    printf ("\n\nGradients table\n");
    for (int i = 0; i < GRAD_TABLE_SIZE; ++i) {
        printf ("(%f, %f) ", grads[i].x, grads[i].y);
    }
    printf ("\n");
#endif // DEBUG
*/
}

float dot (vec2 a, vec2 b)
{
    return a.x*b.x + a.y*b.y;
}

float scurve (float x)
{
    float x2 = x*x;
    return 3*x2 - 2*x2*x;
}

float lerp (float a, float b, float t)
{
    return a + (b-a)*t;
}

float noise (float x, float y)
{
    int x0 = x;
    int y0 = y;
    int x1 = x0 + 1;
    int y1 = y0 + 1;

    vec2 p  = vec2 (x,y);
    vec2 p0 = vec2 (x0,y0);
    vec2 p1 = vec2 (x1,y0);
    vec2 p2 = vec2 (x0,y1);
    vec2 p3 = vec2 (x1,y1);

    const vec2& g0 = grads[INDEX(x0,y0)];
    const vec2& g1 = grads[INDEX(x1,y0)];
    const vec2& g2 = grads[INDEX(x0,y1)];
    const vec2& g3 = grads[INDEX(x1,y1)];

    float s = dot (g0, p-p0);
    float t = dot (g1, p-p1);
    float u = dot (g2, p-p2);
    float v = dot (g3, p-p3);

    float sx = scurve (x - (float)x0);
    float sy = scurve (y - (float)y0);
    float a = lerp (s, t, sx);
    float b = lerp (u, v, sx);
    float c = lerp (a, b, sy);

    // Map from [-1,1] to [0,1]
    c = c*0.5f + 0.5f;

    return c;
}

void noise (int seed, float* image, int w, int h, int cell_size, int freq)
{
    setup (seed);
    const float s = (float) freq / (float) cell_size;
    for (int i = 0; i < h; ++i) {
        for (int j = 0; j < w; ++j) {
            *image++ = noise ((float)j*s, (float)i*s);
        }
    }
}

float geom (float r, float n)
{
    return (1.0f - pow(r,n)) / (1.0f - r);
}

void fbm (int seed, float* image, int w, int h, int cell_size, float lacunarity, float H, int octaves)
{
    setup (seed);
    const float s = 1.0f / cell_size;
    const float gain = pow (lacunarity, -2*H);
    const float fbm_max = geom (gain, octaves);
    for (int i = 0; i < h; ++i) {
        for (int j = 0; j < w; ++j) {
            float* p = image++;
            float f = 1.0f;
            float a = 1.0f;
            for (int x = 0; x < octaves; ++x) {
                *p += a * noise ((float)j*f*s, (float)i*f*s);
                f *= lacunarity;
                a *= gain;
            }
            *p /= fbm_max;
        }
    }
}
