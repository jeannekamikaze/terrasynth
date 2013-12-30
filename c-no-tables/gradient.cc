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

vec2 normalise (vec2 a)
{
    float m = sqrt(a.x*a.x + a.y*a.y);
    m = m == 0.0 ? 1.0 : 1.0/m;
    return vec2(a.x*m, a.y*m);
}

float dot (vec2 a, vec2 b)
{
    return a.x*b.x + a.y*b.y;
}

// 3t^2 - 2t^3
float scurve (float t)
{
    return t * t * (t * -2 + 3);
}

// 6t^5 - 15t^4 + 10t^3
float scurve5 (float t)
{
    return t * t * t * (t * (t * 6 - 15) + 10);
}

float lerp (float a, float b, float t)
{
    return a + (b-a)*t;
}

int idx1 (float x)
{
    return (int) (sin(x*128353.989558) * 4758.5412653);
}

int idx (vec2 p)
{
    return idx1(p.x + idx1(p.y));
}

vec2 grad (int x)
{
    float gx = cos((float)x);
    float gy = sin((float)x);
    return normalise(vec2(gx,gy));
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

    vec2 g0 = grad(idx(vec2(x0,y0)));
    vec2 g1 = grad(idx(vec2(x1,y0)));
    vec2 g2 = grad(idx(vec2(x0,y1)));
    vec2 g3 = grad(idx(vec2(x1,y1)));

    float s = dot (g0, p-p0);
    float t = dot (g1, p-p1);
    float u = dot (g2, p-p2);
    float v = dot (g3, p-p3);

    float sx = scurve5 (x - (float)x0);
    float sy = scurve5 (y - (float)y0);
    float a = lerp (s, t, sx);
    float b = lerp (u, v, sx);
    float c = lerp (a, b, sy);

    // Map from [-1,1] to [0,1]
    c = c*0.5f + 0.5f;

    return c;
}

void noise (int seed, float* image, int w, int h, int cell_size, int freq)
{
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
    const float s = 1.0f / cell_size;
    const float gain = pow (lacunarity, -2*H);
    const float fbm_max = gain == 1.0f ? 1.0f : geom (gain, octaves);
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
