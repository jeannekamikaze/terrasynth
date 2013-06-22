#include "gradient.h"
#include <cstdio>
#include <cstdlib>
#include <cmath>

#define TABLE_SIZE 256
#define TABLE_MASK (TABLE_SIZE-1)

typedef unsigned char uchar;

// Device Code

inline __device__ float dot (const float2& a, const float2& b)
{
    return a.x*b.x + a.y*b.y;
}

// 3t^2 - 2t^3
inline __device__ float scurve (float t)
{
    return t * t * (t * -2 + 3);
}

// 6t^5 - 15t^4 + 10t^3
inline __device__ float scurve5 (float t)
{
    return t * t * t * (t * (t * 6 - 15) + 10);
}

inline __device__ float lerp (float a, float b, float t)
{
    return a + (b-a)*t;
}

#define PERM(x)      perms [(x) & TABLE_MASK]
#define INDEX(ix,iy) PERM ((ix)+PERM((iy)))

inline __device__ float2 operator- (const float2& a, const float2& b)
{
    return make_float2 (a.x-b.x, a.y-b.y);
}

__device__ float noise (const uchar* perms, const float2* grads, float x, float y)
{
    int x0 = x;
    int y0 = y;
    int x1 = x0 + 1;
    int y1 = y0 + 1;

    float2 p  = make_float2 (x,y);
    float2 p0 = make_float2 (x0,y0);
    float2 p1 = make_float2 (x1,y0);
    float2 p2 = make_float2 (x0,y1);
    float2 p3 = make_float2 (x1,y1);

    float2 g0 = grads[INDEX(x0,y0)];
    float2 g1 = grads[INDEX(x1,y0)];
    float2 g2 = grads[INDEX(x0,y1)];
    float2 g3 = grads[INDEX(x1,y1)];

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

__global__ void kernel_noise (const uchar* perms, const float2* grads, float s, int w, int h, float* image)
{
    unsigned n = blockIdx.x * blockDim.x + threadIdx.x;
    unsigned x = n % w;
    unsigned y = n / w;
    float* p = image + n;
    *p = noise (perms, grads, (float)x*s, (float)y*s);
}

__global__ void kernel_fbm
(const uchar* perms, const float2* grads, float octaves, float lacunarity, float gain, float fbm_max, float s, int w, int h, float* image)
{
    unsigned n = blockIdx.x * blockDim.x + threadIdx.x;
    unsigned x = n % w;
    unsigned y = n / w;
    float* p = image + n;

    *p = 0.0f;
    float f = 1.0f;
    float a = 1.0f;
    for (int n = 0; n < octaves; ++n) {
        *p += a * noise (perms, grads, (float)x*f*s, (float)y*f*s);
        f *= lacunarity;
        a *= gain;
    }
    *p /= fbm_max;
}

// Host Code

uchar  h_perms [TABLE_SIZE];
float2 h_grads [TABLE_SIZE];

uchar*  d_perms;
float2* d_grads;

void setup (int seed)
{
    srand (seed);

    // Construct a random permutaion table of values uniformly distributed
    // in the 0..255 range.
    for (int i = 0; i < TABLE_SIZE; ++i) h_perms[i] = i;
    for (int i = 0; i < TABLE_SIZE; ++i) {
        uchar j = rand ();
        h_perms[i] ^= h_perms[j] ^= h_perms[i];
    }

    // Construct a random gradient table of values uniformly distributed
    // along the unit circle.
    float step = 2*M_PI / (float) TABLE_SIZE;
    float angle = 0.0f;
    for (int i = 0; i < TABLE_SIZE; ++i, angle += step) {
        h_grads[i].x = cos (angle);
        h_grads[i].y = sin (angle);
    }

    // Copy to device memory.
    cudaMalloc ((void**)&d_perms, TABLE_SIZE);
    cudaMemcpy (d_perms, h_perms, TABLE_SIZE, cudaMemcpyHostToDevice);
    cudaMalloc ((void**)&d_grads, TABLE_SIZE*sizeof(float2));
    cudaMemcpy (d_grads, h_grads, TABLE_SIZE*sizeof(float2), cudaMemcpyHostToDevice);
}

void clean ()
{
    cudaFree (d_perms);
    cudaFree (d_grads);
}

void noise (int seed, float* h_image, int w, int h, int cell_size, int freq)
{
    setup (seed);

    unsigned n = (unsigned) w * (unsigned) h;
    unsigned nf = n * sizeof(float);
    float* d_image;
    cudaMalloc ((void**)&d_image, nf);

    const float s = (float) freq / (float) cell_size;
    unsigned tpb = 1024;
    unsigned nb = n / tpb;
    kernel_noise<<<nb,tpb>>>(d_perms, d_grads, s, w, h, d_image);

    cudaMemcpy (h_image, d_image, nf, cudaMemcpyDeviceToHost);
    cudaFree (h_image);

    clean();
}

float geom (float r, float n)
{
    return (1.0f - pow(r,n)) / (1.0f - r);
}

void fbm (int seed, float* h_image, int w, int h, int cell_size, float lacunarity, float H, int octaves)
{
    setup (seed);

    unsigned n = (unsigned) w * (unsigned) h;
    unsigned nf = n * sizeof(float);
    float* d_image;
    cudaMalloc ((void**)&d_image, nf);

    const float s = 1.0f / (float) cell_size;
    const float gain = pow (lacunarity, -2*H);
    const float fbm_max = gain == 1.0f ? 1.0f : geom (gain, octaves);
    unsigned tpb = 1024;
    unsigned nb = n / tpb;
    kernel_fbm<<<nb,tpb>>>(d_perms, d_grads, octaves, lacunarity, gain, fbm_max, s, w, h, d_image);

    cudaMemcpy (h_image, d_image, nf, cudaMemcpyDeviceToHost);
    cudaFree (h_image);

    clean();
}
