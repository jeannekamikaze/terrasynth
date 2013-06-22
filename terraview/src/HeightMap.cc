#include "HeightMap.h"
#include <OGDT/gl.h>
#include <OGDT/Image.h>
#include <OGDT/math/vec3.h>
#include <cstdio>
#include <cmath>

using namespace OGDT;

struct vert
{
    float x, z;
    vec3 n;
};

struct HeightMap::_impl
{
    GLuint verts;
    GLuint vao;
    int across;
    int rows;
};

template <class T>
void compute_positions (const OGDT::TImage<T,1>& image, vert* verts)
{
    float wf = (float) width;
    float hf = (float) height;
    float xstart  = 1.0f / wf / 2.0f;
    float zstart  = 1.0f / hf / 2.0f;
    float xstride = 1.0f / wf;
    float zstride = 1.0f / hf;
    int i = 0;
    float zf = zstart;

    for (int z = 0; z < height-1; ++z, zf+=zstride)
    {
        float xf = xstart;
        for (int x = 0; x < width; ++x, i+=2, xf+=xstride)
        {
            verts[i].x = xf;
            verts[i].z = zf+zstride;
            verts[i+1].x = xf;
            verts[i+1].z = zf;
        }
    }
}

template <class T>
void compute_normals (const OGDT::TImage<T,1>& image, vert* verts, int n)
{
    // Compute smooth normals
    
    int i = 0;

    for (int z = 0; z < height-1; ++z)
    {
        for (int x = 0; x < width; ++x, i+=2)
        {
            if (x < width-1)
            {
                vec3 p1 (0,       (float) image (height-z-1,x)   / 255.0f, 0); // Bot
                vec3 p2 (0,       (float) image (height-z-2,x)   / 255.0f, -zstride); // Top
                vec3 p3 (xstride, (float) image (height-z-2,x+1) / 255.0f, -zstride); // Top right
                vec3 N = cross (p3-p1, p2-p1);

                // Top
                verts[i].n += N;

                // Bot
                verts[i+1].n += N;

                // Top right
                verts[i+2].n += N;

                if (z < height-2)
                {
                    // Bot on upper row.
                    verts[across + i+1].n += N;

                    // Bot right on upper row.
                    verts[across + i+3].n += N;
                }

                if (z > 0)
                {
                    // Top on lower row.
                    verts[i - across].n += N;
                }
            }
            if (x > 0)
            {
                vec3 p1 (0,        (float) image (height-z-1,x) / 255.0f,   0); // Bot
                vec3 p2 (0,        (float) image (height-z-2,x) / 255.0f,   -zstride); // Top
                vec3 p3 (-xstride, (float) image (height-z-1,x-1) / 255.0f, 0); // Bot left
                vec3 N = cross (p2-p1, p3-p1);

                // Top
                verts[i].n += N;

                // Bot
                verts[i+1].n += N;

                // Bot left
                verts[i-1].n += N;

                if (z < height-2)
                {
                    // Bot on upper row.
                    verts[across + i+1].n += N;
                }

                if (z > 0)
                {
                    // Top on lower row.
                    verts[i - across].n += N;

                    // Top left on lower row.
                    verts[i-1 - across].n += N;
                }
            }
        }
    }

    // Normalise normals.

    i = 0;

    for (int i = 0; i < n; ++i)
    {
        verts[i].n.normalise();
    }
}

HeightMap::HeightMap (const OGDT::TImage<U8,1>& image) : impl (new _impl)
{
    int width = image.width();
    int height = image.height();
    int across = width*2; // Number of vertices across.
    int rows = height-1; // Number of rows.
    int n = across * rows; // Total number of verts.
    vert* verts = new vert[n];

    compute_normals (image, verts, n);

    glGenVertexArrays (1, &impl->vao);
    glBindVertexArray (impl->vao);

    glGenBuffers (1, &impl->verts);
    glBindBuffer (GL_ARRAY_BUFFER, impl->verts);
    glBufferData (GL_ARRAY_BUFFER, n*sizeof(vert), verts, GL_STATIC_DRAW);
    glEnableVertexAttribArray (0);
    glVertexAttribPointer (0, 2, GL_FLOAT, GL_FALSE, sizeof(vert), 0);
    glEnableVertexAttribArray (1);
    glVertexAttribPointer (1, 3, GL_FLOAT, GL_FALSE, sizeof(vert), (GLvoid*) (2*sizeof(float)));
    glBindBuffer (GL_ARRAY_BUFFER, 0);

    glBindVertexArray (0);

    delete[] verts;

    impl->across = across;
    impl->rows = rows;
}

HeightMap (const OGDT::TImage<float,1>& image) : impl (new _impl)
{
}

HeightMap::~HeightMap ()
{
    glDeleteVertexArrays (1, &impl->vao);
    glDeleteBuffers (1, &impl->verts);
    delete impl;
}

void HeightMap::render () const
{
    int n = impl->across;
    int off = 0;
    glBindVertexArray (impl->vao);
    for (int i = 0; i < impl->rows; ++i, off+=n)
    {
        glDrawArrays (GL_TRIANGLE_STRIP, off, n);
    }
    glBindVertexArray (0);
}
