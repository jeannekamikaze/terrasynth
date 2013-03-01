#pragma once

void noise (int seed, float* image, int w, int h, int cell_size = 64, int freq = 1);

void fbm (int seed, float* image, int w, int h, int cell_size = 64, float lacunarity = 2.0f, float H = 2.0f, int octaves = 1);
