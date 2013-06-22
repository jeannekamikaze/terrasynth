#pragma once

#include <OGDT/Image.hpp>

class HeightMap
{
    struct _impl;
    _impl* impl;

    HeightMap (const HeightMap&);
    HeightMap& operator= (const HeightMap&);

public:

    HeightMap (const OGDT::TImage<U8,1>&);
    
    ~HeightMap ();

    void render () const;
};
