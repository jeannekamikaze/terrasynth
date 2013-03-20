#pragma once

#include <OGDT/Image.h>

class HeightMap
{
    struct _impl;
    _impl* impl;
    
    HeightMap (const HeightMap&);
    HeightMap& operator= (const HeightMap&);
    
public:
    
    HeightMap (const OGDT::TImage<U8,1>&);

    HeightMap (const OGDT::TImage<float,1>&);
    
    ~HeightMap ();
    
    void render () const;
};
