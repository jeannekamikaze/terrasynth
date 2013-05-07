#pragma once

#include <OGDT/Application.h>

class HmApp : public OGDT::Application
{
    struct _impl;
    _impl* impl;

    void onUpdate (float dt);

    HmApp (const HmApp&);
    HmApp& operator= (const HmApp&);

    void updateCamera ();

    void setHeightMap (const char *path);

public:

    HmApp (const char* height_map_path, int w = 800, int h = 600);

    ~HmApp ();

    void onResize (int width, int height);

    void update (float dt);

    void onKey (OGDT::Input::code key, bool pressed);

    void onMouseButton (OGDT::Input::code button, bool pressed);

    void onMouseMove (int x, int y);

    void onMouseWheel (int pos);
};
