#include "HmApp.h"
#include "HeightMap.h"
#include <OGDT/gl.h>
#include <OGDT/gl_utils.hpp>
#include <OGDT/Image.hpp>
#include <OGDT/math/Camera.h>
#include <OGDT/math/vec3.h>
#include <cstdio>
#include <cmath>

using namespace OGDT;

const vec3 L = vec3 (1,1,1);

float clamp (float lower, float x, float upper)
{
    x = fmax (lower, x);
    return fmin (upper, x);
}

void update_light (GLint prog, GLint light_dir, vec3 L, const mat4& modelview)
{
    L = normalise(transform (modelview, L, 0));
    glUseProgram (prog);
    glUniform3f (light_dir, L.x, L.y, L.z);
    glUseProgram (0);

}

void update_modelview (GLint prog, GLint modelview, GLint normalmat, const mat4& mat)
{
    glUseProgram (prog);
    glUniformMatrix4fv (modelview, 1, GL_FALSE, mat);
    glUniformMatrix3fv (normalmat, 1, GL_FALSE, transpose(inverse(mat.to33())));
    glUseProgram (0);
}

void update_projection (GLint prog, GLint proj, const Camera& cam)
{
    glUseProgram (prog);
    glUniformMatrix4fv (proj, 1, GL_FALSE, cam.projection());
    glUseProgram (0);
}

struct Light
{
	GLint dir;
	GLint La;
	GLint Ld;
};

struct Material
{
	GLint Ka;
	GLint Kd;
};

struct Program
{
    GLint id;
    GLint modelview;
    GLint projection;
    GLint normalmat;
    GLint heightmap;
    Light light;
    Material mat;

    Program () : id (0) {}

    ~Program ()
    {
        if (id) glDeleteProgram (id);
    }

private:

    Program (const Program&);
    Program& operator= (const Program&);
};


struct HmApp::_impl
{
    HeightMap* heightmap;
    GLuint heightmap_tex;

    Program prog;

    Camera cam;
    float aspect;
    float far;
    float radius;

    vec3 center;
    mat4 scale;

    bool dragging;
    int xmouse, ymouse;
    int dx, dy;
    int wheel;
    int wheel_delta;
    float azimuth, zenith;

    bool wireframe;

    _impl ()
        : heightmap (nullptr), heightmap_tex (0), dragging (false), wireframe (false) {}
};

HmApp::HmApp (const char* height_map_path, int w, int h)
    : Application (w, h, "Heightmap Viewer 0.1", 4, 2), impl (new _impl)
{
    glGenTextures (1, &impl->heightmap_tex);
    setHeightMap (height_map_path);

    Program& prog = impl->prog;
    prog.id = create_program_from_files ("assets/glsl/terrain.vert", "assets/glsl/terrain.frag");
    prog.modelview  = get_uniform (prog.id, "ModelView");
    prog.projection = get_uniform (prog.id, "Projection");
    prog.normalmat  = get_uniform (prog.id, "NormalMat");
    prog.heightmap  = get_uniform (prog.id, "heightmap");
    prog.light.dir  = get_uniform (prog.id, "light.dir");
    prog.light.La   = get_uniform (prog.id, "light.La");
    prog.light.Ld   = get_uniform (prog.id, "light.Ld");
    prog.mat.Ka     = get_uniform (prog.id, "mat.Ka");
    prog.mat.Kd     = get_uniform (prog.id, "mat.Kd");

    vec3 L (1.0f, 1.0f, 1.0f);
    L.normalise();

    glUseProgram (prog.id);
    glUniform1i (prog.heightmap, 0); // Texture unit 0
    glUniform3f (prog.light.dir, L.x, L.y, L.z);
    glUniform3f (prog.light.La, 0.3f, 0.3f, 0.3f);
    glUniform3f (prog.light.Ld, 0.7f, 0.7f, 0.7f);
    glUniform3f (prog.mat.Ka, 1.0f, 1.0f, 1.0f);
    glUniform3f (prog.mat.Kd, 1.0f, 1.0f, 1.0f);
    glUseProgram (0);

    glClearColor (0.6f, 0.7f, 0.6f, 1.0f);
    glEnable (GL_DEPTH_TEST);

    impl->wheel = getInput().getMouseWheel();
}

HmApp::~HmApp ()
{
    if (impl->heightmap) delete impl->heightmap;
    if (impl->heightmap_tex) glDeleteTextures (1, &impl->heightmap_tex);
    delete impl;
}

void HmApp::onResize (int width, int height)
{
    impl->aspect = (float) width / (float) height;
    glViewport (0, 0, width, height);
    impl->cam = Camera (45.0f, impl->aspect, 1.0f, impl->far);
    update_projection (impl->prog.id, impl->prog.projection, impl->cam);
}

void HmApp::update (float dt)
{
    pollInput ();

    bool rotate = impl->dragging && (impl->dx != 0 || impl->dy != 0);
    bool zoom = impl->wheel_delta != 0;

    if (zoom)
    {
        const float offset = impl->wheel_delta * norm (impl->center - impl->cam.pos()) * 0.1;
        impl->radius += offset;
        impl->wheel_delta = 0;
    }

    if (rotate)
    {
        static const float speed = 20.0f;
        impl->azimuth += -impl->dx * dt * speed;
        impl->zenith += impl->dy * dt * speed;
        impl->zenith = clamp (-85.0f, impl->zenith, 85.0f);
        impl->dx = 0;
        impl->dy = 0;
    }

    if (zoom || rotate)
    {
        Camera& cam = impl->cam;
        cam.orbit (impl->center, impl->radius, impl->azimuth, impl->zenith);
        cam.lookAt (impl->center);
        mat4 modelview = cam.inverseTransform() * impl->scale;
        update_light (impl->prog.id, impl->prog.light.dir, L, modelview);
        update_modelview (impl->prog.id, impl->prog.modelview,
                          impl->prog.normalmat, modelview);
    }

    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    if (impl->wireframe) glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
    glUseProgram (impl->prog.id);
    glActiveTexture (GL_TEXTURE0);
    glBindTexture (GL_TEXTURE_2D, impl->heightmap_tex);
    impl->heightmap->render ();
    glBindTexture (GL_TEXTURE_2D, 0);
    glUseProgram (0);
    if (impl->wireframe) glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);

    swapBuffers ();
}

void HmApp::onKey (OGDT::Input::code key, bool pressed)
{
    if (key == OGDT::Input::Esc) quit();
    else if (key == OGDT::Input::KeyF && pressed)
    {
        impl->wireframe = !impl->wireframe;
    }
}

void HmApp::onMouseButton (Input::code button, bool pressed)
{
    impl->dragging = pressed;
    getInput().getMousePosition (impl->xmouse, impl->ymouse);
    impl->dx = 0;
    impl->dy = 0;
}

void HmApp::onMouseMove (int x, int y)
{
    impl->dx = x - impl->xmouse;
    impl->dy = y - impl->ymouse;
    impl->xmouse = x;
    impl->ymouse = y;
}

void HmApp::onMouseWheel (int pos)
{
    impl->wheel_delta = impl->wheel - pos;
    impl->wheel = pos;
}

void HmApp::setHeightMap (const char *path)
{
    Image image;
    Image::from_file (path, image);
    int w = image.width();
    int h = image.height();

    if (impl->heightmap) delete impl->heightmap;
    impl->heightmap = new HeightMap (image.coerce<U8,1>());

    image.flipVertically();
    glBindTexture (GL_TEXTURE_2D, impl->heightmap_tex);
    glTexImage2D (GL_TEXTURE_2D, 0, GL_R8, w, h, 0, GL_RED, GL_UNSIGNED_BYTE, image);
    glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture (GL_TEXTURE_2D, 0);

    float wf = (float) w;
    float hf = (float) h;
    float xcenter =  wf / 2.0f;
    float zcenter = -hf / 2.0f;
    float height = sqrt (wf*wf + hf*hf) / 4.0f;
    impl->far = sqrt (wf*wf + hf*hf) * 2.5;
    impl->radius = impl->far / 2.0f;
    impl->scale = mat4::scale (wf, height, hf);
    impl->center = vec3 (xcenter, height, zcenter);
    impl->azimuth = 0.0f;
    impl->zenith = 30.0f;

    Camera& cam = impl->cam;
    cam = Camera (45.0f, impl->aspect, 1.0f, impl->far);
    cam.orbit (xcenter, height, zcenter, impl->radius, impl->azimuth, impl->zenith);
    cam.lookAt (xcenter, height, zcenter);
    mat4 modelview = cam.inverseTransform() * impl->scale;
    update_light (impl->prog.id, impl->prog.light.dir, L, modelview);
    update_modelview (impl->prog.id, impl->prog.modelview, impl->prog.normalmat, modelview);
    update_projection (impl->prog.id, impl->prog.projection, cam);

    printf ("Scene center: %f, %f, %f\n", xcenter, height, zcenter);
}
