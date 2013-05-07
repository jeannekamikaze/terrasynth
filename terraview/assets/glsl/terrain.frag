#version 400

struct Light
{
    vec3 dir;
    vec3 La;
    vec3 Ld;
};

struct Material
{
    vec3 Ka;
    vec3 Kd;
};

uniform sampler2D grass;
uniform sampler2D soil;

uniform Light light;
uniform Material mat;

in vec3 Position;
in vec3 Normal;
in vec2 TexCoord;
in float Height;

layout (location = 0) out vec4 FragColor;

void main ()
{
    vec3 N = normalize (Normal);

    vec3 colour
        = mat.Ka * light.La
        + mat.Kd * light.Ld * max(dot(N,light.dir), 0.0);

    vec4 c_grass = texture (grass, TexCoord);
    vec4 c_soil  = texture (soil,  TexCoord);
    vec4 c_tex   = mix (c_grass, c_soil, Height);

    FragColor = vec4 (colour, 1.0) * c_tex;
}
