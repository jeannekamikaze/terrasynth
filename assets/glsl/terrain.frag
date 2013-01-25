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

uniform Light light;
uniform Material mat;

in vec3 Position;
in vec3 Normal;

layout (location = 0) out vec4 FragColor;

void main ()
{
	vec3 N = normalize (Normal);

	vec3 colour
             = mat.Ka * light.La
             + mat.Kd * light.Ld * max(dot(N,light.dir), 0.0);

	FragColor = vec4 (colour, 1.0);
}
