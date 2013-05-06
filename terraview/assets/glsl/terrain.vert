#version 400

uniform mat4 ModelView;
uniform mat4 Projection;
uniform mat3 NormalMat;

uniform sampler2D heightmap;

layout (location = 0) in vec2 VertexPosition;
layout (location = 1) in vec3 VertexNormal;

out vec3 Position;
out vec3 Normal;

void main ()
{
	float y = texture (heightmap, VertexPosition).r;
	vec4 v = ModelView * vec4 (VertexPosition.x, y, -VertexPosition.y, 1.0);

    Position = vec3 (v);
	Normal = NormalMat * VertexNormal;

	gl_Position = Projection * v;
}

