#version 400

uniform mat4 ModelView;
uniform mat4 Projection;
uniform mat3 NormalMat;

uniform sampler2D heightmap;

layout (location = 0) in vec2 VertexPosition;

out vec3 Position;
out vec3 Normal;
out float ambient;

void main ()
{
	float y = texture (heightmap, VertexPosition).r;
	vec4 v = ModelView * vec4 (VertexPosition.x, y, -VertexPosition.y, 1.0);

        vec2 dim = textureSize (heightmap, 0);
        vec2 xoff = vec2 (1.0 / dim.x, 0);
        vec2 yoff = vec2 (0, 1.0 / dim.y);

        vec2 right = VertexPosition + xoff;
        vec2 left  = VertexPosition - xoff;
        vec2 up    = VertexPosition + yoff;
        vec2 down  = VertexPosition - yoff;

        right.x = min (right.x, dim.x-1);
        left.x  = max (left.x, 0);
        up.y    = min (up.y, dim.y-1);
        down.y  = max (down.y, 0);

        float hright = texture (heightmap, right).r;
        float hleft = texture (heightmap, left).r;
        float hup = texture (heightmap, up).r;
        float hdown = texture (heightmap, down).r;

        vec3 s = vec3 (1, 0, hright - hleft);
        vec3 t = vec3 (0, 1, hup - hdown);

	Position = vec3 (v);
	Normal = NormalMat * normalize(cross (s,t));
        ambient = hright <= y ? 0.25 : 0.0;
        ambient += (hleft <= y ? 0.25 : 0.0);
        ambient += (hup <= y ? 0.25 : 0.0);
        ambient += hdown <= y ? 0.25 : 0.0;

	gl_Position = Projection * v;
}

