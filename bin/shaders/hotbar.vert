#version 330 core

layout(location = 0) in vec2 pos;
layout(location = 1) in vec3 color;

out vec3 v_color;

uniform float aspect;

void main() {
    gl_Position = vec4(pos.x / aspect, pos.y, 0.0, 1.0);
    v_color = color;
}
