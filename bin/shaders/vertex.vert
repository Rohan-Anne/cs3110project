#version 330 core

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 color;

out vec3 v_color;

void main() {
    v_color = color;
    gl_Position = vec4(vertex, 1.0);
}
