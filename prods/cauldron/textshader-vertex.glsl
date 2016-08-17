
uniform vec2       iTopLeftV;

varying vec3 N, V;

void main(void)
{
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    vec4 vTopLeft = vec4(iTopLeftV.x, iTopLeftV.y, 0.0, 0.0);
    vec4 result = gl_ModelViewProjectionMatrix * vTopLeft;
    V = result.xyz;
    //V = gl_ModelViewProjectionMatrix * vTopLeft;
    //V = gl_Vertex.xyz;
    N = gl_NormalMatrix * gl_Normal;
}

