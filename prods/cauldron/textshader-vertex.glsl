
varying vec3 N, V;

void main(void)
{ 
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    V = gl_Vertex.xyz;
    N = gl_NormalMatrix * gl_Normal;
}
