-- praxis:


shadprog,shadres = glCreateProgram(
[[
// Generic vertex transformation,
// copy object-space position and 
// lighting vectors out to interpolants

uniform vec3 lightPos;

varying vec3 N, L, V;

void main(void)
{ 
    // normal MVP transform
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

    // map object-space position onto unit sphere
    V = gl_Vertex.xyz;

    // eye-space normal
    N = gl_NormalMatrix * gl_Normal;

    // eye-space light vector
    vec4 Veye = gl_ModelViewMatrix * gl_Vertex;
    L = lightPos - Veye.xyz;
}
]],

[[

// Star Nest by Pablo Roman Andrioli
// https://www.shadertoy.com/view/XlfGRj
// This content is under the MIT License.

// precision highp float;

vec3 iResolution = vec3(1,1,1);
float iGlobalTime = 10.0;
uniform float iChannelTime[4];
vec4 iMouse = vec4(1,1,1,1);
uniform vec4 iDate;
uniform float iSampleRate;
uniform vec3 iChannelResolution[4];
uniform sampler2D iChannel0;
uniform sampler2D iChannel1;
uniform sampler2D iChannel2;
uniform sampler2D iChannel3;

#define iterations 17
#define formuparam 0.53

#define volsteps 20
#define stepsize 0.1

#define zoom   0.800
#define tile   0.850
#define speed  0.010 

#define brightness 0.0015
#define darkmatter 0.300
#define distfading 0.730
#define saturation 0.850


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  //get coords and direction
  vec2 uv=fragCoord.xy/iResolution.xy-.5;
  uv.y*=iResolution.y/iResolution.x;
  vec3 dir=vec3(uv*zoom,1.);
  float time=iGlobalTime*speed+.25;

  //mouse rotation
  float a1=.5+iMouse.x/iResolution.x*2.;
  float a2=.8+iMouse.y/iResolution.y*2.;
  mat2 rot1=mat2(cos(a1),sin(a1),-sin(a1),cos(a1));
  mat2 rot2=mat2(cos(a2),sin(a2),-sin(a2),cos(a2));
  dir.xz*=rot1;
  dir.xy*=rot2;
  vec3 from=vec3(1.,.5,0.5);
  from+=vec3(time*2.,time,-2.);
  from.xz*=rot1;
  from.xy*=rot2;
  
  //volumetric rendering
  float s=0.1,fade=1.;
  vec3 v=vec3(0.);
  for (int r=0; r<volsteps; r++) {
    vec3 p=from+s*dir*.5;
    p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
    float pa,a=pa=0.;
    for (int i=0; i<iterations; i++) { 
      p=abs(p)/dot(p,p)-formuparam; // the magic formula
      a+=abs(length(p)-pa); // absolute sum of average change
      pa=length(p);
    }
    float dm=max(0.,darkmatter-a*a*.001); //dark matter
    a*=a*a; // add contrast
    if (r>6) fade*=1.-dm; // dark matter, don't render near
    //v+=vec3(dm,dm*.5,0.);
    v+=fade;
    v+=vec3(s,s*s,s*s*s*s)*a*brightness*fade; // coloring based on distance
    fade*=distfading; // distance fading
    s+=stepsize;
  }
  v=mix(vec3(length(v)),v,saturation); //color adjust
  fragColor = vec4(v*.01,1.);  
  
}

void main(){mainImage(gl_FragColor,gl_FragCoord.xy);}
]])


