#define PI 3.141592653
uniform float cursorx;
uniform float cursory;
uniform float time;

void main(void) 
{
    vec2 v = gl_TexCoord[0].xy * PI;
    float cols = 80.0;
    float rows = 24.0;
    float intens = abs(sin(abs(v.y))*sin(abs(v.x))*0.12*sin(time)+0.2);
    gl_FragColor = intens * gl_Color;
    gl_FragColor[1] -= 1.0 * max(pow(sin((v.x-0.15)*2.0*cols),2.0),-0.15);
    gl_FragColor[1] -= 1.0 * max(pow(sin((v.y-0.226)*2.0*rows),2.0),-0.15);
}
