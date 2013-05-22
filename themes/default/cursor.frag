#define PI 3.141592653
uniform float cursorx;
uniform float cursory;
uniform float time;

void main(void) 
{
    vec2 v = gl_TexCoord[0].xy * PI;
    float cols = 80.0;
    float rows = 24.0;
    gl_FragColor[1] = 1.0;
    gl_FragColor[2] = 0.25*min(1.0,1.0/abs(1.5*( ((cols-cursorx+0.5)-gl_TexCoord[0].x*cols) )))
    +0.25*min(1.0,1.0/abs(3.0*( ((rows-cursory+0.5)-gl_TexCoord[0].y*rows) )));
    float cursor =  (sin(time*10.0)/2.0+0.5)*10.0*(min(1.0,max(0.0,-0.4+0.25*min(1.0,1.0/abs(2.7*( ((cols-cursorx+0.5)-gl_TexCoord[0].x*cols) )))
    +0.25*min(1.0,1.0/abs(3.0*( ((rows-cursory+0.5)-gl_TexCoord[0].y*rows) ))))));

    gl_FragColor[3] = gl_FragColor[2]*0.5 + cursor;
}
