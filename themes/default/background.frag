#define PI 3.141592653
uniform float cursorx;
uniform float cursory;
uniform float time;

void main(void) 
{
    vec2 v = gl_TexCoord[0].xy * PI;
    float intens = abs(sin(abs(v.y))*sin(abs(v.x))*2.0*sin(time));
    gl_FragColor = intens * gl_Color;
    gl_FragColor[2] = 0.25*min(1.0,1.0/abs(1.5*( ((80.0-cursorx+0.5)-gl_TexCoord[0].x*80.0) )))
    +0.25*min(1.0,1.0/abs(3.0*( ((25.0-cursory+0.5)-gl_TexCoord[0].y*25.0) )));
    float cursor =  (sin(time*3.0)/7.0+0.6)*10.0*(min(1.0,max(0.0,-0.4+0.25*min(1.0,1.0/abs(1.5*( ((80.0-cursorx+0.5)-gl_TexCoord[0].x*80.0) )))
    +0.25*min(1.0,1.0/abs(3.0*( ((25.0-cursory+0.5)-gl_TexCoord[0].y*25.0) ))))));

    gl_FragColor[1] += cursor;
   }
