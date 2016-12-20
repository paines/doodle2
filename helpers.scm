
(import chicken scheme)
  (use (srfi 1 4 18) bind)
(bind-rename "getTimestamp" "get-timestamp")
(bind* #<<EOF
char* getTimestamp();
#ifndef CHICKEN
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <strings.h>
#endif

char* getTimestamp() {
                      char *buffer = (char*)malloc(32);
                      time_t t = time(0);
                      struct tm *tm;

                      tm = gmtime(&t);
                      strftime(buffer, 31, "%Y%m%d-%H%M%S", tm);
                      return buffer;
                     }

EOF
)


(bind-rename "noise2d" "noise2d")
(bind* #<<EOF

double findnoise2(double x,double y)
{
 int n=(int)x+(int)y*57;
 n=(n<<13)^n;
 int nn=(n*(n*n*60493+19990303)+1376312589)&0x7fffffff;
 return 1.0-((double)nn/1073741824.0);
}


double interpolate(double a,double b,double x)
{
 double ft=x * 3.1415927;
 double f=(1.0-cos(ft))* 0.5;
 return a*(1.0-f)+b*f;
}

double noise2d(double x,double y)
{
 double floorx=(double)((int)x);//This is kinda a cheap way to floor a double integer.
 double floory=(double)((int)y);
 double s,t,u,v;//Integer declaration
 s=findnoise2(floorx,floory); 
 t=findnoise2(floorx+1,floory);
 u=findnoise2(floorx,floory+1);//Get the surrounding pixels to calculate the transition.
 v=findnoise2(floorx+1,floory+1);
 double int1=interpolate(s,t,x-floorx);//Interpolate between the values.
 double int2=interpolate(u,v,x-floorx);//Here we use x-floorx, to get 1st dimension. Don't mind the x-floorx thingie, it's part of the cosine formula.
 return interpolate(int1,int2,y-floory);//Here we use y-floory, to get the 2nd dimension.
}


EOF
)
