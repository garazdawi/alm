-module(mandelbrot).

-export([mandelbrot/3]).

mandelbrot(Width,Height,Iters) ->
    mandelbrot_start(Width/2,Height/2,Iters).

mandelbrot_calc(X,Y,Z,ZI,Iters,I) ->
    if (((Z*Z)+(ZI*ZI)) > 4) -> I;
       (Iters == I) -> 0;
       true -> mandelbrot_calc(X,Y,(Z*Z)-(ZI*ZI) + X,2*Z*ZI + Y,Iters,I+1)
    end.

mandelbrot_r(X,Y,Yend,Iters,Rows) ->
    if (Y == Yend) -> Rows;
       true -> 
	    mandelbrot_r(X,Y-1,Yend,Iters,
			 [mandelbrot_calc(X,Y,0,0,Iters,0)|Rows])
    end.

mandelbrot_c(X,Xend,Ystart,Yend,Iters,Cols) ->
    if (X == Xend) -> Cols;
       true -> 
	    mandelbrot_c(X-1,Xend,Ystart,Yend,Iters,
			 [mandelbrot_r(X,Ystart,Yend,Iters,[])|Cols])
    end.

mandelbrot_start(Xstart, Ystart, Iters) ->
    mandelbrot_c(Xstart,-1*Xstart,Ystart,-1*Ystart,Iters,[]).

