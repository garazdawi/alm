# Step 1

Basic version with constants only and basic arithmetic

code: add.alm

    def add() {
        1+2*3/(5+91);
    }

cmd:

    $ alc add.alm
    $ alm add
    1.0625

# Step 2

Add variales to the mix

code: add2.alm

    def add2(a,b) {
        a+b;
    }

cmd:

    $ alc add2.alm
    $ alm add2(1.5,3)
    4.5

# Step 3

add if statments

code: mul.alm

    def add3(a,b) {
        if (b == 0) a;
        else b + add3(a,b-1);
    }

    def mul(a,b) {
        add3(a,b);
    }

cmd:

    $ alc mul.alm
    $ alm mul(5,6)
    30.0

# Step 4

Add a new datatype called list!

code: seq.alm

    def list(n) {
        if (n == 0) [];
        else [n|list(n-1)];
    }

cmd:

    $ alc seq.alm
    $ alm list(5)
    [5,4,3,2,1]

# Step 5

Implement a mandelbrot set calculation

code: mandelbrot.alm

    def mandelbrot(x,y,z,zi,iters,i) {
	if (((z*z)+(zi*zi)) > 4) i;
	else if(iters == i) 0;
	else mandelbrot(x,y,(z*z)-(zi*zi) + x,2*z*zi + y,iters,i+1);
    }

    def mandelbrot_r(x,y,yend,iters,rows) {
	if (y == yend) rows;
	else mandelbrot_c(x,y,yend,
		[mandelbrot_calc(x,y,0,0,iters,0)|rows]);
    }

    def mandelbrot_c(x,xend,ystart,yend,iters,cols) {
	if (x == xend) rows;
	else mandelbrot_c(x-1,xend,ystart,yend,
		[mandelbrot_r(x,ystart,yend,iters,[])|cols]);
    }

    def mandelbrot(width,height,iters) {
        xstart = widht/2;
	ystart = height/2;
	set = mandelbrot_c(xstart,-xstart,ystart,-ystart,iters,[]);
    }


cmd:

    # alc mandelbrot.alm
    # alm mandelbrot(400,400,200)
    "mandelbrot.tiff"


# Step 6

Add a tracing copy collect GC
