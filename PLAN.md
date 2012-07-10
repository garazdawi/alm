# Step 1

Basic version with constants only and basic arithmetic

code: add.alm

    def add() {
        1+2*3/(5+91);
    }

asm: add.asm

    {func, add, 0}
      {label, add_0}
      {load, 5, x(0)}
      {load, 91, x(1)}
      {add, x(0), x(1), x(2)}
      {load, 3, x(3)}
      {div, x(3), x(2), x(4)}
      {load, 2, x(5)}
      {mul, x(5), x(4), x(6)}
      {load, 1, x(7)}
      {add, x(7), x(6), x(8)}
      {move, x(8), x(0)}
      {return}

cmd:

    $ alc add.alm
    $ alm add
    1.0625

# Step 2

Add variales to the mix!

code: add2.alm

    def add2(a,b) {
        a+b;
    }

asm: add2.asm

    {func, add2, 2}
    {label, add2_2}
      {add, x(0), x(1), x(2)}
      {move, x(2), x(0)}
      {return}

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

asm: mul.asm

    {func,add3,2}
    {label, add3_2}
      {load, 0, x(2)}
      {eq, x(1), x(2), x(3)}
      {brt, x(3), add3_false}
      {return}
    {label, add3_2_false}
      {load, 1, x(2)}
      {sub, x(1), x(2), x(3)}
      {move, x(1), y(0)}
      {move, x(3), x(1)}
      {call, add_3,2,1}
      {add, x(0), y(0), x(0)}
      {return}

    {func,mul,2}
      {call,add3,2,0}
      {return}

cmd:

    $ alc mul.alm
    $ alm mul(5,6)
    30.0

# Step 4

Add a new datatype called list! Also we now have to add typechecks when doing arithmetic

code: seq.alm

    def list(n) {
        if (n == 0) [];
        else [n|list(n-1)];
    }

asm: seq.asm

    {func, list, 1}
    {label, list_1}
      {load, 0, x(1)}
      {eq, x(0), x(1), x(2)}
      {brt, x(2), list_1_false}
      {move, nil, x(0)}
      {return}
    {label, list_1_false}
      {is_number, x(0), x(1)}
      {brt, x(1), list_1_error}
      {load, 1, x(1)}
      {sub, x(0), x(1), x(2)}
      {move, x(0), y(0)}
      {move, x(2), x(0)}
      {call, list, 1, 1}
      {cons, x(0), y(0), x(0)}
      {return}
    {label, list_1_error}
      {throw, error}

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
	else mandelbrot_r(x,y-1,yend,iters,
		[mandelbrot(x,y,0,0,iters,0)|rows]);
    }

    def mandelbrot_c(x,xend,ystart,yend,iters,cols) {
	if (x == xend) cols;
	else mandelbrot_c(x-1,xend,ystart,yend,iters,
		[mandelbrot_r(x,ystart,yend,iters,[])|cols]);
    }

    def mandelbrot(width,height,iters) {
        xstart = widht/2;
	ystart = height/2;
	mandelbrot_c(xstart,-1*xstart,ystart,-1*ystart,iters,[]);
    }

asm: mandelbrot.asm

Note! For brevity I've not included the is_number tests which have to be before each arithmentic operation.

    {func, mandelbrot, 6}
    {label, mandelbrot_6}
      ;; if (((z*z)+(zi*zi)) > 4)
      {mul, x(2), x(2), x(6)}
      {mul, x(3), x(3), x(7)}
      {add, x(6), x(7), x(8)}
      {load, 4, x(9)}
      {gt, x(8), x(9), x(10)}
      {brt, x(10), mandelbrot_6_false}
      ;; return i
      {move, x(5), x(0)}
      {return}
    {label, mandelbrot_6_false}
      ;; if (iters == i)
      {eq, x(4), x(5), x(10)}
      {brt, x(10), mandelbrot_6_false_2}
      {load, 0, x(0)}
      {return}
    {label, mandelbrot_6_false_2}
      ;; (z * z) - (zi * zi) + x
      {mul, x(2), x(2), x(11)}
      {mul, x(3), x(3), x(12)}
      {sub, x(11), x(12), x(13)}
      {add, x(13), x(0), x(14)}
      ;; 2 * z * zi + y
      {load, 2, x(15)}
      {mul, x(15), x(2), x(16)}
      {mul, x(16), x(3), x(17)}
      {add, x(17), x(1), x(18)}
      ;; i + 1
      {load, 1, x(19)}
      {add, x(5), x(19), x(20)}
      {move, x(14), x(2)}
      {move, x(18), x(3)}
      {move, x(20), x(5)}
      {call, mandelbrot, 6, 0}
      {return}

    {func, mandelbrot_r, 5}
    {label, mandelbrot_r_5}
      ;; if (y == yend)
      {eq, x(1), x(2), x(5)}
      {brt, x(5), mandelbrot_r_5_false}
      {move, x(4), x(0)}
      {return}
    {label, mandelbrot_r_5_false}
      {move, x(0), y(0)}
      {move, x(1), y(1)}
      {move, x(2), y(2)}
      {move, x(3), y(3)}
      {move, x(4), y(4)}
      {move, x(3), x(4)}
      {load, 0, x(2)}
      {load, 0, x(3)}
      {load, 0, x(5)}
      {call, mandelbrot, 6, 5}
      ;; [H | cols]
      {cons, x(0), y(4), x(4)}
      {move, y(0), x(0)}
      {load, 1, x(5)}
      ;; y - 1
      {sub, y(1), x(5), x(1)}
      {move, y(2), x(2)}
      {move, y(3), x(3)}
      {call, mandelbrot_r, 5, 0}
      {return}

    {func, mandelbrot_c, 6}
    {label, mandelbrot_c_6}
      ;; if (x == xend)
      {eq, x(0), x(1), x(6)}
      {brt, x(6), mandelbrot_c_6_false}
      {move, x(5), x(0)}
      {return}
    {label, mandelbrot_c_6_false}
      {move, x(0), y(0)}
      {move, x(1), y(1)}
      {move, x(2), y(2)}
      {move, x(3), y(3)}
      {move, x(4), y(4)}
      {move, x(5), y(5)}
      {move, x(2), x(1)}
      {move, x(3), x(2)}
      {move, x(4), x(3)}
      {load, nil, x(4)}
      {call, mandelbrot_r, 5, 6}
      {cons, x(0), y(5), x(5)}
      {load, 1, x(6)}
      ;; x - 1
      {sub, y(0), x(6), x(0)}
      {move, y(1), x(1)}
      {move, y(2), x(2)}
      {move, y(3), x(3)}
      {move, y(4), x(4)}
      {call, mandelbrot_r, 5, 0}
      {return}

    {func, mandelbrot, 3}
    {label, mandelbrot_3}
      {load, 2, x(3)}
      {div, x(0), x(3), x(4)}
      {div, x(1), x(3), x(5)}
      {load, -1, x(6)}
      {mul, x(4), x(6), x(7)}
      {mul, x(5), x(6), x(8)}
      {move, x(4), x(0)}
      {move, x(7), x(1)}
      {move, x(5), x(2)}
      {move, x(8), x(3)}
      {move, x(2), x(4)}
      {load, nil, x(5)}
      {call, mandelbrot_c, 5, 0}
      {return}

cmd:

    # alc mandelbrot.alm
    # alm mandelbrot(400,400,200)
    huge list


# Step 6

Add a tracing copy collect GC
