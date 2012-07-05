# Step 1

code:

    def add() {
        return 1+2*3/(5+91);
    }

cmd:

    $ alm add
    1.0625

# Step 2

code:

    def add(a,b) {
        return a+b;
    }

cmd:

    $ alm add(1.5,3)
    4.5

# Step 3

code:

    def add(a,b) {
        if (b == 0) return a;
        return b + add(a,b-1);
    }

    def mul(a,b) {
        return add(a,b);
    }

cmd:

    $ alm mul(5,6)
    30.0

# Step 4

code:

    def list(n) {
        if (n == 0) return [];
        return [n|list(n-1)];
    }

cmd:

    # alm list(5)
    [5,4,3,2,1]

# Step 5

Add GC

# Step 6

Implement mandelbrot+julia