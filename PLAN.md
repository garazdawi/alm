# Step 1

def add() {
    return 1+2*3/(5+91);
}

$ alm add
1.0625

# Step 2

def add(a,b) {
    return a+b;
}

$ alm add(1.5,3)
4.5

# Step 3

def add(a,b) {
    if (b == 0) return a;
    return b + add(a,b-1);
}

def mul(a,b) {
    return add(a,b);
}

$ alm mul(5,6)
30.0

# Step 4

def list(n) {
    if (n == 0) return [];
    return [n|list(n-1)];
}

# alm list(5)
[5,4,3,2,1]

# Step 5

Add GC

# Step 6

Implement mandelbrot+julia