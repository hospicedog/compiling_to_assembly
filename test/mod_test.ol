function factorial(n) {
    let ret = 1;

    while (n != 1) {
        ret = ret * n;
        n = n - 1;
    }
    
    return ret;
}

function mod(n, m) {
    return n - (n / m) * m;
}

function fact_sum_mod(to_fact, other, mod) {
    let fact = factorial(to_fact);
    let sum = fact + other;
    let res = mod(sum, mod);
    return res;
}

function _start() {
    let a = 5;
    let b = 11;
    let m = 4;
    let res = fact_sum_mod(a, b, m);
    // res = 3;
    return res;
}
