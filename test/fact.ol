function factorial(n) {
    let ret = 1;

    while (n != 1) {
        ret = ret * n;
        n = n - 1;
    }
    
    return ret;
}

function _start() {
    let n = factorial(3);
    return n;
}
