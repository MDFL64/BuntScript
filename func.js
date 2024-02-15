let performance = require('node:perf_hooks').performance;

function func(limit, a, b) {
    let i = 0;
    let sum = 0;

    while (i < limit) {
        i = i + 1;
        sum = (sum + a) / b;
    };
    return sum;
}

let start = performance.now();
console.log(func(1_000_000_000.0, 5.0, 1.01));
let end = performance.now();
console.log((end - start)/1000);
