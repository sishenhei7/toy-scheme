function factorialTrampoline(n, k) {
  if (n === 0) {
    return k(0)
  }
  return () => factorialTrampoline(n - 1, (res) => () => k(res + 1))
}

// Trampoline function that iteratively applies a continuation function
function trampoline(fn) {
  let result = fn();
  while (typeof result === 'function') {
    result = result()
  }
  return result;
}

// Call the factorial function using the trampoline
const factorial = (n) => trampoline(() => factorialTrampoline(n, (result) => result));
console.log(factorial(16000));
// console.log(test(10000))