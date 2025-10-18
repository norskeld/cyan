int fib(int n) {
  if (n == 0 || n == 1) {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

int main(void) {
  int n = 12;
  return fib(n);
}
