fun f(): Unit {
  return;
}

fun id(i: Int): Int {
  return i;
}

fun id(d: Double): Double {
  return d;
}
/* You could add all other variants of argument. */

fun max(a: Int, b: Int): Int {
  if (a > b) {
    return a;
  }
  else {
    return b;
  }
}

fun max(a: Int, b: Int, c: Int, d: Int): Int {
  return max(max(a, b), max(c, d));
}

fun countFor(n: Int): Int {
  if (n <= 0) {
    return 0;
  }
  var acc: Int = 0;
  for (x in 1..n) {
    acc = acc + x;
  }
  return acc;
}

fun countMath(n: Int): Int {
  if (n <= 0) {
    return 0;
  }
  else {
    return n * (n + 1) / 2;
  }
}

fun main(): Unit {
  /* do nothing */
  f();

  /* print given numbers */
  println(id(10));
  println(id(1.0));

  /* print max number (2) */
  println(max(1, 2, 2, 0));

  /* print the same sums */
  println(countFor(10));
  println(countMath(10));
}
