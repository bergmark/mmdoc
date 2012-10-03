function f
  input A b;
  input A.B c;
  input A.B<C> d;
  input A.B.C d;
algorithm
  A.B.c();
  A.B.c;
end f;
