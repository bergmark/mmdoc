function f<A>
  input List<A> a;
  output A b;
algorithm
end f;

partial function f<A>
  input List<A> a;
end f;

function f<A,B>
  output Tuple<A,B> v;
algorithm
end f;

partial function f<A>
  output List<List<A>> v;
end f;
