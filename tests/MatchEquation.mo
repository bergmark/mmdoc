function f
algorithm
  res := match x
    case _ equation a; then ();
    case _ equation b; f(); then ();
  end match;
end f;
