function f
algorithm
  res := match x
    case _ then x;
    else equation y; then z;
  end match;
end f;
