function f
algorithm
  res := match x
    local A y;
    case _ then y;
  end match;
  res := match x
    local A a1, a2; B b;
    case _ then y;
  end match;
end f;
