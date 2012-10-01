encapsulated package Build

/**
 * Currently the type variables in Build are prefixed with a B.
 * This is since type errors are reported with unqualified type names
 * and it would thus be hard to tell e.g. Iterator.State and
 * Build.State apart.
 */

replaceable type BElement subtypeof Any;
replaceable type BState subtypeof Any;
replaceable type BResult subtypeof Any;

partial function Insert
  input BElement element;
  input BState inState;
  output BState outState;
end Insert;

partial function Current
  input BState state;
  output BResult result;
end Current;


end Build;
