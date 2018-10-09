@first exists@
identifier f;
position p;
@@

f (...) {
... 
dotrap@p
... 
}


@second exists@
identifier first.f;
statement S1;
position first.p;
position p1;
identifier i;
type t;
@@
f (t i) {
...
 if@p1 (<+... i  ...+>) 
{
...
   dotrap@p
...
  } else S1
...
}

@extract_e@
position second.p1;
expression E;
position EP;
statement S1;
@@

if@p1 (E@EP) 
{
...
   dotrap
...
  } else S1

@third exists @

identifier first.f;
expression extract_e.E;
position p;
@@
(
 return f@p(...);
|
 f@p(...);
)

// prints out some hints as to what constraints to add
@initialize:python@
@@

from coccilib.org import print_todo, print_safe_todo, print_link, print_safe_link


@ script:python @
E << extract_e.E;
EP << extract_e.EP;
fp << third.p;
ff << first.f;

@@
print("Consider adding checks:")
print("if !(" + E + ") ")
print(ff)
print_link(fp[0])
print("=====================")