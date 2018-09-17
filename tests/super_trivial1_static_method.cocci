@other@
identifier k;
expression list[n] params;
@@

OtherClass.k(params)

@script:python@
p1 << other.params;
k << other.k;
n << other.n;
@@
print("calls to OtherClass %s with params %s [%s]" % (k, p1, n,))