@@
TimeSeries ts, ts1;
identifier i, i1;
expression E;
@@


* ts1 =
(
  i.clone()
|
  (TimeSeries) i.clone()
)
... when != ts1.minY = Double.NaN
    when !=  ts1.maxY = Double.NaN 



