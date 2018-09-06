@@
identifier i
@@

try {
    average(...)
} catch (
- InvalidArgumentException i
+ InvalidArgumentException | CustomException i
)


@@

@@

average(...) 
- throws InvalidArgumentException
+ throws InvalidArgumentException | CustomException
 
