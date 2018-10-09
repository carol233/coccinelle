// instead of looking of lack of uses of i in the then-block
// check for the lack of guards


// find identifiers that need to be checked for " >= 0"
@ first @
identifier i;
statement S, S1;
position p;
@@
if (i@p >= 0) S else S1


// check if a wrong guard is used
@ second @
identifier first.i, j!={first.i};
expression E;
statement S;
@@

if (j >= 0) {
<...
(
* E(i)
|
* i
)
...>
} else S

