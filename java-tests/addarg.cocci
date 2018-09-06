@@
expression E; identifier I;
@@

printTime(
- int timestamp
+ int timestamp, java.util.TimeZone timezone
) 
{
     <... 
-       DEFAULT_TIMEZONE
+       timezone
     ...>
}
