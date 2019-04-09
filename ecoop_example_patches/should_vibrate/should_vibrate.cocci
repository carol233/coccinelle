@@
identifier am, f, ctx;
expression vibrate_type;
position p;
@@
+ boolean shouldVibrate(AudioManager am, Context ctx, int vibrateType) {
+     if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
+         Vibrator vibrator = (Vibrator) ctx. getSystemService(
+             Context.VIBRATOR_SERVICE );
+         if (vibrator == null || !vibrator.hasVibrator()) {
+                 return false;
+         }
+         return am.getRingerMode() != 
+             AudioManager.RINGER_MODE_SILENT;
+     } else {
+         return audioManager.shouldVibrate(vibrateType);
+     }
+ }

f(..., Context ctx, ...) {
...
- am.shouldVibrate(vibrate_type)
+ shouldVibrate(am, ctx, vibrate_type)
...
}

@@
identifier am;
expression vibrate_type;
identifier f !~ "^shouldVibrate$";
@@
f(...) {
<... when exists
- am.shouldVibrate(vibrate_type)
+ am.getRingerMode() != AudioManager.RINGER_MODE_SILENT
...>
}
