@@
identifier am , f,ctx;
expression vibrate_type;
@@
f(..., Context ctx , ...) {
...
- am.shouldVibrate(vibrate_type )
+ if (Build.VERSION.SDK_INT >= Build. VERSION_CODES .JELLY_BEAN) { 
+ Vibrator vibrator = (Vibrator) ctx. getSystemService (
+ Context. VIBRATOR_SERVICE );
+ if (vibrator == null || !vibrator.hasVibrator ()) {
+ return false;
+ }
+ return audioManager.getRingerMode () !=
+ AudioManager. RINGER_MODE_SILENT ;
+ } else {
+ return audioManager.shouldVibrate ( vibrate_type );
+ }
...
}