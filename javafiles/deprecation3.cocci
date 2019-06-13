
@first@
identifier a0;
position p0;
@@

- void onMetadataChanged(RadioMetadata a0) {
+void onProgramInfoChanged(ProgramInfo a1) {
+RadioMetadata a0 = a1.getMetadata();
...@p0
}

@@
position p = first.p0;
@@
+ import android.hardware.radio.RadioManager.ProgramInfo;
+ import android.hardware.radio.RadioManager.ProgramInfo2;
import ...;

...@p


