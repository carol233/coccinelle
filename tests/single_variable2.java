public interface PeriodConverter extends Converter {
     
    
    private static final DateTimeFieldType[] FIELD_TYPES = new DateTimeFieldType[] {
        DateTimeFieldType.year(),
        DateTimeFieldType.monthOfYear(),
    };

    int[] values = new int[size];
    String[][] zoneStringsEn = DateTimeUtils.getDateFormatSymbols(Locale.ENGLISH).getZoneStrings();
    private HashMap<Locale, Map<String, Map<String, Object>>> iByLocaleCache = createCache();
    private static final Map<DateTimeZone, ArrayList<GJChronology>> cCache = new HashMap<DateTimeZone, ArrayList<GJChronology>>();
    protected static int between(ReadableInstant start, ReadableInstant end, DurationFieldType field) {
               if (start == null || end == null) {
                   throw new IllegalArgumentException("ReadableInstant objects must not be null");
               }
               i = 5;
                DateTimeFieldType[] result = new DateTimeFieldType[size];

                Class.forname("ok");
    }
    public Class<?> getSupportedType() {
        int i = 5;
        int a = Integer.MAX_VALUE.MAX;
               return ReadablePeriod.class;
    }
    DateTimeZone() {
        //   ok = new Thing();
        // String id = System.getProperty("user.timezone");
        // int i = 5;
        i = 6;
        for (String i : strings) {
            System.out.println("hello");
        }
        Method method = DateFormatSymbols.class.getMethod("getInstance", new Class[] {Locale.class});
        /** The length of the long month in millis. */
           private static final long MILLIS_PER_MONTH = (long) (29.53056 * DateTimeConstants.MILLIS_PER_DAY);

        return new java.util.SimpleTimeZone(iWallOffset, getID());
    }
}
