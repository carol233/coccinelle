import Math.*;

public interface PeriodConverter extends Converter {
     
    
    // private static final DateTimeFieldType[] FIELD_TYPES = new DateTimeFieldType[] {
    //     DateTimeFieldType.year(),
    //     DateTimeFieldType.monthOfYear(),
    // };

    // int[] values = new int[size];
    // String[][] zoneStringsEn = DateTimeUtils.getDateFormatSymbols(Locale.ENGLISH).getZoneStrings();
    // private HashMap<Locale, Map<String, Map<String, Object>>> iByLocaleCache = createCache();
    // private static final Map<DateTimeZone, ArrayList<GJChronology>> cCache = new HashMap<DateTimeZone, ArrayList<GJChronology>>();
    // protected static int between(ReadableInstant start, ReadableInstant end, DurationFieldType field) {
    //            if (start == null || end == null) {
    //                throw new IllegalArgumentException("ReadableInstant objects must not be null");
    //            }
    //            i = 5;
    //             DateTimeFieldType[] result = new DateTimeFieldType[size];

    //             Class.forname("ok");
    // }
    // public Class<?> getSupportedType(String... strings) {
    //     if (StringUtils.isEmpty(str) || deepEmpty(set)) {
    //            return ReadablePeriod.class;
    //     }
    //     int i = 5;
    //     int a = Integer.MAX_VALUE.MAX;
    //            return ReadablePeriod.class;
    // }
    // DateTimeZone() {
    //     //   ok = new Thing();
    //     // String id = System.getProperty("user.timezone");
    //     // int i = 5;
    //     i = 6;
    //     for (String i : strings) {
    //         System.out.println("hello");
    //     }
    //     Method method = DateFormatSymbols.class.getMethod("getInstance", new Class[] {Locale.class});
    //     /** The length of the long month in millis. */
    //        private static final long MILLIS_PER_MONTH = (long) (29.53056 * DateTimeConstants.MILLIS_PER_DAY);

    //     return new java.util.SimpleTimeZone(iWallOffset, getID());
    // }

    // public THing get(String strings, Ok ok) {
    //     final Map.Entry<?, ?> other = (Map.Entry<?, ?>) obj;
    //     return strings;
    // }
    //  public static <T> T[] toArray(final T... items) {
    //      final String[][] newarray = new String[array.length][2];
    //     // return items;
    //     return SerializationUtils.<Q>deserialize(new ByteArrayInputStream(objectData));
    // }
    @Test(expected = NullPointerException.class)
    public void testMedian_nullItems() {
        ObjectUtils.median((String[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMedian_emptyItems() {
        assert 1 == 1;
        for (Map.Entry e : inits.entrySet()) {
        ObjectUtils.<String> median();

        }
        Class<?>[] array1s = new Class[] {String.class};
        Object[] params = new Object[] {"other", new Byte((byte) 128)};
        java.util.TimeZone tz = zone.toTimeZone();

        Object[] array = new Object[] {null, base, new int[] {3, 6}};
    }

}
