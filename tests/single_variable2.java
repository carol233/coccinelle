import Math.*;

public enum JavaVersion{
     
    JAVA_0_9(true, true),
    JAVA_1_1(1.1f, "1.1");

    private static AtomicReference<DSCompiler[][]> compilers = new AtomicReference<DSCompiler[][]>(null);

    static final AtomicReference<long[][]> STIRLING_S2 = new AtomicReference<long[][]> (null);

    private final Map<Integer, Pair<T[], T[]>> pointsAndWeights
        = new TreeMap<Integer, Pair<T[], T[]>>();
@Option(name = "--define",
        aliases = {"--D", "-D"},
         nest = @NestAnnotation( 
             booleanValue = false,
             booleanValues = { false }
         ),
         nests = {
             @NestAnnotation( 
             booleanValue = false,
             booleanValues = { false }
         )
         })
    Thing() {
        new Covariance(new double[][] {{},{}});
        ImmutableList.Builder<Set<E>> result = ImmutableList.builder();
    }

    public static Type[] getImplicitBounds(final TypeVariable<?> typeVariable) {
        final Type[] bounds = typeVariable.getBounds();
        final Class<?>[] array1 = new Class[] {Object.class};
        final Type[] expectedArray = {String.class};
        return bounds.length == 0 ? new Type[] { Object.class } : normalizeUpperBounds(bounds);
    }

    @Test
    public void testWrappersToPrimitivesEmpty() {
         final Class<?>[] empty = new Class[0];
         assertArrayEquals("Wrong result for empty input", empty, ClassUtils.wrappersToPrimitives(empty));
     }


     void hodor() {
         if (y < y) {
             return true;
         }

         return new double[][] {
                 { -radius * cLon * sLat, -radius * sLon * cLat },
                 { -radius * sLon * sLat,  radius * cLon * cLat },
                 {  radius * cLat,         0  }
             };
     }
}
