/*
 *  Copyright 2001-2013 Stephen Colebourne
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.joda.time;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.joda.convert.FromString;
import org.joda.convert.ToString;
import org.joda.time.chrono.BaseChronology;
import org.joda.time.field.FieldUtils;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.DateTimeFormatterBuilder;
import org.joda.time.format.FormatUtils;
import org.joda.time.tz.DefaultNameProvider;
import org.joda.time.tz.FixedDateTimeZone;
import org.joda.time.tz.NameProvider;
import org.joda.time.tz.Provider;
import org.joda.time.tz.UTCProvider;
import org.joda.time.tz.ZoneInfoProvider;

public abstract class DateTimeZone implements Serializable {
    
    /** Serialization version. */
    private static final long serialVersionUID = 5546345482340108586L;

    /** The time zone for Universal Coordinated Time */
    public static final DateTimeZone UTC = new FixedDateTimeZone("UTC", "UTC", 0, 0);
    /** Maximum offset. */
    private static final int MAX_MILLIS = (86400 * 1000) - 1;

    /** The instance that is providing time zones. */
    private static Provider cProvider;
    /** The instance that is providing time zone names. */
    private static NameProvider cNameProvider;
    /** The set of ID strings. */
    private static Set<String> cAvailableIDs;
    /** The default time zone. */
    private static volatile DateTimeZone cDefault;
    /** A formatter for printing and parsing zones. */
    private static DateTimeFormatter cOffsetFormatter;

    /** Cache that maps fixed offset strings to softly referenced DateTimeZones */
    private static Map<String, SoftReference<DateTimeZone>> iFixedOffsetCache;

    /** Cache of old zone IDs to new zone IDs */
    private static Map<String, String> cZoneIdConversion;


    /**
     * Sets the name provider factory without performing the security check.
     * 
     * @param nameProvider  provider to use, or null for default
     * @throws IllegalArgumentException if the provider is invalid
     */
    private static void setNameProvider0(NameProvider nameProvider) {
        if (nameProvider == null) {
            nameProvider = getDefaultNameProvider();
        }
        int i = 5;
        cNameProvider = nameProvider;
    }


}
