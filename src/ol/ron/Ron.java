package ol.ron;

import clojure.lang.BigInt;
import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.ITransientCollection;
import clojure.lang.ITransientMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentVector;
import clojure.lang.Ratio;
import clojure.lang.Sequential;
import clojure.lang.Symbol;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicReferenceArray;

/**
 * JDK 25 RON parser and renderer.
 *
 * <p>The exact text API retains number spelling, object member order, and
 * duplicate evidence. The data API constructs Clojure collections directly.
 * String and UTF-8 inputs use separate scanners.</p>
 */
public final class Ron {
  public enum Mode {
    PRETTY,
    COMPACT,
    CANONICAL
  }

  /** A JSON/RON number retained as source text. */
  public static final class Num {
    public final String text;

    public Num(String text) {
      if (!isNumberShaped(text)) {
        throw new IllegalArgumentException("invalid JSON number: " + text);
      }
      this.text = text;
    }

    @Override
    public String toString() {
      return text;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Num num && num.text.equals(text);
    }

    @Override
    public int hashCode() {
      return text.hashCode();
    }
  }

  /** A path replacement used by JSON-to-RON typed rendering. */
  public static final class Hook {
    public final List<?> path;
    public final Object replacement;

    public Hook(List<?> path, Object replacement) {
      this.path = List.copyOf(path);
      this.replacement = replacement;
    }
  }

  /** Parse failure with a character offset for String input or byte offset for UTF-8 input. */
  public static class ParseException extends RuntimeException {
    private static final long serialVersionUID = 1L;
    public final int offset;

    public ParseException(String message, int offset) {
      super(message + " at offset " + offset, null, false, false);
      this.offset = offset;
    }
  }

  /** Ordered exact-model object. Map lookup exposes last-value-wins semantics. */
  public static final class OrderedObject extends AbstractMap<String, Object> {
    private final ArrayList<Member> all = new ArrayList<>();
    private final ArrayList<Member> effective = new ArrayList<>();
    private boolean duplicate;

    void add(String key, Object value) {
      Member member = new Member(key, value);
      all.add(member);
      for (int i = effective.size() - 1; i >= 0; i--) {
        if (effective.get(i).key.equals(key)) {
          effective.remove(i);
          duplicate = true;
          break;
        }
      }
      effective.add(member);
    }

    boolean hasDuplicate() {
      return duplicate;
    }

    // Ordinary elided documents canonicalize their last-value view. Explicit
    // object documents retain duplicate evidence for canonical-source rejection.
    void collapseDuplicateEvidence() {
      duplicate = false;
    }

    List<Member> allMembers() {
      return all;
    }

    List<Member> members() {
      return effective;
    }

    @Override
    public Object get(Object key) {
      for (int i = effective.size() - 1; i >= 0; i--) {
        Member member = effective.get(i);
        if (member.key.equals(key)) {
          return member.value;
        }
      }
      return null;
    }

    @Override
    public boolean containsKey(Object key) {
      for (Member member : effective) {
        if (member.key.equals(key)) {
          return true;
        }
      }
      return false;
    }

    @Override
    public int size() {
      return effective.size();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
      return new AbstractSet<>() {
        @Override
        public int size() {
          return effective.size();
        }

        @Override
        public Iterator<Entry<String, Object>> iterator() {
          Iterator<Member> members = effective.iterator();
          return new Iterator<>() {
            @Override
            public boolean hasNext() {
              return members.hasNext();
            }

            @Override
            public Entry<String, Object> next() {
              Member member = members.next();
              return new SimpleImmutableEntry<>(member.key, member.value);
            }
          };
        }
      };
    }
  }

  static final class Member {
    final String key;
    final Object value;

    Member(String key, Object value) {
      this.key = key;
      this.value = value;
    }
  }

  static final class Span {
    final int start;
    final int end;
    final boolean escaped;
    final boolean ascii;

    Span(int start, int end, boolean escaped, boolean ascii) {
      this.start = start;
      this.end = end;
      this.escaped = escaped;
      this.ascii = ascii;
    }
  }

  private static final int DEFAULT_MAX_DEPTH = 1000;
  private static final int INLINE_LIMIT = 80;
  private static final int KEY_CACHE_SIZE = 1024;
  private static final int MAX_CACHED_KEY = 64;
  private static final int OUTPUT_BUFFER_SIZE = 64 * 1024;
  private static final int MAX_RETAINED_OUTPUT = 8 * 1024 * 1024;
  private static final char[] HEX = "0123456789abcdef".toCharArray();
  private static final boolean[] ASCII_WS = new boolean[128];
  private static final boolean[] ASCII_DELIM = new boolean[128];
  private static final AtomicReferenceArray<String> KEY_CACHE =
      new AtomicReferenceArray<>(KEY_CACHE_SIZE);
  private static final ArrayBlockingQueue<StringBuilder> STRING_BUILDERS =
      new ArrayBlockingQueue<>(8);
  private static final ArrayBlockingQueue<ByteOut> BYTE_OUTPUTS =
      new ArrayBlockingQueue<>(8);
  private static final ArrayBlockingQueue<StreamOut> STREAM_OUTPUTS =
      new ArrayBlockingQueue<>(8);
  private static final boolean VECTOR =
      !"false".equalsIgnoreCase(System.getProperty("ol.ron.vector", "true"))
          && ModuleLayer.boot().findModule("jdk.incubator.vector").isPresent();
  private static final int VECTOR_BYTE_THRESHOLD = 64;

  public static final Comparator<String> UTF16_ORDER = String::compareTo;

  static {
    for (char c : new char[] {'\t', '\n', '\r', ' '}) {
      ASCII_WS[c] = true;
    }
    for (char c : new char[] {'\t', '\n', '\r', ' ', ',', '{', '}', '[', ']', '"', '\''}) {
      ASCII_DELIM[c] = true;
    }
  }

  private Ron() {}

  static ParseException error(String message, int offset) {
    return new ParseException(message, offset);
  }

  static boolean isWhitespace(char c) {
    return c < 128 ? ASCII_WS[c] : Character.isWhitespace(c) || Character.isSpaceChar(c);
  }

  static boolean isWhitespace(int codePoint) {
    return codePoint < 128
        ? ASCII_WS[codePoint]
        : Character.isWhitespace(codePoint) || Character.isSpaceChar(codePoint);
  }

  static boolean isDelimiter(char c) {
    return c < 128 ? ASCII_DELIM[c] : isWhitespace(c);
  }

  static boolean isDelimiter(int codePoint) {
    return codePoint < 128 ? ASCII_DELIM[codePoint] : isWhitespace(codePoint);
  }

  static boolean isDigit(char c) {
    return c >= '0' && c <= '9';
  }

  static boolean isDigit(byte c) {
    return c >= '0' && c <= '9';
  }

  public static boolean isNumberShaped(String text) {
    return isNumberShaped(text, 0, text.length());
  }

  static boolean isNumberShaped(CharSequence text, int start, int end) {
    int i = start;
    if (i < end && text.charAt(i) == '-') {
      i++;
    }
    if (i == end) {
      return false;
    }
    char c = text.charAt(i);
    if (c == '0') {
      i++;
    } else if (c >= '1' && c <= '9') {
      do {
        i++;
      } while (i < end && isDigit(text.charAt(i)));
    } else {
      return false;
    }
    if (i < end && text.charAt(i) == '.') {
      i++;
      if (i == end || !isDigit(text.charAt(i))) {
        return false;
      }
      do {
        i++;
      } while (i < end && isDigit(text.charAt(i)));
    }
    if (i < end && (text.charAt(i) == 'e' || text.charAt(i) == 'E')) {
      i++;
      if (i < end && (text.charAt(i) == '+' || text.charAt(i) == '-')) {
        i++;
      }
      if (i == end || !isDigit(text.charAt(i))) {
        return false;
      }
      do {
        i++;
      } while (i < end && isDigit(text.charAt(i)));
    }
    return i == end;
  }

  static boolean isNumberShaped(byte[] bytes, int start, int end) {
    int i = start;
    if (i < end && bytes[i] == '-') {
      i++;
    }
    if (i == end) {
      return false;
    }
    byte c = bytes[i];
    if (c == '0') {
      i++;
    } else if (c >= '1' && c <= '9') {
      do {
        i++;
      } while (i < end && isDigit(bytes[i]));
    } else {
      return false;
    }
    if (i < end && bytes[i] == '.') {
      i++;
      if (i == end || !isDigit(bytes[i])) {
        return false;
      }
      do {
        i++;
      } while (i < end && isDigit(bytes[i]));
    }
    if (i < end && (bytes[i] == 'e' || bytes[i] == 'E')) {
      i++;
      if (i < end && (bytes[i] == '+' || bytes[i] == '-')) {
        i++;
      }
      if (i == end || !isDigit(bytes[i])) {
        return false;
      }
      do {
        i++;
      } while (i < end && isDigit(bytes[i]));
    }
    return i == end;
  }

  static int hex(char c, int offset) {
    if (c >= '0' && c <= '9') {
      return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
      return c - 'a' + 10;
    }
    if (c >= 'A' && c <= 'F') {
      return c - 'A' + 10;
    }
    throw error("invalid hex digit '" + c + "'", offset);
  }

  static int hex(byte c, int offset) {
    if (c >= '0' && c <= '9') {
      return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
      return c - 'a' + 10;
    }
    if (c >= 'A' && c <= 'F') {
      return c - 'A' + 10;
    }
    throw error("invalid hex digit '" + (char) (c & 0xff) + "'", offset);
  }

  static long utf8CodePoint(byte[] bytes, int offset, int end) {
    int b0 = bytes[offset] & 0xff;
    if (b0 < 0x80) {
      return ((long) b0 << 3) | 1;
    }
    if (b0 >= 0xc2 && b0 <= 0xdf) {
      requireContinuation(bytes, offset + 1, end, offset);
      int cp = ((b0 & 0x1f) << 6) | (bytes[offset + 1] & 0x3f);
      return ((long) cp << 3) | 2;
    }
    if (b0 >= 0xe0 && b0 <= 0xef) {
      requireContinuation(bytes, offset + 1, end, offset);
      requireContinuation(bytes, offset + 2, end, offset);
      int b1 = bytes[offset + 1] & 0xff;
      if ((b0 == 0xe0 && b1 < 0xa0) || (b0 == 0xed && b1 >= 0xa0)) {
        throw error("malformed UTF-8", offset);
      }
      int cp = ((b0 & 0x0f) << 12) | ((b1 & 0x3f) << 6) | (bytes[offset + 2] & 0x3f);
      return ((long) cp << 3) | 3;
    }
    if (b0 >= 0xf0 && b0 <= 0xf4) {
      requireContinuation(bytes, offset + 1, end, offset);
      requireContinuation(bytes, offset + 2, end, offset);
      requireContinuation(bytes, offset + 3, end, offset);
      int b1 = bytes[offset + 1] & 0xff;
      if ((b0 == 0xf0 && b1 < 0x90) || (b0 == 0xf4 && b1 >= 0x90)) {
        throw error("malformed UTF-8", offset);
      }
      int cp = ((b0 & 0x07) << 18)
          | ((b1 & 0x3f) << 12)
          | ((bytes[offset + 2] & 0x3f) << 6)
          | (bytes[offset + 3] & 0x3f);
      return ((long) cp << 3) | 4;
    }
    throw error("malformed UTF-8", offset);
  }

  static void requireContinuation(byte[] bytes, int offset, int end, int start) {
    if (offset >= end || (bytes[offset] & 0xc0) != 0x80) {
      throw error("malformed UTF-8", start);
    }
  }

  static int utf8Width(long packed) {
    return (int) (packed & 7);
  }

  static int utf8Value(long packed) {
    return (int) (packed >>> 3);
  }

  static String strictUtf8(byte[] bytes, int start, int end) {
    for (int i = start; i < end; ) {
      long packed = utf8CodePoint(bytes, i, end);
      i += utf8Width(packed);
    }
    return new String(bytes, start, end - start, StandardCharsets.UTF_8);
  }

  static String cachedCharKey(String source, int start, int end) {
    int length = end - start;
    if (length == 0 || length > MAX_CACHED_KEY) {
      return source.substring(start, end);
    }
    int hash = 0;
    for (int i = start; i < end; i++) {
      hash = 31 * hash + source.charAt(i);
    }
    int slot = hash & (KEY_CACHE_SIZE - 1);
    String cached = KEY_CACHE.get(slot);
    if (cached != null && cached.length() == length
        && source.regionMatches(start, cached, 0, length)) {
      return cached;
    }
    String key = source.substring(start, end);
    KEY_CACHE.set(slot, key);
    return key;
  }

  static String cachedByteKey(byte[] source, int start, int end, boolean ascii) {
    int length = end - start;
    if (!ascii || length == 0 || length > MAX_CACHED_KEY) {
      return strictUtf8(source, start, end);
    }
    int hash = 0;
    for (int i = start; i < end; i++) {
      hash = 31 * hash + (source[i] & 0xff);
    }
    int slot = hash & (KEY_CACHE_SIZE - 1);
    String cached = KEY_CACHE.get(slot);
    if (cached != null && cached.length() == length) {
      boolean same = true;
      for (int i = 0; i < length; i++) {
        if (cached.charAt(i) != (char) (source[start + i] & 0xff)) {
          same = false;
          break;
        }
      }
      if (same) {
        return cached;
      }
    }
    String key = new String(source, start, length, StandardCharsets.US_ASCII);
    KEY_CACHE.set(slot, key);
    return key;
  }

  static String cacheDecodedKey(String key) {
    if (key.isEmpty() || key.length() > MAX_CACHED_KEY) {
      return key;
    }
    int slot = key.hashCode() & (KEY_CACHE_SIZE - 1);
    String cached = KEY_CACHE.get(slot);
    if (key.equals(cached)) {
      return cached;
    }
    KEY_CACHE.set(slot, key);
    return key;
  }

  static Object dataKey(String key, IFn keyFn, int keyMode) {
    return switch (keyMode) {
      case 0 -> key;
      case 1 -> Keyword.intern(key);
      case 2 -> keyFn.invoke(key);
      default -> throw new IllegalArgumentException("invalid key mode: " + keyMode);
    };
  }

  static Object realizeNumber(String text) {
    return realizeNumber(text, 0, text.length());
  }

  static Object realizeNumber(String text, int start, int end) {
    for (int i = start; i < end; i++) {
      char c = text.charAt(i);
      if (c == '.' || c == 'e' || c == 'E') {
        return Double.parseDouble(text.substring(start, end));
      }
    }
    boolean negative = text.charAt(start) == '-';
    int i = negative ? start + 1 : start;
    long limit = negative ? Long.MIN_VALUE : -Long.MAX_VALUE;
    long multiplyLimit = limit / 10;
    long value = 0;
    while (i < end) {
      int digit = text.charAt(i++) - '0';
      if (value < multiplyLimit) {
        return BigInt.fromBigInteger(new BigInteger(text.substring(start, end)));
      }
      value *= 10;
      if (value < limit + digit) {
        return BigInt.fromBigInteger(new BigInteger(text.substring(start, end)));
      }
      value -= digit;
    }
    return negative ? value : -value;
  }

  static Object realizeNumber(byte[] text, int start, int end) {
    for (int i = start; i < end; i++) {
      byte c = text[i];
      if (c == '.' || c == 'e' || c == 'E') {
        return Double.parseDouble(new String(text, start, end - start, StandardCharsets.US_ASCII));
      }
    }
    boolean negative = text[start] == '-';
    int i = negative ? start + 1 : start;
    long limit = negative ? Long.MIN_VALUE : -Long.MAX_VALUE;
    long multiplyLimit = limit / 10;
    long value = 0;
    while (i < end) {
      int digit = text[i++] - '0';
      if (value < multiplyLimit) {
        return BigInt.fromBigInteger(new BigInteger(
            new String(text, start, end - start, StandardCharsets.US_ASCII)));
      }
      value *= 10;
      if (value < limit + digit) {
        return BigInt.fromBigInteger(new BigInteger(
            new String(text, start, end - start, StandardCharsets.US_ASCII)));
      }
      value -= digit;
    }
    return negative ? value : -value;
  }

  static void putPair(ArrayList<Object> pairs, Object key, Object value) {
    for (int i = pairs.size() - 2; i >= 0; i -= 2) {
      if (java.util.Objects.equals(pairs.get(i), key)) {
        pairs.remove(i + 1);
        pairs.remove(i);
        break;
      }
    }
    pairs.add(key);
    pairs.add(value);
  }

  static IPersistentMap persistentMap(ArrayList<Object> pairs) {
    if (pairs.isEmpty()) {
      return PersistentArrayMap.EMPTY;
    }
    if (pairs.size() <= 16) {
      return PersistentArrayMap.createAsIfByAssoc(pairs.toArray());
    }
    ITransientMap transientMap = PersistentHashMap.EMPTY.asTransient();
    for (int i = 0; i < pairs.size(); i += 2) {
      transientMap = transientMap.assoc(pairs.get(i), pairs.get(i + 1));
    }
    return transientMap.persistent();
  }

  static final class CharRonParser {
    final String source;
    final int length;
    final boolean data;
    final IFn keyFn;
    final int keyMode;
    final int maxDepth;
    int position;

    CharRonParser(String source, boolean data, IFn keyFn, int keyMode, int maxDepth) {
      this.source = source;
      this.length = source.length();
      this.data = data;
      this.keyFn = keyFn;
      this.keyMode = keyMode;
      this.maxDepth = maxDepth;
    }

    Object parseDocument() {
      skipTopLevel();
      if (position == length) {
        throw error("empty input", position);
      }
      int first = position;
      Object value;
      char c = source.charAt(position);
      if (c == '{' || c == '[') {
        value = parseValue(0);
      } else {
        try {
          position = 0;
          value = parseElidedObject();
        } catch (ParseException ignored) {
          position = 0;
          skipTopLevel();
          value = parseValue(0);
        }
      }
      skipTopLevel();
      if (position != length) {
        throw error("trailing data", position);
      }
      if (position < first) {
        throw error("invalid parser state", position);
      }
      return value;
    }

    void skipTopLevel() {
      while (position < length) {
        char c = source.charAt(position);
        if (c == ',' || isWhitespace(c)) {
          position++;
        } else {
          break;
        }
      }
    }

    void skipWhitespace() {
      while (position < length && isWhitespace(source.charAt(position))) {
        position++;
      }
    }

    void skipSeparators() {
      while (position < length) {
        char c = source.charAt(position);
        if (c == ',' || isWhitespace(c)) {
          position++;
        } else {
          break;
        }
      }
    }

    Object parseValue(int depth) {
      if (position == length) {
        throw error("unexpected EOF, expected value", position);
      }
      return switch (source.charAt(position)) {
        case '{' -> {
          checkDepth(depth);
          yield parseObject(depth + 1);
        }
        case '[' -> {
          checkDepth(depth);
          yield parseArray(depth + 1);
        }
        case '\'', '"' -> parseQuoted(false);
        case ',' -> parseCommaToken(false);
        case '}', ']' -> throw error("unexpected closing delimiter", position);
        default -> parseBareValue();
      };
    }

    void checkDepth(int depth) {
      if (depth >= maxDepth) {
        throw error("maximum nesting depth exceeded", position);
      }
    }

    Object parseElidedObject() {
      checkDepth(0);
      skipTopLevel();
      if (position == length) {
        throw error("empty input", position);
      }
      Object object = data ? new ArrayList<>() : new OrderedObject();
      while (true) {
        String key = parseKey();
        Object finalKey = data ? dataKey(key, keyFn, keyMode) : key;
        skipWhitespace();
        Object value = parseValue(1);
        addObjectMember(object, finalKey, key, value);
        skipSeparators();
        if (position == length) {
          if (!data) {
            ((OrderedObject) object).collapseDuplicateEvidence();
          }
          return finishObject(object);
        }
      }
    }

    Object parseObject(int depth) {
      position++;
      Object object = data ? new ArrayList<>() : new OrderedObject();
      skipWhitespace();
      if (position < length && source.charAt(position) == '}') {
        position++;
        return finishObject(object);
      }
      while (true) {
        if (position == length) {
          throw error("unterminated object", position);
        }
        String key = parseKey();
        Object finalKey = data ? dataKey(key, keyFn, keyMode) : key;
        skipWhitespace();
        Object value = parseValue(depth);
        addObjectMember(object, finalKey, key, value);
        skipSeparators();
        if (position == length) {
          throw error("unterminated object", position);
        }
        if (source.charAt(position) == '}') {
          position++;
          return finishObject(object);
        }
      }
    }

    @SuppressWarnings("unchecked")
    void addObjectMember(Object object, Object finalKey, String rawKey, Object value) {
      if (data) {
        putPair((ArrayList<Object>) object, finalKey, value);
      } else {
        ((OrderedObject) object).add(rawKey, value);
      }
    }

    @SuppressWarnings("unchecked")
    Object finishObject(Object object) {
      return data ? persistentMap((ArrayList<Object>) object) : object;
    }

    Object parseArray(int depth) {
      position++;
      ITransientCollection transientVector = data ? PersistentVector.EMPTY.asTransient() : null;
      ArrayList<Object> exact = data ? null : new ArrayList<>();
      skipWhitespace();
      if (position < length && source.charAt(position) == ']') {
        position++;
        return data ? PersistentVector.EMPTY : exact;
      }
      while (true) {
        if (position == length) {
          throw error("unterminated array", position);
        }
        Object value = parseValue(depth);
        if (data) {
          transientVector = transientVector.conj(value);
        } else {
          exact.add(value);
        }
        skipSeparators();
        if (position == length) {
          throw error("unterminated array", position);
        }
        if (source.charAt(position) == ']') {
          position++;
          return data ? transientVector.persistent() : exact;
        }
      }
    }

    String parseKey() {
      if (position == length) {
        throw error("unexpected EOF, expected object key", position);
      }
      return switch (source.charAt(position)) {
        case '\'', '"' -> parseQuoted(true);
        case ',' -> parseCommaToken(true);
        case '{', '}', '[', ']' -> throw error("unexpected delimiter, expected object key", position);
        default -> {
          Span span = scanToken(position);
          if (span.end == span.start) {
            throw error("empty object key", position);
          }
          position = span.end;
          yield keyFromSpan(span);
        }
      };
    }

    Object parseBareValue() {
      Span span = scanToken(position);
      if (span.end == span.start) {
        throw error("empty value", position);
      }
      position = span.end;
      int size = span.end - span.start;
      if (!span.escaped && size == 4 && source.regionMatches(span.start, "true", 0, 4)) {
        return Boolean.TRUE;
      }
      if (!span.escaped && size == 5 && source.regionMatches(span.start, "false", 0, 5)) {
        return Boolean.FALSE;
      }
      if (!span.escaped && size == 4 && source.regionMatches(span.start, "null", 0, 4)) {
        return null;
      }
      if (!span.escaped && isNumberShaped(source, span.start, span.end)) {
        if (data) {
          return realizeNumber(source, span.start, span.end);
        }
        return new Num(source.substring(span.start, span.end));
      }
      return span.escaped ? decodeRonSpan(span.start, span.end) : source.substring(span.start, span.end);
    }

    String parseCommaToken(boolean key) {
      int start = position;
      Span span = scanToken(position + 1);
      position = span.end;
      Span whole = new Span(start, span.end, span.escaped, true);
      return key ? keyFromSpan(whole) : stringFromSpan(whole);
    }

    String parseQuoted(boolean key) {
      char quote = source.charAt(position);
      int opening = position;
      int p = opening;
      while (p < length && source.charAt(p) == quote) {
        p++;
      }
      int delimiterLength = p - opening;
      if (p == length || isDelimiter(source.charAt(p))) {
        if ((delimiterLength & 1) == 0) {
          position = p;
          return "";
        }
        if (quote == '\'' && delimiterLength >= 5 && (delimiterLength - 2) % 3 == 0) {
          position = p;
          return "'".repeat((delimiterLength - 2) / 3);
        }
      }
      int contentStart = p;
      boolean escaped = false;
      while (p < length) {
        char c = source.charAt(p);
        if (c == '\\') {
          escaped = true;
          p = validateRonEscape(p, length);
          continue;
        }
        if (c < 0x20) {
          if (quote == '\'' && delimiterLength == 1
              && isDelimiter(source.charAt(opening + 1))) {
            position = opening + 1;
            return "'";
          }
          throw error("unescaped control character in string", p);
        }
        if (Character.isSurrogate(c)) {
          if (!Character.isHighSurrogate(c) || p + 1 >= length
              || !Character.isLowSurrogate(source.charAt(p + 1))) {
            throw error("unpaired surrogate in string", p);
          }
          p += 2;
          continue;
        }
        if (c == quote) {
          int run = p;
          while (p < length && source.charAt(p) == quote) {
            p++;
          }
          if (p - run >= delimiterLength) {
            position = run + delimiterLength;
            Span span = new Span(contentStart, run, escaped, true);
            return key ? keyFromSpan(span) : stringFromSpan(span);
          }
          continue;
        }
        p++;
      }
      if (quote == '\'' && opening + 1 <= length
          && (opening + 1 == length || isDelimiter(source.charAt(opening + 1)))) {
        position = opening + 1;
        return "'";
      }
      throw error("unterminated string", opening);
    }

    Span scanToken(int start) {
      int p = start;
      boolean escaped = false;
      while (p < length) {
        char c = source.charAt(p);
        if (c == '\\') {
          escaped = true;
          p = validateRonEscape(p, length);
          continue;
        }
        if (isDelimiter(c)) {
          break;
        }
        if (c < 0x20) {
          throw error("unescaped control character in string", p);
        }
        if (Character.isSurrogate(c)) {
          if (!Character.isHighSurrogate(c) || p + 1 >= length
              || !Character.isLowSurrogate(source.charAt(p + 1))) {
            throw error("unpaired surrogate in string", p);
          }
          p += 2;
        } else {
          p++;
        }
      }
      return new Span(start, p, escaped, true);
    }

    int validateRonEscape(int slash, int end) {
      int p = slash + 1;
      if (p >= end) {
        throw error("trailing backslash", slash);
      }
      char escape = source.charAt(p++);
      if (escape == '"' || escape == '\\' || escape == '/'
          || escape == 'b' || escape == 'f' || escape == 'n'
          || escape == 'r' || escape == 't') {
        return p;
      }
      if (escape != 'u') {
        throw error("invalid escape character '" + escape + "'", p - 1);
      }
      int value = unicodeEscapeValue(p, end);
      p += 4;
      if (Character.isHighSurrogate((char) value)) {
        if (p + 6 > end || source.charAt(p) != '\\' || source.charAt(p + 1) != 'u') {
          throw error("unpaired high surrogate", slash);
        }
        int low = unicodeEscapeValue(p + 2, end);
        if (!Character.isLowSurrogate((char) low)) {
          throw error("unpaired high surrogate", slash);
        }
        p += 6;
      } else if (Character.isLowSurrogate((char) value)) {
        throw error("unpaired low surrogate", slash);
      }
      return p;
    }

    int unicodeEscapeValue(int start, int end) {
      if (start + 4 > end) {
        throw error("unterminated unicode escape", start);
      }
      return (hex(source.charAt(start), start) << 12)
          | (hex(source.charAt(start + 1), start + 1) << 8)
          | (hex(source.charAt(start + 2), start + 2) << 4)
          | hex(source.charAt(start + 3), start + 3);
    }

    String keyFromSpan(Span span) {
      if (!span.escaped) {
        return cachedCharKey(source, span.start, span.end);
      }
      return cacheDecodedKey(decodeRonSpan(span.start, span.end));
    }

    String stringFromSpan(Span span) {
      return span.escaped ? decodeRonSpan(span.start, span.end) : source.substring(span.start, span.end);
    }

    String decodeRonSpan(int start, int end) {
      int slash = source.indexOf('\\', start);
      if (slash < 0 || slash >= end) {
        return source.substring(start, end);
      }
      StringBuilder out = new StringBuilder(end - start);
      out.append(source, start, slash);
      int p = slash;
      while (p < end) {
        char c = source.charAt(p++);
        if (c != '\\') {
          out.append(c);
          continue;
        }
        char escape = source.charAt(p++);
        switch (escape) {
          case '"' -> out.append('"');
          case '\\' -> out.append('\\');
          case '/' -> out.append('/');
          case 'b' -> out.append('\b');
          case 'f' -> out.append('\f');
          case 'n' -> out.append('\n');
          case 'r' -> out.append('\r');
          case 't' -> out.append('\t');
          case 'u' -> {
            int value = unicodeEscapeValue(p, end);
            p += 4;
            if (Character.isHighSurrogate((char) value)) {
              int low = unicodeEscapeValue(p + 2, end);
              out.append((char) value).append((char) low);
              p += 6;
            } else {
              out.append((char) value);
            }
          }
          default -> throw error("invalid escape character", p - 1);
        }
      }
      return out.toString();
    }
  }

  static final class ByteRonParser {
    final byte[] source;
    final int length;
    final boolean data;
    final IFn keyFn;
    final int keyMode;
    final int maxDepth;
    int position;

    ByteRonParser(byte[] source, boolean data, IFn keyFn, int keyMode, int maxDepth) {
      this.source = source;
      this.length = source.length;
      this.data = data;
      this.keyFn = keyFn;
      this.keyMode = keyMode;
      this.maxDepth = maxDepth;
    }

    Object parseDocument() {
      skipTopLevel();
      if (position == length) {
        throw error("empty input", position);
      }
      Object value;
      byte c = source[position];
      if (c == '{' || c == '[') {
        value = parseValue(0);
      } else {
        try {
          position = 0;
          value = parseElidedObject();
        } catch (ParseException ignored) {
          position = 0;
          skipTopLevel();
          value = parseValue(0);
        }
      }
      skipTopLevel();
      if (position != length) {
        throw error("trailing data", position);
      }
      return value;
    }

    void skipTopLevel() {
      while (position < length) {
        int b = source[position] & 0xff;
        if (b == ',' || (b < 128 && ASCII_WS[b])) {
          position++;
        } else if (b >= 128) {
          long packed = utf8CodePoint(source, position, length);
          if (!isWhitespace(utf8Value(packed))) {
            return;
          }
          position += utf8Width(packed);
        } else {
          return;
        }
      }
    }

    void skipWhitespace() {
      while (position < length) {
        int b = source[position] & 0xff;
        if (b < 128 && ASCII_WS[b]) {
          position++;
        } else if (b >= 128) {
          long packed = utf8CodePoint(source, position, length);
          if (!isWhitespace(utf8Value(packed))) {
            return;
          }
          position += utf8Width(packed);
        } else {
          return;
        }
      }
    }

    void skipSeparators() {
      while (position < length) {
        int b = source[position] & 0xff;
        if (b == ',' || (b < 128 && ASCII_WS[b])) {
          position++;
        } else if (b >= 128) {
          long packed = utf8CodePoint(source, position, length);
          if (!isWhitespace(utf8Value(packed))) {
            return;
          }
          position += utf8Width(packed);
        } else {
          return;
        }
      }
    }

    Object parseValue(int depth) {
      if (position == length) {
        throw error("unexpected EOF, expected value", position);
      }
      return switch (source[position]) {
        case '{' -> {
          checkDepth(depth);
          yield parseObject(depth + 1);
        }
        case '[' -> {
          checkDepth(depth);
          yield parseArray(depth + 1);
        }
        case '\'', '"' -> parseQuoted(false);
        case ',' -> parseCommaToken(false);
        case '}', ']' -> throw error("unexpected closing delimiter", position);
        default -> parseBareValue();
      };
    }

    void checkDepth(int depth) {
      if (depth >= maxDepth) {
        throw error("maximum nesting depth exceeded", position);
      }
    }

    Object parseElidedObject() {
      checkDepth(0);
      skipTopLevel();
      if (position == length) {
        throw error("empty input", position);
      }
      Object object = data ? new ArrayList<>() : new OrderedObject();
      while (true) {
        String key = parseKey();
        Object finalKey = data ? dataKey(key, keyFn, keyMode) : key;
        skipWhitespace();
        Object value = parseValue(1);
        addObjectMember(object, finalKey, key, value);
        skipSeparators();
        if (position == length) {
          if (!data) {
            ((OrderedObject) object).collapseDuplicateEvidence();
          }
          return finishObject(object);
        }
      }
    }

    Object parseObject(int depth) {
      position++;
      Object object = data ? new ArrayList<>() : new OrderedObject();
      skipWhitespace();
      if (position < length && source[position] == '}') {
        position++;
        return finishObject(object);
      }
      while (true) {
        if (position == length) {
          throw error("unterminated object", position);
        }
        String key = parseKey();
        Object finalKey = data ? dataKey(key, keyFn, keyMode) : key;
        skipWhitespace();
        Object value = parseValue(depth);
        addObjectMember(object, finalKey, key, value);
        skipSeparators();
        if (position == length) {
          throw error("unterminated object", position);
        }
        if (source[position] == '}') {
          position++;
          return finishObject(object);
        }
      }
    }

    @SuppressWarnings("unchecked")
    void addObjectMember(Object object, Object finalKey, String rawKey, Object value) {
      if (data) {
        putPair((ArrayList<Object>) object, finalKey, value);
      } else {
        ((OrderedObject) object).add(rawKey, value);
      }
    }

    @SuppressWarnings("unchecked")
    Object finishObject(Object object) {
      return data ? persistentMap((ArrayList<Object>) object) : object;
    }

    Object parseArray(int depth) {
      position++;
      ITransientCollection transientVector = data ? PersistentVector.EMPTY.asTransient() : null;
      ArrayList<Object> exact = data ? null : new ArrayList<>();
      skipWhitespace();
      if (position < length && source[position] == ']') {
        position++;
        return data ? PersistentVector.EMPTY : exact;
      }
      while (true) {
        if (position == length) {
          throw error("unterminated array", position);
        }
        Object value = parseValue(depth);
        if (data) {
          transientVector = transientVector.conj(value);
        } else {
          exact.add(value);
        }
        skipSeparators();
        if (position == length) {
          throw error("unterminated array", position);
        }
        if (source[position] == ']') {
          position++;
          return data ? transientVector.persistent() : exact;
        }
      }
    }

    String parseKey() {
      if (position == length) {
        throw error("unexpected EOF, expected object key", position);
      }
      return switch (source[position]) {
        case '\'', '"' -> parseQuoted(true);
        case ',' -> parseCommaToken(true);
        case '{', '}', '[', ']' -> throw error("unexpected delimiter, expected object key", position);
        default -> {
          Span span = scanToken(position);
          if (span.end == span.start) {
            throw error("empty object key", position);
          }
          position = span.end;
          yield keyFromSpan(span);
        }
      };
    }

    Object parseBareValue() {
      Span span = scanToken(position);
      if (span.end == span.start) {
        throw error("empty value", position);
      }
      position = span.end;
      int size = span.end - span.start;
      if (!span.escaped && span.ascii && size == 4 && asciiEquals(span.start, "true")) {
        return Boolean.TRUE;
      }
      if (!span.escaped && span.ascii && size == 5 && asciiEquals(span.start, "false")) {
        return Boolean.FALSE;
      }
      if (!span.escaped && span.ascii && size == 4 && asciiEquals(span.start, "null")) {
        return null;
      }
      if (!span.escaped && span.ascii && isNumberShaped(source, span.start, span.end)) {
        if (data) {
          return realizeNumber(source, span.start, span.end);
        }
        return new Num(new String(source, span.start, size, StandardCharsets.US_ASCII));
      }
      return stringFromSpan(span);
    }

    boolean asciiEquals(int start, String text) {
      for (int i = 0; i < text.length(); i++) {
        if (source[start + i] != (byte) text.charAt(i)) {
          return false;
        }
      }
      return true;
    }

    String parseCommaToken(boolean key) {
      int start = position;
      Span rest = scanToken(position + 1);
      position = rest.end;
      Span whole = new Span(start, rest.end, rest.escaped, rest.ascii);
      return key ? keyFromSpan(whole) : stringFromSpan(whole);
    }

    String parseQuoted(boolean key) {
      byte quote = source[position];
      int opening = position;
      int p = opening;
      while (p < length && source[p] == quote) {
        p++;
      }
      int delimiterLength = p - opening;
      if (p == length || byteDelimiterAt(p)) {
        if ((delimiterLength & 1) == 0) {
          position = p;
          return "";
        }
        if (quote == '\'' && delimiterLength >= 5 && (delimiterLength - 2) % 3 == 0) {
          position = p;
          return "'".repeat((delimiterLength - 2) / 3);
        }
      }
      int contentStart = p;
      boolean escaped = false;
      boolean ascii = true;
      while (p < length) {
        if (VECTOR && length - p >= VECTOR_BYTE_THRESHOLD) {
          int next = VectorScan.scanRonQuoted(source, p, length, quote);
          if (next > p) {
            p = next;
            if (p == length) {
              break;
            }
          }
        }
        int b = source[p] & 0xff;
        if (b == '\\') {
          escaped = true;
          p = validateRonEscape(p, length);
          continue;
        }
        if (b < 0x20) {
          if (quote == '\'' && delimiterLength == 1
              && byteDelimiterAt(opening + 1)) {
            position = opening + 1;
            return "'";
          }
          throw error("unescaped control character in string", p);
        }
        if (b == (quote & 0xff)) {
          int run = p;
          while (p < length && source[p] == quote) {
            p++;
          }
          if (p - run >= delimiterLength) {
            position = run + delimiterLength;
            Span span = new Span(contentStart, run, escaped, ascii);
            return key ? keyFromSpan(span) : stringFromSpan(span);
          }
          continue;
        }
        if (b >= 0x80) {
          ascii = false;
          long packed = utf8CodePoint(source, p, length);
          p += utf8Width(packed);
        } else {
          p++;
        }
      }
      if (quote == '\'' && opening + 1 <= length
          && (opening + 1 == length || byteDelimiterAt(opening + 1))) {
        position = opening + 1;
        return "'";
      }
      throw error("unterminated string", opening);
    }

    boolean byteDelimiterAt(int offset) {
      int b = source[offset] & 0xff;
      if (b < 128) {
        return ASCII_DELIM[b];
      }
      return isWhitespace(utf8Value(utf8CodePoint(source, offset, length)));
    }

    Span scanToken(int start) {
      int p = start;
      boolean escaped = false;
      boolean ascii = true;
      while (p < length) {
        if (VECTOR && length - p >= VECTOR_BYTE_THRESHOLD) {
          int next = VectorScan.scanRonToken(source, p, length);
          if (next > p) {
            p = next;
            if (p == length) {
              break;
            }
          }
        }
        int b = source[p] & 0xff;
        if (b == '\\') {
          escaped = true;
          p = validateRonEscape(p, length);
          continue;
        }
        if (b < 128) {
          if (ASCII_DELIM[b]) {
            break;
          }
          if (b < 0x20) {
            throw error("unescaped control character in string", p);
          }
          p++;
        } else {
          ascii = false;
          long packed = utf8CodePoint(source, p, length);
          if (isWhitespace(utf8Value(packed))) {
            break;
          }
          p += utf8Width(packed);
        }
      }
      return new Span(start, p, escaped, ascii);
    }

    int validateRonEscape(int slash, int end) {
      int p = slash + 1;
      if (p >= end) {
        throw error("trailing backslash", slash);
      }
      byte escape = source[p++];
      if (escape == '"' || escape == '\\' || escape == '/'
          || escape == 'b' || escape == 'f' || escape == 'n'
          || escape == 'r' || escape == 't') {
        return p;
      }
      if (escape != 'u') {
        throw error("invalid escape character '" + (char) (escape & 0xff) + "'", p - 1);
      }
      int value = unicodeEscapeValue(p, end);
      p += 4;
      if (Character.isHighSurrogate((char) value)) {
        if (p + 6 > end || source[p] != '\\' || source[p + 1] != 'u') {
          throw error("unpaired high surrogate", slash);
        }
        int low = unicodeEscapeValue(p + 2, end);
        if (!Character.isLowSurrogate((char) low)) {
          throw error("unpaired high surrogate", slash);
        }
        p += 6;
      } else if (Character.isLowSurrogate((char) value)) {
        throw error("unpaired low surrogate", slash);
      }
      return p;
    }

    int unicodeEscapeValue(int start, int end) {
      if (start + 4 > end) {
        throw error("unterminated unicode escape", start);
      }
      return (hex(source[start], start) << 12)
          | (hex(source[start + 1], start + 1) << 8)
          | (hex(source[start + 2], start + 2) << 4)
          | hex(source[start + 3], start + 3);
    }

    String keyFromSpan(Span span) {
      if (!span.escaped) {
        return cachedByteKey(source, span.start, span.end, span.ascii);
      }
      return cacheDecodedKey(decodeRonSpan(span.start, span.end));
    }

    String stringFromSpan(Span span) {
      if (span.escaped) {
        return decodeRonSpan(span.start, span.end);
      }
      return span.ascii
          ? new String(source, span.start, span.end - span.start, StandardCharsets.US_ASCII)
          : strictUtf8(source, span.start, span.end);
    }

    String decodeRonSpan(int start, int end) {
      StringBuilder out = new StringBuilder(end - start);
      int p = start;
      int clean = start;
      while (p < end) {
        int b = source[p] & 0xff;
        if (b == '\\') {
          if (clean < p) {
            out.append(strictUtf8(source, clean, p));
          }
          byte escape = source[p + 1];
          p += 2;
          switch (escape) {
            case '"' -> out.append('"');
            case '\\' -> out.append('\\');
            case '/' -> out.append('/');
            case 'b' -> out.append('\b');
            case 'f' -> out.append('\f');
            case 'n' -> out.append('\n');
            case 'r' -> out.append('\r');
            case 't' -> out.append('\t');
            case 'u' -> {
              int value = unicodeEscapeValue(p, end);
              p += 4;
              if (Character.isHighSurrogate((char) value)) {
                int low = unicodeEscapeValue(p + 2, end);
                out.append((char) value).append((char) low);
                p += 6;
              } else {
                out.append((char) value);
              }
            }
            default -> throw error("invalid escape character", p - 1);
          }
          clean = p;
        } else if (b < 0x80) {
          p++;
        } else {
          p += utf8Width(utf8CodePoint(source, p, end));
        }
      }
      if (clean < end) {
        out.append(strictUtf8(source, clean, end));
      }
      return out.toString();
    }
  }

  static final class CharJsonParser {
    final String source;
    final int length;
    final int maxDepth;
    int position;

    CharJsonParser(String source, int maxDepth) {
      this.source = source;
      this.length = source.length();
      this.maxDepth = maxDepth;
    }

    Object parseDocument() {
      skipWhitespace();
      if (position == length) {
        throw error("empty input", position);
      }
      Object value = parseValue(0);
      skipWhitespace();
      if (position != length) {
        throw error("trailing data", position);
      }
      return value;
    }

    void skipWhitespace() {
      while (position < length) {
        char c = source.charAt(position);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
          position++;
        } else {
          return;
        }
      }
    }

    Object parseValue(int depth) {
      if (position == length) {
        throw error("unexpected EOF, expected value", position);
      }
      char c = source.charAt(position);
      return switch (c) {
        case '{' -> {
          checkDepth(depth);
          yield parseObject(depth + 1);
        }
        case '[' -> {
          checkDepth(depth);
          yield parseArray(depth + 1);
        }
        case '"' -> parseString();
        case 't' -> {
          expect("true");
          yield Boolean.TRUE;
        }
        case 'f' -> {
          expect("false");
          yield Boolean.FALSE;
        }
        case 'n' -> {
          expect("null");
          yield null;
        }
        default -> {
          if (c == '-' || isDigit(c)) {
            yield parseNumber();
          }
          throw error("unexpected character '" + c + "'", position);
        }
      };
    }

    void checkDepth(int depth) {
      if (depth >= maxDepth) {
        throw error("maximum nesting depth exceeded", position);
      }
    }

    void expect(String text) {
      if (!source.regionMatches(position, text, 0, text.length())) {
        throw error("expected '" + text + "'", position);
      }
      position += text.length();
    }

    OrderedObject parseObject(int depth) {
      position++;
      OrderedObject object = new OrderedObject();
      skipWhitespace();
      if (position < length && source.charAt(position) == '}') {
        position++;
        return object;
      }
      while (true) {
        if (position == length || source.charAt(position) != '"') {
          throw error("expected JSON object key", position);
        }
        String key = parseString();
        skipWhitespace();
        if (position == length || source.charAt(position) != ':') {
          throw error("expected ':' after object key", position);
        }
        position++;
        skipWhitespace();
        object.add(key, parseValue(depth));
        skipWhitespace();
        if (position == length) {
          throw error("unterminated object", position);
        }
        char c = source.charAt(position++);
        if (c == '}') {
          return object;
        }
        if (c != ',') {
          throw error("expected ',' or '}' in object", position - 1);
        }
        skipWhitespace();
      }
    }

    ArrayList<Object> parseArray(int depth) {
      position++;
      ArrayList<Object> array = new ArrayList<>();
      skipWhitespace();
      if (position < length && source.charAt(position) == ']') {
        position++;
        return array;
      }
      while (true) {
        array.add(parseValue(depth));
        skipWhitespace();
        if (position == length) {
          throw error("unterminated array", position);
        }
        char c = source.charAt(position++);
        if (c == ']') {
          return array;
        }
        if (c != ',') {
          throw error("expected ',' or ']' in array", position - 1);
        }
        skipWhitespace();
      }
    }

    String parseString() {
      int opening = position++;
      int start = position;
      int p = position;
      while (p < length) {
        char c = source.charAt(p);
        if (c == '"') {
          position = p + 1;
          return source.substring(start, p);
        }
        if (c == '\\' || c < 0x20) {
          break;
        }
        p++;
      }
      StringBuilder out = new StringBuilder(p - start + 16);
      out.append(source, start, p);
      while (p < length) {
        char c = source.charAt(p++);
        if (c == '"') {
          position = p;
          return out.toString();
        }
        if (c < 0x20) {
          throw error("unescaped control character in string", p - 1);
        }
        if (c != '\\') {
          out.append(c);
          continue;
        }
        if (p == length) {
          break;
        }
        char escape = source.charAt(p++);
        switch (escape) {
          case '"' -> out.append('"');
          case '\\' -> out.append('\\');
          case '/' -> out.append('/');
          case 'b' -> out.append('\b');
          case 'f' -> out.append('\f');
          case 'n' -> out.append('\n');
          case 'r' -> out.append('\r');
          case 't' -> out.append('\t');
          case 'u' -> {
            if (p + 4 > length) {
              throw error("unterminated unicode escape", p);
            }
            int value = (hex(source.charAt(p), p) << 12)
                | (hex(source.charAt(p + 1), p + 1) << 8)
                | (hex(source.charAt(p + 2), p + 2) << 4)
                | hex(source.charAt(p + 3), p + 3);
            out.append((char) value);
            p += 4;
          }
          default -> throw error("invalid escape character '" + escape + "'", p - 1);
        }
      }
      throw error("unterminated string", opening);
    }

    Num parseNumber() {
      int start = position;
      int p = position;
      if (source.charAt(p) == '-') {
        p++;
      }
      if (p == length || !isDigit(source.charAt(p))) {
        throw error("invalid number", start);
      }
      if (source.charAt(p) == '0') {
        p++;
      } else {
        while (p < length && isDigit(source.charAt(p))) {
          p++;
        }
      }
      if (p < length && source.charAt(p) == '.') {
        p++;
        if (p == length || !isDigit(source.charAt(p))) {
          throw error("invalid number", start);
        }
        while (p < length && isDigit(source.charAt(p))) {
          p++;
        }
      }
      if (p < length && (source.charAt(p) == 'e' || source.charAt(p) == 'E')) {
        p++;
        if (p < length && (source.charAt(p) == '+' || source.charAt(p) == '-')) {
          p++;
        }
        if (p == length || !isDigit(source.charAt(p))) {
          throw error("invalid number", start);
        }
        while (p < length && isDigit(source.charAt(p))) {
          p++;
        }
      }
      position = p;
      return new Num(source.substring(start, p));
    }
  }

  static final class ByteJsonParser {
    final byte[] source;
    final int length;
    final int maxDepth;
    int position;

    ByteJsonParser(byte[] source, int maxDepth) {
      this.source = source;
      this.length = source.length;
      this.maxDepth = maxDepth;
    }

    Object parseDocument() {
      skipWhitespace();
      if (position == length) {
        throw error("empty input", position);
      }
      Object value = parseValue(0);
      skipWhitespace();
      if (position != length) {
        throw error("trailing data", position);
      }
      return value;
    }

    void skipWhitespace() {
      while (position < length) {
        byte c = source[position];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
          position++;
        } else {
          return;
        }
      }
    }

    Object parseValue(int depth) {
      if (position == length) {
        throw error("unexpected EOF, expected value", position);
      }
      byte c = source[position];
      return switch (c) {
        case '{' -> {
          checkDepth(depth);
          yield parseObject(depth + 1);
        }
        case '[' -> {
          checkDepth(depth);
          yield parseArray(depth + 1);
        }
        case '"' -> parseString();
        case 't' -> {
          expect("true");
          yield Boolean.TRUE;
        }
        case 'f' -> {
          expect("false");
          yield Boolean.FALSE;
        }
        case 'n' -> {
          expect("null");
          yield null;
        }
        default -> {
          if (c == '-' || isDigit(c)) {
            yield parseNumber();
          }
          throw error("unexpected character", position);
        }
      };
    }

    void checkDepth(int depth) {
      if (depth >= maxDepth) {
        throw error("maximum nesting depth exceeded", position);
      }
    }

    void expect(String text) {
      if (position + text.length() > length) {
        throw error("expected '" + text + "'", position);
      }
      for (int i = 0; i < text.length(); i++) {
        if (source[position + i] != (byte) text.charAt(i)) {
          throw error("expected '" + text + "'", position);
        }
      }
      position += text.length();
    }

    OrderedObject parseObject(int depth) {
      position++;
      OrderedObject object = new OrderedObject();
      skipWhitespace();
      if (position < length && source[position] == '}') {
        position++;
        return object;
      }
      while (true) {
        if (position == length || source[position] != '"') {
          throw error("expected JSON object key", position);
        }
        String key = parseString();
        skipWhitespace();
        if (position == length || source[position] != ':') {
          throw error("expected ':' after object key", position);
        }
        position++;
        skipWhitespace();
        object.add(key, parseValue(depth));
        skipWhitespace();
        if (position == length) {
          throw error("unterminated object", position);
        }
        byte c = source[position++];
        if (c == '}') {
          return object;
        }
        if (c != ',') {
          throw error("expected ',' or '}' in object", position - 1);
        }
        skipWhitespace();
      }
    }

    ArrayList<Object> parseArray(int depth) {
      position++;
      ArrayList<Object> array = new ArrayList<>();
      skipWhitespace();
      if (position < length && source[position] == ']') {
        position++;
        return array;
      }
      while (true) {
        array.add(parseValue(depth));
        skipWhitespace();
        if (position == length) {
          throw error("unterminated array", position);
        }
        byte c = source[position++];
        if (c == ']') {
          return array;
        }
        if (c != ',') {
          throw error("expected ',' or ']' in array", position - 1);
        }
        skipWhitespace();
      }
    }

    String parseString() {
      int opening = position++;
      int start = position;
      int p = position;
      boolean ascii = true;
      while (p < length) {
        if (VECTOR && length - p >= VECTOR_BYTE_THRESHOLD) {
          int next = VectorScan.scanJsonString(source, p, length);
          if (next > p) {
            p = next;
            if (p == length) {
              break;
            }
          }
        }
        int b = source[p] & 0xff;
        if (b == '"') {
          position = p + 1;
          return ascii
              ? new String(source, start, p - start, StandardCharsets.US_ASCII)
              : strictUtf8(source, start, p);
        }
        if (b == '\\' || b < 0x20) {
          break;
        }
        if (b >= 0x80) {
          ascii = false;
          p += utf8Width(utf8CodePoint(source, p, length));
        } else {
          p++;
        }
      }
      StringBuilder out = new StringBuilder(p - start + 16);
      if (start < p) {
        out.append(strictUtf8(source, start, p));
      }
      while (p < length) {
        int b = source[p] & 0xff;
        if (b == '"') {
          position = p + 1;
          return out.toString();
        }
        if (b < 0x20) {
          throw error("unescaped control character in string", p);
        }
        if (b == '\\') {
          p++;
          if (p == length) {
            break;
          }
          byte escape = source[p++];
          switch (escape) {
            case '"' -> out.append('"');
            case '\\' -> out.append('\\');
            case '/' -> out.append('/');
            case 'b' -> out.append('\b');
            case 'f' -> out.append('\f');
            case 'n' -> out.append('\n');
            case 'r' -> out.append('\r');
            case 't' -> out.append('\t');
            case 'u' -> {
              if (p + 4 > length) {
                throw error("unterminated unicode escape", p);
              }
              int value = (hex(source[p], p) << 12)
                  | (hex(source[p + 1], p + 1) << 8)
                  | (hex(source[p + 2], p + 2) << 4)
                  | hex(source[p + 3], p + 3);
              out.append((char) value);
              p += 4;
            }
            default -> throw error("invalid escape character", p - 1);
          }
        } else if (b < 0x80) {
          out.append((char) b);
          p++;
        } else {
          long packed = utf8CodePoint(source, p, length);
          out.appendCodePoint(utf8Value(packed));
          p += utf8Width(packed);
        }
      }
      throw error("unterminated string", opening);
    }

    Num parseNumber() {
      int start = position;
      int p = position;
      if (source[p] == '-') {
        p++;
      }
      if (p == length || !isDigit(source[p])) {
        throw error("invalid number", start);
      }
      if (source[p] == '0') {
        p++;
      } else {
        while (p < length && isDigit(source[p])) {
          p++;
        }
      }
      if (p < length && source[p] == '.') {
        p++;
        if (p == length || !isDigit(source[p])) {
          throw error("invalid number", start);
        }
        while (p < length && isDigit(source[p])) {
          p++;
        }
      }
      if (p < length && (source[p] == 'e' || source[p] == 'E')) {
        p++;
        if (p < length && (source[p] == '+' || source[p] == '-')) {
          p++;
        }
        if (p == length || !isDigit(source[p])) {
          throw error("invalid number", start);
        }
        while (p < length && isDigit(source[p])) {
          p++;
        }
      }
      position = p;
      return new Num(new String(source, start, p - start, StandardCharsets.US_ASCII));
    }
  }

  static boolean isMapValue(Object value) {
    return value instanceof Map;
  }

  static boolean hasOnlyStringKeys(Object value) {
    for (Object key : ((Map<?, ?>) value).keySet()) {
      if (!(key instanceof String)) {
        return false;
      }
    }
    return true;
  }

  static boolean isListValue(Object value) {
    return value instanceof List || value instanceof Sequential;
  }

  static List<?> listValue(Object value) {
    if (value instanceof List<?> list) {
      return list;
    }
    if (value instanceof Iterable<?> iterable && value instanceof Sequential) {
      ArrayList<Object> result = new ArrayList<>();
      for (Object element : iterable) {
        result.add(element);
      }
      return result;
    }
    throw unsupported(value);
  }

  static IllegalArgumentException unsupported(Object value) {
    return new IllegalArgumentException(
        "value has no RON representation: " + (value == null ? "null" : value.getClass().getName()));
  }

  static String keyString(Object key) {
    if (key instanceof String text) {
      return text;
    }
    if (key instanceof Keyword keyword) {
      String namespace = keyword.getNamespace();
      return namespace == null ? keyword.getName() : namespace + "/" + keyword.getName();
    }
    if (key instanceof Symbol symbol) {
      return symbol.toString();
    }
    throw new IllegalArgumentException("RON object keys must be strings, keywords, or symbols: " + key);
  }

  static String numberText(Number number) {
    if (number instanceof Ratio) {
      throw new IllegalArgumentException("ratios have no RON representation");
    }
    if (number instanceof Double || number instanceof Float) {
      double value = number.doubleValue();
      if (!Double.isFinite(value)) {
        throw new IllegalArgumentException("NaN and infinity have no RON representation");
      }
      return Double.toString(value);
    }
    if (number instanceof BigInt bigInt) {
      return bigInt.toString();
    }
    if (number instanceof BigInteger || number instanceof BigDecimal
        || number instanceof Byte || number instanceof Short
        || number instanceof Integer || number instanceof Long) {
      return number.toString();
    }
    String text = number.toString();
    if (!isNumberShaped(text)) {
      throw unsupported(number);
    }
    return text;
  }

  static Object cljToModel(Object value) {
    if (value == null || value instanceof Boolean || value instanceof String) {
      return value;
    }
    if (value instanceof Keyword || value instanceof Symbol) {
      return keyString(value);
    }
    if (value instanceof Number number) {
      return new Num(numberText(number));
    }
    if (value instanceof Map<?, ?> map) {
      OrderedObject object = new OrderedObject();
      for (Map.Entry<?, ?> entry : map.entrySet()) {
        object.add(keyString(entry.getKey()), cljToModel(entry.getValue()));
      }
      return object;
    }
    if (isListValue(value)) {
      ArrayList<Object> array = new ArrayList<>();
      for (Object element : listValue(value)) {
        array.add(cljToModel(element));
      }
      return array;
    }
    throw unsupported(value);
  }

  static Object applyHooks(Object value, List<Hook> hooks) {
    if (hooks == null || hooks.isEmpty()) {
      return value;
    }
    return applyHooksAt(value, hooks, new ArrayList<>());
  }

  static Object applyHooksAt(Object value, List<Hook> hooks, ArrayList<Object> path) {
    for (Hook hook : hooks) {
      if (hook.path.equals(path)) {
        return cljToModel(hook.replacement);
      }
    }
    if (value instanceof OrderedObject object) {
      OrderedObject result = new OrderedObject();
      for (Member member : object.allMembers()) {
        path.add(member.key);
        result.add(member.key, applyHooksAt(member.value, hooks, path));
        path.remove(path.size() - 1);
      }
      return result;
    }
    if (value instanceof List<?> list) {
      ArrayList<Object> result = new ArrayList<>(list.size());
      for (int i = 0; i < list.size(); i++) {
        path.add((long) i);
        result.add(applyHooksAt(list.get(i), hooks, path));
        path.remove(path.size() - 1);
      }
      return result;
    }
    return value;
  }

  static final class RenderMember {
    final String key;
    final Object value;

    RenderMember(String key, Object value) {
      this.key = key;
      this.value = value;
    }
  }

  static List<RenderMember> renderMembers(Object value, boolean canonical) {
    ArrayList<RenderMember> members = new ArrayList<>();
    if (value instanceof OrderedObject object) {
      if (canonical && object.hasDuplicate()) {
        throw new IllegalArgumentException("canonical objects must not contain duplicate member names");
      }
      for (Member member : object.members()) {
        members.add(new RenderMember(member.key, member.value));
      }
    } else if (value instanceof Map<?, ?> map) {
      for (Map.Entry<?, ?> entry : map.entrySet()) {
        members.add(new RenderMember(keyString(entry.getKey()), entry.getValue()));
      }
    } else {
      throw unsupported(value);
    }
    if (canonical) {
      HashSet<String> names = new HashSet<>();
      for (RenderMember member : members) {
        if (!names.add(member.key)) {
          throw new IllegalArgumentException("canonical objects must not contain duplicate member names");
        }
      }
      members.sort(Comparator.comparing(member -> member.key, UTF16_ORDER));
    }
    return members;
  }

  static boolean isNoncharacter(int codePoint) {
    return (codePoint >= 0xfdd0 && codePoint <= 0xfdef) || (codePoint & 0xfffe) == 0xfffe;
  }

  static void validateString(String text, boolean canonical) {
    for (int i = 0; i < text.length(); i++) {
      char c = text.charAt(i);
      int codePoint;
      if (Character.isHighSurrogate(c)) {
        if (i + 1 >= text.length() || !Character.isLowSurrogate(text.charAt(i + 1))) {
          throw new IllegalArgumentException("string contains an unpaired high surrogate");
        }
        codePoint = Character.toCodePoint(c, text.charAt(++i));
      } else if (Character.isLowSurrogate(c)) {
        throw new IllegalArgumentException("string contains an unpaired low surrogate");
      } else {
        codePoint = c;
      }
      if (canonical && isNoncharacter(codePoint)) {
        throw new IllegalArgumentException("I-JSON strings must not contain Unicode noncharacters");
      }
    }
  }

  static String canonicalNumber(Object value) {
    String source;
    if (value instanceof Num num) {
      source = num.text;
    } else if (value instanceof Number number) {
      source = numberText(number);
    } else {
      throw unsupported(value);
    }
    final double number;
    try {
      number = Double.parseDouble(source);
    } catch (NumberFormatException exception) {
      throw new IllegalArgumentException("invalid canonical number: " + source, exception);
    }
    if (!Double.isFinite(number)) {
      throw new IllegalArgumentException("canonical number is not finite: " + source);
    }
    if (number == 0.0d) {
      return "0";
    }
    return ecmaNumber(number);
  }

  static String ecmaNumber(double value) {
    boolean negative = value < 0;
    if (Math.abs(value) == Double.MIN_VALUE) {
      return negative ? "-5e-324" : "5e-324";
    }
    String java = Double.toString(Math.abs(value));
    int exponentIndex = Math.max(java.indexOf('E'), java.indexOf('e'));
    String mantissa = exponentIndex < 0 ? java : java.substring(0, exponentIndex);
    int explicitExponent = exponentIndex < 0 ? 0 : Integer.parseInt(java.substring(exponentIndex + 1));
    int dot = mantissa.indexOf('.');
    int decimalPosition = (dot < 0 ? mantissa.length() : dot) + explicitExponent;
    String digits = dot < 0 ? mantissa : mantissa.substring(0, dot) + mantissa.substring(dot + 1);
    int leading = 0;
    while (leading < digits.length() - 1 && digits.charAt(leading) == '0') {
      leading++;
      decimalPosition--;
    }
    int trailing = digits.length();
    while (trailing > leading + 1 && digits.charAt(trailing - 1) == '0') {
      trailing--;
    }
    digits = digits.substring(leading, trailing);
    int k = digits.length();
    int n = decimalPosition;
    StringBuilder out = new StringBuilder(k + Math.abs(n) + 8);
    if (negative) {
      out.append('-');
    }
    if (k <= n && n <= 21) {
      out.append(digits).repeat('0', n - k);
    } else if (0 < n && n <= 21) {
      out.append(digits, 0, n).append('.').append(digits, n, k);
    } else if (-6 < n && n <= 0) {
      out.append("0.").repeat('0', -n).append(digits);
    } else {
      out.append(digits.charAt(0));
      if (k > 1) {
        out.append('.').append(digits, 1, k);
      }
      int exponent = n - 1;
      out.append('e');
      if (exponent >= 0) {
        out.append('+');
      }
      out.append(exponent);
    }
    return out.toString();
  }

  abstract static class Out {
    abstract void append(char c);
    abstract int length();
    abstract int byteLength();
    abstract void reset(int length, int bytes);

    void append(String text) {
      for (int i = 0; i < text.length(); i++) {
        char c = text.charAt(i);
        if (Character.isHighSurrogate(c)) {
          appendCodePoint(Character.toCodePoint(c, text.charAt(++i)));
        } else {
          append(c);
        }
      }
    }

    void append(String text, int start, int end) {
      for (int i = start; i < end; i++) {
        char c = text.charAt(i);
        if (Character.isHighSurrogate(c)) {
          appendCodePoint(Character.toCodePoint(c, text.charAt(++i)));
        } else {
          append(c);
        }
      }
    }

    void appendLong(long value) {
      append(Long.toString(value));
    }

    void appendDouble(double value) {
      append(Double.toString(value));
    }

    void appendCodePoint(int codePoint) {
      if (codePoint <= 0xffff) {
        append((char) codePoint);
      } else {
        append(Character.highSurrogate(codePoint));
        append(Character.lowSurrogate(codePoint));
      }
    }

    void indent(int count) {
      for (int i = 0; i < count; i++) {
        append(' ');
      }
    }
  }

  static final class StringOut extends Out {
    final StringBuilder builder;
    int bytes;

    StringOut(StringBuilder builder) {
      this.builder = builder;
    }

    @Override
    void append(char c) {
      builder.append(c);
      if (c < 0x80) {
        bytes++;
      } else if (c < 0x800) {
        bytes += 2;
      } else if (Character.isHighSurrogate(c)) {
        bytes += 4;
      } else if (!Character.isLowSurrogate(c)) {
        bytes += 3;
      }
    }

    @Override
    void append(String text) {
      builder.append(text);
      bytes += utf8Length(text);
    }

    @Override
    void append(String text, int start, int end) {
      builder.append(text, start, end);
      bytes += utf8Length(text, start, end);
    }

    @Override
    void appendLong(long value) {
      int start = builder.length();
      builder.append(value);
      bytes += builder.length() - start;
    }

    @Override
    void appendDouble(double value) {
      int start = builder.length();
      builder.append(value);
      bytes += builder.length() - start;
    }

    @Override
    void indent(int count) {
      builder.repeat(' ', count);
      bytes += count;
    }

    @Override
    int length() {
      return builder.length();
    }

    @Override
    int byteLength() {
      return bytes;
    }

    @Override
    void reset(int length, int byteLength) {
      builder.setLength(length);
      bytes = byteLength;
    }
  }

  static class ByteOut extends Out {
    byte[] bytes;
    int position;
    final char[] chars = new char[1024];
    final StringBuilder number = new StringBuilder(32);

    ByteOut(byte[] bytes) {
      this.bytes = bytes;
    }

    void ensure(int count) {
      if (position + count > bytes.length) {
        int size = Math.max(position + count, bytes.length + (bytes.length >>> 1));
        bytes = Arrays.copyOf(bytes, size);
      }
    }

    @Override
    void append(char c) {
      if (Character.isSurrogate(c)) {
        throw new IllegalArgumentException("unpaired surrogate in output");
      }
      appendCodePoint(c);
    }

    @Override
    void appendCodePoint(int codePoint) {
      if (codePoint < 0x80) {
        ensure(1);
        bytes[position++] = (byte) codePoint;
      } else if (codePoint < 0x800) {
        ensure(2);
        bytes[position++] = (byte) (0xc0 | (codePoint >>> 6));
        bytes[position++] = (byte) (0x80 | (codePoint & 0x3f));
      } else if (codePoint < 0x10000) {
        ensure(3);
        bytes[position++] = (byte) (0xe0 | (codePoint >>> 12));
        bytes[position++] = (byte) (0x80 | ((codePoint >>> 6) & 0x3f));
        bytes[position++] = (byte) (0x80 | (codePoint & 0x3f));
      } else {
        ensure(4);
        bytes[position++] = (byte) (0xf0 | (codePoint >>> 18));
        bytes[position++] = (byte) (0x80 | ((codePoint >>> 12) & 0x3f));
        bytes[position++] = (byte) (0x80 | ((codePoint >>> 6) & 0x3f));
        bytes[position++] = (byte) (0x80 | (codePoint & 0x3f));
      }
    }

    @Override
    void append(String text) {
      append(text, 0, text.length());
    }

    @SuppressWarnings("deprecation")
    @Override
    void append(String text, int start, int end) {
      int offset = start;
      if (!VECTOR || end - start < VectorScan.CHAR_THRESHOLD) {
        for (int i = start; i < end; i++) {
          char c = text.charAt(i);
          if (Character.isHighSurrogate(c)) {
            appendCodePoint(Character.toCodePoint(c, text.charAt(++i)));
          } else {
            appendCodePoint(c);
          }
        }
        return;
      }
      while (offset < end) {
        int count = Math.min(chars.length, end - offset);
        text.getChars(offset, offset + count, chars, 0);
        if (count > 0 && Character.isHighSurrogate(chars[count - 1]) && offset + count < end) {
          count--;
        }
        int i = 0;
        while (i < count) {
          if (VECTOR && count - i >= VectorScan.CHAR_THRESHOLD) {
            int encoded = VectorScan.scanAscii(chars, i, count);
            if (encoded != 0) {
              ensure(encoded);
              text.getBytes(offset + i, offset + i + encoded, bytes, position);
              position += encoded;
              i += encoded;
              continue;
            }
          }
          char c = chars[i++];
          if (Character.isHighSurrogate(c)) {
            if (i >= count || !Character.isLowSurrogate(chars[i])) {
              throw new IllegalArgumentException("unpaired surrogate in output");
            }
            appendCodePoint(Character.toCodePoint(c, chars[i++]));
          } else if (Character.isLowSurrogate(c)) {
            throw new IllegalArgumentException("unpaired surrogate in output");
          } else {
            appendCodePoint(c);
          }
        }
        offset += count;
      }
    }

    @SuppressWarnings("deprecation")
    void appendRonEscaped(String text) {
      int offset = 0;
      while (offset < text.length()) {
        int count = Math.min(chars.length, text.length() - offset);
        text.getChars(offset, offset + count, chars, 0);
        if (count > 0 && Character.isHighSurrogate(chars[count - 1])
            && offset + count < text.length()) {
          count--;
        }
        int i = 0;
        while (i < count) {
          if (count - i >= VectorScan.CHAR_THRESHOLD) {
            int clean = VectorScan.scanRonClean(chars, i, count);
            if (clean != 0) {
              ensure(clean);
              text.getBytes(offset + i, offset + i + clean, bytes, position);
              position += clean;
              i += clean;
              continue;
            }
          }
          char c = chars[i++];
          switch (c) {
            case '\\' -> append("\\\\");
            case '\b' -> append("\\b");
            case '\f' -> append("\\f");
            case '\n' -> append("\\n");
            case '\r' -> append("\\r");
            case '\t' -> append("\\t");
            default -> {
              if (c < 0x20) {
                append("\\u00");
                append(HEX[c >>> 4]);
                append(HEX[c & 15]);
              } else if (Character.isHighSurrogate(c)) {
                if (i >= count || !Character.isLowSurrogate(chars[i])) {
                  throw new IllegalArgumentException("unpaired surrogate in output");
                }
                appendCodePoint(Character.toCodePoint(c, chars[i++]));
              } else if (Character.isLowSurrogate(c)) {
                throw new IllegalArgumentException("unpaired surrogate in output");
              } else {
                appendCodePoint(c);
              }
            }
          }
        }
        offset += count;
      }
    }

    @Override
    void appendLong(long value) {
      number.setLength(0);
      number.append(value);
      ensure(number.length());
      for (int i = 0; i < number.length(); i++) {
        bytes[position++] = (byte) number.charAt(i);
      }
    }

    @Override
    void appendDouble(double value) {
      number.setLength(0);
      number.append(value);
      ensure(number.length());
      for (int i = 0; i < number.length(); i++) {
        bytes[position++] = (byte) number.charAt(i);
      }
    }

    @Override
    int length() {
      return position;
    }

    @Override
    int byteLength() {
      return position;
    }

    @Override
    void reset(int length, int ignored) {
      position = length;
    }

    byte[] result() {
      return Arrays.copyOf(bytes, position);
    }
  }

  static final class StreamOut extends ByteOut {
    OutputStream output;
    long flushed;

    StreamOut() {
      super(new byte[OUTPUT_BUFFER_SIZE]);
    }

    void bind(OutputStream output) {
      this.output = output;
      position = 0;
      flushed = 0;
    }

    void flushBuffer() {
      if (position == 0) {
        return;
      }
      try {
        output.write(bytes, 0, position);
      } catch (IOException exception) {
        throw new UncheckedIOException(exception);
      }
      flushed += position;
      position = 0;
    }

    @Override
    void ensure(int count) {
      if (count > bytes.length) {
        flushBuffer();
        bytes = new byte[count];
      } else if (position + count > bytes.length) {
        flushBuffer();
      }
    }

    @Override
    int length() {
      return (int) Math.min(Integer.MAX_VALUE, flushed + position);
    }

    @Override
    int byteLength() {
      return length();
    }

    @Override
    void reset(int length, int byteLength) {
      throw new IllegalStateException("stream output cannot rewind");
    }

    void finish() {
      flushBuffer();
      try {
        output.flush();
      } catch (IOException exception) {
        throw new UncheckedIOException(exception);
      }
    }

    void unbind() {
      output = null;
      position = 0;
      flushed = 0;
      number.setLength(0);
    }
  }

  static final class CountingOut extends Out {
    final StringBuilder number = new StringBuilder(32);
    int chars;
    int bytes;

    @Override
    void append(char c) {
      chars++;
      if (c < 0x80) {
        bytes++;
      } else if (c < 0x800) {
        bytes += 2;
      } else if (Character.isHighSurrogate(c)) {
        bytes += 4;
      } else if (!Character.isLowSurrogate(c)) {
        bytes += 3;
      }
    }

    @Override
    void append(String text) {
      chars += text.length();
      bytes += utf8Length(text);
    }

    @Override
    void append(String text, int start, int end) {
      chars += end - start;
      bytes += utf8Length(text, start, end);
    }

    @Override
    void appendLong(long value) {
      number.setLength(0);
      number.append(value);
      chars += number.length();
      bytes += number.length();
    }

    @Override
    void appendDouble(double value) {
      number.setLength(0);
      number.append(value);
      chars += number.length();
      bytes += number.length();
    }

    @Override
    void indent(int count) {
      chars += count;
      bytes += count;
    }

    @Override
    int length() {
      return chars;
    }

    @Override
    int byteLength() {
      return bytes;
    }

    @Override
    void reset(int length, int byteLength) {
      chars = length;
      bytes = byteLength;
    }

    void clear() {
      chars = 0;
      bytes = 0;
      number.setLength(0);
    }
  }

  static int utf8Length(CharSequence text) {
    int bytes = 0;
    for (int i = 0; i < text.length(); i++) {
      char c = text.charAt(i);
      if (c < 0x80) {
        bytes++;
      } else if (c < 0x800) {
        bytes += 2;
      } else if (Character.isHighSurrogate(c)) {
        if (i + 1 >= text.length() || !Character.isLowSurrogate(text.charAt(i + 1))) {
          throw new IllegalArgumentException("unpaired surrogate in output");
        }
        bytes += 4;
        i++;
      } else if (Character.isLowSurrogate(c)) {
        throw new IllegalArgumentException("unpaired surrogate in output");
      } else {
        bytes += 3;
      }
    }
    return bytes;
  }

  static int utf8Length(String text, int start, int end) {
    int bytes = 0;
    for (int i = start; i < end; i++) {
      char c = text.charAt(i);
      if (c < 0x80) {
        bytes++;
      } else if (c < 0x800) {
        bytes += 2;
      } else if (Character.isHighSurrogate(c)) {
        if (i + 1 >= end || !Character.isLowSurrogate(text.charAt(i + 1))) {
          throw new IllegalArgumentException("unpaired surrogate in output");
        }
        bytes += 4;
        i++;
      } else if (Character.isLowSurrogate(c)) {
        throw new IllegalArgumentException("unpaired surrogate in output");
      } else {
        bytes += 3;
      }
    }
    return bytes;
  }

  static final class Renderer {
    final Out out;
    final Mode mode;
    final boolean canonical;
    final int maxDepth;
    final Renderer inlineProbe;
    int depth;

    Renderer(Out out, Mode mode, int maxDepth) {
      this(out, mode, maxDepth, true);
    }

    Renderer(Out out, Mode mode, int maxDepth, boolean createProbe) {
      this.out = out;
      this.mode = mode;
      this.canonical = mode == Mode.CANONICAL;
      this.maxDepth = checkedDepth(maxDepth);
      this.inlineProbe = createProbe && mode == Mode.PRETTY && out instanceof StreamOut
          ? new Renderer(new CountingOut(), mode, maxDepth, false)
          : null;
    }

    void enterContainer() {
      if (depth >= maxDepth) {
        throw new IllegalArgumentException("maximum nesting depth exceeded");
      }
      depth++;
    }

    void leaveContainer() {
      depth--;
    }

    void json(Object value) {
      if (mode == Mode.PRETTY) {
        jsonPretty(value, 0);
      } else {
        jsonCompact(value);
      }
    }

    void jsonCompact(Object value) {
      if (value == null) {
        out.append("null");
      } else if (value instanceof Boolean bool) {
        out.append(bool ? "true" : "false");
      } else if (value instanceof Num || value instanceof Number) {
        writeNumber(value);
      } else if (value instanceof String text) {
        jsonString(text);
      } else if (value instanceof Keyword || value instanceof Symbol) {
        jsonString(keyString(value));
      } else if (isListValue(value)) {
        enterContainer();
        try {
          List<?> list = listValue(value);
          out.append('[');
          for (int i = 0; i < list.size(); i++) {
            if (i != 0) {
              out.append(',');
            }
            jsonCompact(list.get(i));
          }
          out.append(']');
        } finally {
          leaveContainer();
        }
      } else if (isMapValue(value)) {
        enterContainer();
        try {
          List<RenderMember> members = renderMembers(value, canonical);
          out.append('{');
          for (int i = 0; i < members.size(); i++) {
            if (i != 0) {
              out.append(',');
            }
            RenderMember member = members.get(i);
            jsonString(member.key);
            out.append(':');
            jsonCompact(member.value);
          }
          out.append('}');
        } finally {
          leaveContainer();
        }
      } else {
        throw unsupported(value);
      }
    }

    void jsonPretty(Object value, int indent) {
      if (isListValue(value)) {
        enterContainer();
        try {
          List<?> list = listValue(value);
          if (list.isEmpty()) {
            out.append("[]");
            return;
          }
          out.append("[\n");
          for (int i = 0; i < list.size(); i++) {
            out.indent(indent + 2);
            jsonPretty(list.get(i), indent + 2);
            out.append(i + 1 < list.size() ? ",\n" : "\n");
          }
          out.indent(indent);
          out.append(']');
        } finally {
          leaveContainer();
        }
      } else if (isMapValue(value)) {
        enterContainer();
        try {
          List<RenderMember> members = renderMembers(value, false);
          if (members.isEmpty()) {
            out.append("{}");
            return;
          }
          out.append("{\n");
          for (int i = 0; i < members.size(); i++) {
            RenderMember member = members.get(i);
            out.indent(indent + 2);
            jsonString(member.key);
            out.append(": ");
            jsonPretty(member.value, indent + 2);
            out.append(i + 1 < members.size() ? ",\n" : "\n");
          }
          out.indent(indent);
          out.append('}');
        } finally {
          leaveContainer();
        }
      } else {
        jsonCompact(value);
      }
    }

    void jsonString(String text) {
      validateString(text, canonical);
      out.append('\"');
      int clean = 0;
      for (int i = 0; i < text.length(); i++) {
        char c = text.charAt(i);
        if (c != '\"' && c != '\\' && c >= 0x20 && !Character.isSurrogate(c)) {
          continue;
        }
        if (clean < i) {
          out.append(text, clean, i);
        }
        switch (c) {
          case '\"' -> out.append("\\\"");
          case '\\' -> out.append("\\\\");
          case '\b' -> out.append("\\b");
          case '\f' -> out.append("\\f");
          case '\n' -> out.append("\\n");
          case '\r' -> out.append("\\r");
          case '\t' -> out.append("\\t");
          default -> {
            if (c < 0x20) {
              out.append("\\u00");
              out.append(HEX[c >>> 4]);
              out.append(HEX[c & 15]);
            } else {
              out.appendCodePoint(Character.toCodePoint(c, text.charAt(++i)));
            }
          }
        }
        clean = i + 1;
      }
      if (clean < text.length()) {
        out.append(text, clean, text.length());
      }
      out.append('\"');
    }

    void writeNumber(Object value) {
      if (canonical) {
        out.append(canonicalNumber(value));
        return;
      }
      if (value instanceof Num num) {
        out.append(num.text);
        return;
      }
      Number number = (Number) value;
      if (number instanceof Ratio) {
        throw new IllegalArgumentException("ratios have no RON representation");
      }
      if (number instanceof Double || number instanceof Float) {
        double doubleValue = number.doubleValue();
        if (!Double.isFinite(doubleValue)) {
          throw new IllegalArgumentException("NaN and infinity have no RON representation");
        }
        out.appendDouble(doubleValue);
      } else if (number instanceof Byte || number instanceof Short
          || number instanceof Integer || number instanceof Long) {
        out.appendLong(number.longValue());
      } else if (number instanceof BigInt || number instanceof BigInteger
          || number instanceof BigDecimal) {
        out.append(number.toString());
      } else {
        out.append(numberText(number));
      }
    }

    void ron(Object value) {
      if (mode == Mode.PRETTY) {
        ronPrettyRoot(value);
      } else if (isMapValue(value) && !((Map<?, ?>) value).isEmpty()) {
        enterContainer();
        try {
          ronCompactMembers(value);
        } finally {
          leaveContainer();
        }
      } else {
        ronCompactValue(value);
      }
    }

    void ronCompactValue(Object value) {
      if (value == null) {
        out.append("null");
      } else if (value instanceof Boolean bool) {
        out.append(bool ? "true" : "false");
      } else if (value instanceof Num || value instanceof Number) {
        writeNumber(value);
      } else if (value instanceof String text) {
        ronString(text, false);
      } else if (value instanceof Keyword || value instanceof Symbol) {
        ronString(keyString(value), false);
      } else if (isListValue(value)) {
        enterContainer();
        try {
          List<?> list = listValue(value);
          out.append('[');
          for (int i = 0; i < list.size(); i++) {
            if (i != 0) {
              out.append(' ');
            }
            ronCompactValue(list.get(i));
          }
          out.append(']');
        } finally {
          leaveContainer();
        }
      } else if (isMapValue(value)) {
        enterContainer();
        try {
          out.append('{');
          ronCompactMembers(value);
          out.append('}');
        } finally {
          leaveContainer();
        }
      } else {
        throw unsupported(value);
      }
    }

    void ronCompactMembers(Object value) {
      if (!canonical
          && (!(out instanceof StreamOut) || hasOnlyStringKeys(value))) {
        int length = out.length();
        int bytes = out.byteLength();
        if (ronCompactStringMembers(value)) {
          return;
        }
        out.reset(length, bytes);
      }
      List<RenderMember> members = renderMembers(value, canonical);
      for (int i = 0; i < members.size(); i++) {
        if (i != 0) {
          out.append(' ');
        }
        RenderMember member = members.get(i);
        ronString(member.key, true);
        if (!startsStructural(member.value)) {
          out.append(' ');
        }
        ronCompactValue(member.value);
      }
    }

    boolean ronCompactStringMembers(Object value) {
      boolean first = true;
      for (Map.Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
        if (!(entry.getKey() instanceof String key)) {
          return false;
        }
        if (first) {
          first = false;
        } else {
          out.append(' ');
        }
        ronString(key, true);
        Object entryValue = entry.getValue();
        if (!startsStructural(entryValue)) {
          out.append(' ');
        }
        ronCompactValue(entryValue);
      }
      return true;
    }

    boolean startsStructural(Object value) {
      if (isMapValue(value) || isListValue(value)) {
        return true;
      }
      if (value instanceof String text) {
        return !ronBare(text, false);
      }
      if (value instanceof Keyword || value instanceof Symbol) {
        return !ronBare(keyString(value), false);
      }
      return false;
    }

    void ronPrettyRoot(Object value) {
      if (isMapValue(value)) {
        enterContainer();
        try {
          List<RenderMember> members = renderMembers(value, false);
          if (members.isEmpty()) {
            out.append("{}\n");
            return;
          }
          for (RenderMember member : members) {
            ronString(member.key, true);
            out.append(' ');
            if (!ronInline(member.value)) {
              ronPrettyContainer(member.value, 0);
            }
            out.append('\n');
          }
          return;
        } finally {
          leaveContainer();
        }
      }
      if (!ronInline(value)) {
        ronPrettyContainer(value, 0);
      }
      out.append('\n');
    }

    boolean ronInline(Object value) {
      if (inlineProbe != null) {
        CountingOut probeOut = (CountingOut) inlineProbe.out;
        probeOut.clear();
        inlineProbe.depth = depth;
        boolean inline = inlineProbe.ronInline(value);
        if (inline) {
          ronCompactValue(value);
        }
        return inline;
      }
      int length = out.length();
      int bytes = out.byteLength();
      if (isListValue(value)) {
        enterContainer();
        try {
          List<?> list = listValue(value);
          out.append('[');
          for (int i = 0; i < list.size(); i++) {
            if (i != 0) {
              out.append(' ');
            }
            if (!ronInline(list.get(i))) {
              out.reset(length, bytes);
              return false;
            }
          }
          out.append(']');
        } finally {
          leaveContainer();
        }
      } else if (isMapValue(value)) {
        enterContainer();
        try {
          List<RenderMember> members = renderMembers(value, false);
          if (members.isEmpty()) {
            out.append("{}");
          } else if (members.size() == 1) {
            RenderMember member = members.get(0);
            out.append('{');
            ronString(member.key, true);
            out.append(' ');
            if (!ronInline(member.value)) {
              out.reset(length, bytes);
              return false;
            }
            out.append('}');
          } else {
            return false;
          }
        } finally {
          leaveContainer();
        }
      } else {
        ronCompactValue(value);
      }
      if (out.byteLength() - bytes > INLINE_LIMIT) {
        out.reset(length, bytes);
        return false;
      }
      return true;
    }

    void ronPrettyContainer(Object value, int indent) {
      if (isListValue(value)) {
        enterContainer();
        try {
          List<?> list = listValue(value);
          out.append("[\n");
          for (Object element : list) {
            out.indent(indent + 2);
            if (!ronInline(element)) {
              ronPrettyContainer(element, indent + 2);
            }
            out.append('\n');
          }
          out.indent(indent);
          out.append(']');
        } finally {
          leaveContainer();
        }
      } else if (isMapValue(value)) {
        enterContainer();
        try {
          List<RenderMember> members = renderMembers(value, false);
          out.append("{\n");
          for (RenderMember member : members) {
            out.indent(indent + 2);
            ronString(member.key, true);
            out.append(' ');
            if (!ronInline(member.value)) {
              ronPrettyContainer(member.value, indent + 2);
            }
            out.append('\n');
          }
          out.indent(indent);
          out.append('}');
        } finally {
          leaveContainer();
        }
      } else {
        ronCompactValue(value);
      }
    }

    boolean ronBare(String text, boolean key) {
      if (text.isEmpty()) {
        return false;
      }
      boolean escaped = false;
      for (int i = 0; i < text.length(); i++) {
        char c = text.charAt(i);
        if (c == '\\' || c < 0x20) {
          escaped = true;
          continue;
        }
        if (isDelimiter(c)) {
          return false;
        }
        if (Character.isHighSurrogate(c)) {
          i++;
        }
      }
      if (key || escaped) {
        return true;
      }
      if (text.equals("true") || text.equals("false") || text.equals("null")) {
        return false;
      }
      return !isNumberShaped(text);
    }

    void ronString(String text, boolean key) {
      if (VECTOR && text.length() >= VectorScan.CHAR_THRESHOLD && out instanceof ByteOut) {
        ronStringVector(text, key);
        return;
      }
      boolean bare = !text.isEmpty();
      boolean escaped = false;
      boolean needsEscaping = false;
      int delimiter = 0;
      int quoteRun = 0;
      for (int i = 0; i < text.length(); i++) {
        char c = text.charAt(i);
        int codePoint;
        if (Character.isHighSurrogate(c)) {
          if (i + 1 >= text.length() || !Character.isLowSurrogate(text.charAt(i + 1))) {
            throw new IllegalArgumentException("string contains an unpaired high surrogate");
          }
          codePoint = Character.toCodePoint(c, text.charAt(++i));
        } else if (Character.isLowSurrogate(c)) {
          throw new IllegalArgumentException("string contains an unpaired low surrogate");
        } else {
          codePoint = c;
        }
        if (canonical && isNoncharacter(codePoint)) {
          throw new IllegalArgumentException("I-JSON strings must not contain Unicode noncharacters");
        }
        if (c == '\\' || c < 0x20) {
          escaped = true;
          needsEscaping = true;
          quoteRun = 0;
          continue;
        }
        if (isDelimiter(c)) {
          bare = false;
        }
        if (c == '\'') {
          delimiter = Math.max(delimiter, ++quoteRun);
        } else {
          quoteRun = 0;
        }
      }
      if (bare && !key && !escaped
          && (text.equals("true") || text.equals("false") || text.equals("null")
              || isNumberShaped(text))) {
        bare = false;
      }
      if (!bare) {
        delimiter++;
        for (int i = 0; i < delimiter; i++) {
          out.append('\'');
        }
      }
      if (needsEscaping) {
        ronEscaped(text);
      } else {
        out.append(text);
      }
      if (!bare) {
        for (int i = 0; i < delimiter; i++) {
          out.append('\'');
        }
      }
    }

    void ronStringVector(String text, boolean key) {
      validateString(text, canonical);
      boolean bare = ronBare(text, key);
      int delimiter = 0;
      if (!bare) {
        int quote = text.indexOf('\'');
        while (quote >= 0) {
          int end = quote + 1;
          while (end < text.length() && text.charAt(end) == '\'') {
            end++;
          }
          delimiter = Math.max(delimiter, end - quote);
          quote = text.indexOf('\'', end);
        }
        delimiter++;
        for (int i = 0; i < delimiter; i++) {
          out.append('\'');
        }
      }
      ronEscaped(text);
      if (!bare) {
        for (int i = 0; i < delimiter; i++) {
          out.append('\'');
        }
      }
    }

    void ronEscaped(String text) {
      if (VECTOR && text.length() >= VectorScan.CHAR_THRESHOLD && out instanceof ByteOut byteOut) {
        byteOut.appendRonEscaped(text);
        return;
      }
      int clean = 0;
      for (int i = 0; i < text.length(); i++) {
        char c = text.charAt(i);
        if (c != '\\' && c >= 0x20 && !Character.isSurrogate(c)) {
          continue;
        }
        if (clean < i) {
          out.append(text, clean, i);
        }
        switch (c) {
          case '\\' -> out.append("\\\\");
          case '\b' -> out.append("\\b");
          case '\f' -> out.append("\\f");
          case '\n' -> out.append("\\n");
          case '\r' -> out.append("\\r");
          case '\t' -> out.append("\\t");
          default -> {
            if (c < 0x20) {
              out.append("\\u00");
              out.append(HEX[c >>> 4]);
              out.append(HEX[c & 15]);
            } else {
              out.appendCodePoint(Character.toCodePoint(c, text.charAt(++i)));
            }
          }
        }
        clean = i + 1;
      }
      if (clean < text.length()) {
        out.append(text, clean, text.length());
      }
    }
  }

  static StringBuilder acquireBuilder() {
    StringBuilder builder = STRING_BUILDERS.poll();
    return builder == null ? new StringBuilder(1024) : builder;
  }

  static void releaseBuilder(StringBuilder builder) {
    if (builder.capacity() <= MAX_RETAINED_OUTPUT) {
      builder.setLength(0);
      STRING_BUILDERS.offer(builder);
    }
  }

  static ByteOut acquireByteOut() {
    ByteOut out = BYTE_OUTPUTS.poll();
    if (out == null) {
      return new ByteOut(new byte[OUTPUT_BUFFER_SIZE]);
    }
    out.position = 0;
    return out;
  }

  static void releaseByteOut(ByteOut out) {
    if (out.bytes.length <= MAX_RETAINED_OUTPUT) {
      out.position = 0;
      out.number.setLength(0);
      BYTE_OUTPUTS.offer(out);
    }
  }

  static StreamOut acquireStreamOut(OutputStream output) {
    StreamOut out = STREAM_OUTPUTS.poll();
    if (out == null) {
      out = new StreamOut();
    }
    out.bind(output);
    return out;
  }

  static void releaseStreamOut(StreamOut out) {
    out.unbind();
    if (out.bytes.length != OUTPUT_BUFFER_SIZE) {
      out.bytes = new byte[OUTPUT_BUFFER_SIZE];
    }
    STREAM_OUTPUTS.offer(out);
  }

  static String renderString(Object value, Mode mode, boolean ron) {
    return renderString(value, mode, ron, DEFAULT_MAX_DEPTH);
  }

  static String renderString(Object value, Mode mode, boolean ron, int maxDepth) {
    StringBuilder builder = acquireBuilder();
    try {
      Renderer renderer = new Renderer(new StringOut(builder), mode, maxDepth);
      if (ron) {
        renderer.ron(value);
      } else {
        renderer.json(value);
      }
      return builder.toString();
    } finally {
      releaseBuilder(builder);
    }
  }

  static byte[] renderBytes(Object value, Mode mode, boolean ron) {
    return renderBytes(value, mode, ron, DEFAULT_MAX_DEPTH);
  }

  static byte[] renderBytes(Object value, Mode mode, boolean ron, int maxDepth) {
    ByteOut out = acquireByteOut();
    try {
      Renderer renderer = new Renderer(out, mode, maxDepth);
      if (ron) {
        renderer.ron(value);
      } else {
        renderer.json(value);
      }
      return out.result();
    } finally {
      releaseByteOut(out);
    }
  }

  static void renderToOutput(Object value, OutputStream output, Mode mode) throws IOException {
    renderToOutput(value, output, mode, DEFAULT_MAX_DEPTH);
  }

  static void renderToOutput(Object value, OutputStream output, Mode mode, int maxDepth)
      throws IOException {
    StreamOut out = acquireStreamOut(output);
    try {
      new Renderer(out, mode, maxDepth).ron(value);
      out.finish();
    } catch (UncheckedIOException exception) {
      throw exception.getCause();
    } finally {
      releaseStreamOut(out);
    }
  }

  static int checkedDepth(int maxDepth) {
    if (maxDepth <= 0) {
      throw new IllegalArgumentException("maxDepth must be a positive integer");
    }
    return maxDepth;
  }

  public static Object parseRon(String source) {
    return parseRon(source, DEFAULT_MAX_DEPTH);
  }

  public static Object parseRon(String source, int maxDepth) {
    return new CharRonParser(source, false, null, 0, checkedDepth(maxDepth)).parseDocument();
  }

  public static Object parseRon(byte[] source) {
    return parseRon(source, DEFAULT_MAX_DEPTH);
  }

  public static Object parseRon(byte[] source, int maxDepth) {
    return new ByteRonParser(source, false, null, 0, checkedDepth(maxDepth)).parseDocument();
  }

  public static Object parseJson(String source) {
    return parseJson(source, DEFAULT_MAX_DEPTH);
  }

  public static Object parseJson(String source, int maxDepth) {
    return new CharJsonParser(source, checkedDepth(maxDepth)).parseDocument();
  }

  public static Object parseJson(byte[] source) {
    return parseJson(source, DEFAULT_MAX_DEPTH);
  }

  public static Object parseJson(byte[] source, int maxDepth) {
    return new ByteJsonParser(source, checkedDepth(maxDepth)).parseDocument();
  }

  public static Object readRon(String source, IFn keyFn, int keyMode, int maxDepth) {
    return new CharRonParser(source, true, keyFn, keyMode, checkedDepth(maxDepth)).parseDocument();
  }

  public static Object readRon(byte[] source, IFn keyFn, int keyMode, int maxDepth) {
    return new ByteRonParser(source, true, keyFn, keyMode, checkedDepth(maxDepth)).parseDocument();
  }

  public static String writeJson(Object model, Mode mode) {
    return writeJson(model, mode, DEFAULT_MAX_DEPTH);
  }

  public static String writeJson(Object model, Mode mode, int maxDepth) {
    return renderString(model, mode, false, maxDepth);
  }

  @Deprecated
  public static String writeJson(Object model, boolean pretty) {
    return writeJson(model, pretty ? Mode.PRETTY : Mode.COMPACT);
  }

  public static String writeRon(Object model, Mode mode) {
    return writeRon(model, mode, DEFAULT_MAX_DEPTH);
  }

  public static String writeRon(Object model, Mode mode, int maxDepth) {
    return renderString(model, mode, true, maxDepth);
  }

  @Deprecated
  public static String writeRon(Object model, boolean pretty) {
    return writeRon(model, pretty ? Mode.PRETTY : Mode.COMPACT);
  }

  public static byte[] writeJsonBytes(Object model, Mode mode) {
    return writeJsonBytes(model, mode, DEFAULT_MAX_DEPTH);
  }

  public static byte[] writeJsonBytes(Object model, Mode mode, int maxDepth) {
    return renderBytes(model, mode, false, maxDepth);
  }

  public static byte[] writeRonBytes(Object model, Mode mode) {
    return writeRonBytes(model, mode, DEFAULT_MAX_DEPTH);
  }

  public static byte[] writeRonBytes(Object model, Mode mode, int maxDepth) {
    return renderBytes(model, mode, true, maxDepth);
  }

  public static String ronToJson(String source, Mode mode) {
    return writeJson(parseRon(source), mode);
  }

  @Deprecated
  public static String ronToJson(String source, boolean pretty) {
    return ronToJson(source, pretty ? Mode.PRETTY : Mode.COMPACT);
  }

  public static byte[] ronToJson(byte[] source, Mode mode, int maxDepth) {
    return writeJsonBytes(parseRon(source, maxDepth), mode, maxDepth);
  }

  public static String jsonToRon(String source, Mode mode) {
    return jsonToRon(source, mode, List.of());
  }

  public static String jsonToRon(String source, Mode mode, List<Hook> hooks) {
    return writeRon(applyHooks(parseJson(source), hooks), mode);
  }

  public static String jsonToRon(String source, Mode mode, List<Hook> hooks, int maxDepth) {
    return writeRon(applyHooks(parseJson(source, maxDepth), hooks), mode, maxDepth);
  }

  @Deprecated
  public static String jsonToRon(String source, boolean pretty) {
    return jsonToRon(source, pretty ? Mode.PRETTY : Mode.COMPACT);
  }

  public static byte[] jsonToRon(byte[] source, Mode mode, List<Hook> hooks, int maxDepth) {
    return writeRonBytes(applyHooks(parseJson(source, maxDepth), hooks), mode, maxDepth);
  }

  public static String writeData(Object data, Mode mode) {
    return writeData(data, mode, DEFAULT_MAX_DEPTH);
  }

  public static String writeData(Object data, Mode mode, int maxDepth) {
    return renderString(data, mode, true, maxDepth);
  }

  public static byte[] writeDataBytes(Object data, Mode mode) {
    return writeDataBytes(data, mode, DEFAULT_MAX_DEPTH);
  }

  public static byte[] writeDataBytes(Object data, Mode mode, int maxDepth) {
    return renderBytes(data, mode, true, maxDepth);
  }

  public static OutputStream writeData(Object data, OutputStream output, Mode mode) throws IOException {
    return writeData(data, output, mode, DEFAULT_MAX_DEPTH);
  }

  public static OutputStream writeData(Object data, OutputStream output, Mode mode, int maxDepth)
      throws IOException {
    renderToOutput(data, output, mode, maxDepth);
    return output;
  }
}

/* All direct incubator API references stay in this package-private class. */
final class VectorScan {
  static final jdk.incubator.vector.VectorSpecies<Byte> SPECIES =
      jdk.incubator.vector.ByteVector.SPECIES_PREFERRED;
  static final int LANES = SPECIES.length();
  private static final jdk.incubator.vector.VectorSpecies<Short> CHAR_SPECIES =
      jdk.incubator.vector.ShortVector.SPECIES_128;
  static final int CHAR_LANES = CHAR_SPECIES.length();
  static final int CHAR_THRESHOLD = 64;

  private VectorScan() {}

  static int scanRonToken(byte[] bytes, int position, int end) {
    int p = position;
    while (end - p >= LANES) {
      jdk.incubator.vector.ByteVector vector =
          jdk.incubator.vector.ByteVector.fromArray(SPECIES, bytes, p);
      long special = vector.compare(jdk.incubator.vector.VectorOperators.ULT, (byte) 0x21)
          .or(vector.lt((byte) 0))
          .or(vector.eq((byte) ','))
          .or(vector.eq((byte) '{'))
          .or(vector.eq((byte) '}'))
          .or(vector.eq((byte) '['))
          .or(vector.eq((byte) ']'))
          .or(vector.eq((byte) '"'))
          .or(vector.eq((byte) '\''))
          .or(vector.eq((byte) '\\'))
          .toLong();
      if (special != 0) {
        return p + Long.numberOfTrailingZeros(special);
      }
      p += LANES;
    }
    return p;
  }

  static int scanJsonString(byte[] bytes, int position, int end) {
    int p = position;
    while (end - p >= LANES) {
      jdk.incubator.vector.ByteVector vector =
          jdk.incubator.vector.ByteVector.fromArray(SPECIES, bytes, p);
      long special = vector.compare(jdk.incubator.vector.VectorOperators.ULT, (byte) 0x20)
          .or(vector.lt((byte) 0))
          .or(vector.eq((byte) '"'))
          .or(vector.eq((byte) '\\'))
          .toLong();
      if (special != 0) {
        return p + Long.numberOfTrailingZeros(special);
      }
      p += LANES;
    }
    return p;
  }

  static int scanRonQuoted(byte[] bytes, int position, int end, byte quote) {
    int p = position;
    while (end - p >= LANES) {
      jdk.incubator.vector.ByteVector vector =
          jdk.incubator.vector.ByteVector.fromArray(SPECIES, bytes, p);
      long special = vector.compare(jdk.incubator.vector.VectorOperators.ULT, (byte) 0x20)
          .or(vector.lt((byte) 0))
          .or(vector.eq(quote))
          .or(vector.eq((byte) '\\'))
          .toLong();
      if (special != 0) {
        return p + Long.numberOfTrailingZeros(special);
      }
      p += LANES;
    }
    return p;
  }

  static int scanAscii(char[] chars, int position, int end) {
    int p = position;
    while (end - p >= CHAR_LANES) {
      jdk.incubator.vector.ShortVector vector =
          jdk.incubator.vector.ShortVector.fromCharArray(CHAR_SPECIES, chars, p);
      long high = vector.compare(jdk.incubator.vector.VectorOperators.UGE, (short) 0x80).toLong();
      if (high != 0) {
        return p - position + Long.numberOfTrailingZeros(high);
      }
      p += CHAR_LANES;
    }
    return p - position;
  }

  static int scanRonClean(char[] chars, int position, int end) {
    int p = position;
    while (end - p >= CHAR_LANES) {
      jdk.incubator.vector.ShortVector vector =
          jdk.incubator.vector.ShortVector.fromCharArray(CHAR_SPECIES, chars, p);
      long special = vector.lt((short) 0x20)
          .or(vector.eq((short) '\\'))
          .or(vector.compare(jdk.incubator.vector.VectorOperators.UGE, (short) 0x80))
          .toLong();
      if (special != 0) {
        return p - position + Long.numberOfTrailingZeros(special);
      }
      p += CHAR_LANES;
    }
    return p - position;
  }
}
