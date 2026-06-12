package ol.ron;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * RON (Readable Object Notation) parser and renderer.
 * https://github.com/starfederation/ron
 *
 * Follows the parsing technique of tonsky/fast-edn: scan a char[] with index
 * arithmetic, classify characters through small lookup tables, take an
 * allocation-free fast path through tokens and construct Strings directly
 * from buffer slices.
 *
 * The value model is the JSON value model: null, Boolean, Num (number kept
 * as source text so RON -> JSON -> RON conversion is byte-exact), String,
 * ArrayList and HashMap. Renderers sort object keys by Unicode code point.
 */
public final class Ron {

  /** A JSON/RON number, preserved as source text. */
  public static final class Num {
    public final String text;

    public Num(String text) {
      this.text = text;
    }

    @Override
    public String toString() {
      return text;
    }

    @Override
    public boolean equals(Object o) {
      return o instanceof Num && ((Num) o).text.equals(text);
    }

    @Override
    public int hashCode() {
      return text.hashCode();
    }
  }

  public static class ParseException extends RuntimeException {
    public final int offset;

    public ParseException(String message, int offset) {
      // no stack trace: parse failure is also used for elided-object fallback
      super(message + " at offset " + offset, null, false, false);
      this.offset = offset;
    }
  }

  ///////////////////////
  // Character classes //
  ///////////////////////

  // structural delimiters + ASCII whitespace; ends bare tokens
  private static final boolean[] DELIM = new boolean[128];
  // ASCII whitespace (no comma)
  private static final boolean[] WS = new boolean[128];

  static {
    for (char c : new char[] {'\t', '\n', '\r', ' '}) {
      WS[c] = true;
    }
    for (char c : new char[] {'\t', '\n', '\r', ' ', ',', '{', '}', '[', ']', '"', '\''}) {
      DELIM[c] = true;
    }
  }

  static boolean isWs(char c) {
    return c < 128 ? WS[c] : isUnicodeWs(c);
  }

  static boolean isDelim(char c) {
    return c < 128 ? DELIM[c] : isUnicodeWs(c);
  }

  static boolean isUnicodeWs(char c) {
    return Character.isWhitespace(c) || Character.isSpaceChar(c);
  }

  ////////////////
  // RON parser //
  ////////////////

  static final class RonParser {
    final char[] buf;
    final int len;
    int pos;

    RonParser(char[] buf) {
      this.buf = buf;
      this.len = buf.length;
    }

    ParseException err(String message, int offset) {
      return new ParseException(message, offset);
    }

    Object parseDocument() {
      int p = pos;
      while (p < len && (isWs(buf[p]) || buf[p] == ',')) {
        p++;
      }
      if (p == len) {
        throw err("empty input", p);
      }
      Object v;
      char c = buf[p];
      if (c == '{' || c == '[') {
        pos = p;
        v = parseValue();
      } else {
        try {
          pos = 0;
          v = parseElidedObject();
        } catch (ParseException e) {
          pos = 0;
          v = parseValue();
        }
      }
      p = pos;
      while (p < len && (isWs(buf[p]) || buf[p] == ',')) {
        p++;
      }
      if (p < len) {
        throw err("trailing data", p);
      }
      return v;
    }

    void skipWhitespace() {
      int p = pos;
      while (p < len && isWs(buf[p])) {
        p++;
      }
      pos = p;
    }

    // whitespace plus optional commas; only valid AFTER a value
    void skipSeparators() {
      int p = pos;
      while (p < len && (isWs(buf[p]) || buf[p] == ',')) {
        p++;
      }
      pos = p;
    }

    Object parseElidedObject() {
      HashMap<String, Object> m = new HashMap<>();
      skipWhitespace();
      if (pos == len) {
        throw err("empty input", pos);
      }
      while (true) {
        String k = parseKey();
        skipWhitespace();
        Object v = parseValue();
        m.put(k, v);
        skipSeparators();
        if (pos == len) {
          return m;
        }
      }
    }

    Object parseValue() {
      if (pos == len) {
        throw err("unexpected EOF, expected value", pos);
      }
      char c = buf[pos];
      switch (c) {
        case '{':
          return parseObject();
        case '[':
          return parseArray();
        case '"':
        case '\'':
          return parseQuoted();
        case ',':
          return parseCommaToken();
        case '}':
        case ']':
          throw err("unexpected '" + c + "', expected value", pos);
        default:
          return parseBareValue();
      }
    }

    String parseKey() {
      char c = buf[pos];
      switch (c) {
        case '"':
        case '\'':
          return parseQuoted();
        case ',':
          return parseCommaToken();
        case '{':
        case '}':
        case '[':
        case ']':
          throw err("unexpected '" + c + "', expected object key", pos);
        default: {
          int start = pos;
          int p = pos;
          while (p < len && !isDelim(buf[p])) {
            p++;
          }
          if (p == start) {
            throw err("empty object key", pos);
          }
          pos = p;
          return new String(buf, start, p - start);
        }
      }
    }

    Object parseObject() {
      pos++; // consume '{'
      HashMap<String, Object> m = new HashMap<>();
      skipWhitespace();
      if (pos < len && buf[pos] == '}') {
        pos++;
        return m;
      }
      while (true) {
        if (pos == len) {
          throw err("unterminated object", pos);
        }
        String k = parseKey();
        skipWhitespace();
        Object v = parseValue();
        m.put(k, v);
        skipSeparators();
        if (pos == len) {
          throw err("unterminated object", pos);
        }
        if (buf[pos] == '}') {
          pos++;
          return m;
        }
      }
    }

    Object parseArray() {
      pos++; // consume '['
      ArrayList<Object> a = new ArrayList<>();
      skipWhitespace();
      if (pos < len && buf[pos] == ']') {
        pos++;
        return a;
      }
      while (true) {
        if (pos == len) {
          throw err("unterminated array", pos);
        }
        a.add(parseValue());
        skipSeparators();
        if (pos == len) {
          throw err("unterminated array", pos);
        }
        if (buf[pos] == ']') {
          pos++;
          return a;
        }
      }
    }

    // a key or value starting with ',' is a string token running to a delimiter
    String parseCommaToken() {
      int start = pos;
      int p = pos + 1;
      while (p < len && !isDelim(buf[p])) {
        p++;
      }
      pos = p;
      return new String(buf, start, p - start);
    }

    String parseQuoted() {
      char q = buf[pos];
      int start = pos;
      int p = pos;
      while (p < len && buf[p] == q) {
        p++;
      }
      int n = p - start;
      if (p == len || isDelim(buf[p])) {
        if (n % 2 == 0) {
          pos = p;
          return "";
        }
        if (n >= 5 && (n - 2) % 3 == 0) {
          pos = p;
          char[] out = new char[(n - 2) / 3];
          Arrays.fill(out, q);
          return new String(out);
        }
      }
      int contentStart = p;
      while (p < len) {
        char c = buf[p];
        if (c == q) {
          int runStart = p;
          while (p < len && buf[p] == q) {
            p++;
          }
          if (p - runStart >= n) {
            // consume exactly n closing quotes; surplus quotes start the next token
            pos = runStart + n;
            return new String(buf, contentStart, runStart - contentStart);
          }
        } else if (c == '\n' || c == '\r') {
          // quoted strings are single-line: a lone ' before EOL is the token ',
          // not the start of a string closed by a quote on some later line
          break;
        } else {
          p++;
        }
      }
      // apostrophe compatibility: a lone ' followed by EOF or delimiter is the string "'"
      if (q == '\'' && (start + 1 == len || isDelim(buf[start + 1]))) {
        pos = start + 1;
        return "'";
      }
      throw err("unterminated string", start);
    }

    Object parseBareValue() {
      int start = pos;
      int p = pos;
      while (p < len && !isDelim(buf[p])) {
        p++;
      }
      pos = p;
      int n = p - start;
      char c = buf[start];
      if (n == 4 && c == 't'
          && buf[start + 1] == 'r' && buf[start + 2] == 'u' && buf[start + 3] == 'e') {
        return Boolean.TRUE;
      }
      if (n == 5 && c == 'f'
          && buf[start + 1] == 'a' && buf[start + 2] == 'l'
          && buf[start + 3] == 's' && buf[start + 4] == 'e') {
        return Boolean.FALSE;
      }
      if (n == 4 && c == 'n'
          && buf[start + 1] == 'u' && buf[start + 2] == 'l' && buf[start + 3] == 'l') {
        return null;
      }
      String s = new String(buf, start, n);
      if ((c == '-' || (c >= '0' && c <= '9')) && isNumberShaped(s)) {
        return new Num(s);
      }
      return s;
    }
  }

  /** True when s matches the JSON number grammar: -?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)? */
  public static boolean isNumberShaped(String s) {
    int len = s.length();
    int i = 0;
    if (i < len && s.charAt(i) == '-') {
      i++;
    }
    if (i == len) {
      return false;
    }
    char c = s.charAt(i);
    if (c == '0') {
      i++;
    } else if (c >= '1' && c <= '9') {
      i++;
      while (i < len && isDigit(s.charAt(i))) {
        i++;
      }
    } else {
      return false;
    }
    if (i < len && s.charAt(i) == '.') {
      i++;
      if (i == len || !isDigit(s.charAt(i))) {
        return false;
      }
      while (i < len && isDigit(s.charAt(i))) {
        i++;
      }
    }
    if (i < len && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
      i++;
      if (i < len && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
        i++;
      }
      if (i == len || !isDigit(s.charAt(i))) {
        return false;
      }
      while (i < len && isDigit(s.charAt(i))) {
        i++;
      }
    }
    return i == len;
  }

  private static boolean isDigit(char c) {
    return c >= '0' && c <= '9';
  }

  /////////////////
  // JSON parser //
  /////////////////

  static final class JsonParser {
    final char[] buf;
    final int len;
    int pos;

    JsonParser(char[] buf) {
      this.buf = buf;
      this.len = buf.length;
    }

    ParseException err(String message, int offset) {
      return new ParseException(message, offset);
    }

    void skipWs() {
      int p = pos;
      while (p < len) {
        char c = buf[p];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
          p++;
        } else {
          break;
        }
      }
      pos = p;
    }

    Object parseDocument() {
      skipWs();
      if (pos == len) {
        throw err("empty input", pos);
      }
      Object v = parseValue();
      skipWs();
      if (pos < len) {
        throw err("trailing data", pos);
      }
      return v;
    }

    Object parseValue() {
      if (pos == len) {
        throw err("unexpected EOF, expected value", pos);
      }
      char c = buf[pos];
      switch (c) {
        case '{':
          return parseObject();
        case '[':
          return parseArray();
        case '"':
          return parseString();
        case 't':
          expect("true");
          return Boolean.TRUE;
        case 'f':
          expect("false");
          return Boolean.FALSE;
        case 'n':
          expect("null");
          return null;
        default:
          if (c == '-' || isDigit(c)) {
            return parseNumber();
          }
          throw err("unexpected character '" + c + "'", pos);
      }
    }

    void expect(String word) {
      int n = word.length();
      if (pos + n > len) {
        throw err("unexpected EOF, expected '" + word + "'", pos);
      }
      for (int i = 0; i < n; i++) {
        if (buf[pos + i] != word.charAt(i)) {
          throw err("expected '" + word + "'", pos);
        }
      }
      pos += n;
    }

    Object parseObject() {
      pos++; // consume '{'
      HashMap<String, Object> m = new HashMap<>();
      skipWs();
      if (pos < len && buf[pos] == '}') {
        pos++;
        return m;
      }
      while (true) {
        if (pos == len) {
          throw err("unterminated object", pos);
        }
        if (buf[pos] != '"') {
          throw err("expected '\"' to begin object key", pos);
        }
        String k = parseString();
        skipWs();
        if (pos == len || buf[pos] != ':') {
          throw err("expected ':' after object key", pos);
        }
        pos++;
        skipWs();
        Object v = parseValue();
        m.put(k, v);
        skipWs();
        if (pos == len) {
          throw err("unterminated object", pos);
        }
        char c = buf[pos];
        if (c == ',') {
          pos++;
          skipWs();
        } else if (c == '}') {
          pos++;
          return m;
        } else {
          throw err("expected ',' or '}' in object", pos);
        }
      }
    }

    Object parseArray() {
      pos++; // consume '['
      ArrayList<Object> a = new ArrayList<>();
      skipWs();
      if (pos < len && buf[pos] == ']') {
        pos++;
        return a;
      }
      while (true) {
        if (pos == len) {
          throw err("unterminated array", pos);
        }
        a.add(parseValue());
        skipWs();
        if (pos == len) {
          throw err("unterminated array", pos);
        }
        char c = buf[pos];
        if (c == ',') {
          pos++;
          skipWs();
        } else if (c == ']') {
          pos++;
          return a;
        } else {
          throw err("expected ',' or ']' in array", pos);
        }
      }
    }

    String parseString() {
      pos++; // consume '"'
      int start = pos;
      int p = pos;
      while (p < len) {
        char c = buf[p];
        if (c == '"') {
          pos = p + 1;
          return new String(buf, start, p - start);
        }
        if (c == '\\' || c < 0x20) {
          break;
        }
        p++;
      }
      return parseStringComplex(start, p);
    }

    String parseStringComplex(int start, int p) {
      StringBuilder sb = new StringBuilder();
      sb.append(buf, start, p - start);
      while (p < len) {
        char c = buf[p];
        if (c == '"') {
          pos = p + 1;
          return sb.toString();
        }
        if (c < 0x20) {
          throw err("unescaped control character in string", p);
        }
        if (c != '\\') {
          sb.append(c);
          p++;
          continue;
        }
        p++;
        if (p == len) {
          break;
        }
        char e = buf[p++];
        switch (e) {
          case '"': sb.append('"'); break;
          case '\\': sb.append('\\'); break;
          case '/': sb.append('/'); break;
          case 'b': sb.append('\b'); break;
          case 'f': sb.append('\f'); break;
          case 'n': sb.append('\n'); break;
          case 'r': sb.append('\r'); break;
          case 't': sb.append('\t'); break;
          case 'u': {
            if (p + 4 > len) {
              throw err("unterminated unicode escape", p);
            }
            int ch = (digit16(buf[p]) << 12)
                   + (digit16(buf[p + 1]) << 8)
                   + (digit16(buf[p + 2]) << 4)
                   +  digit16(buf[p + 3]);
            sb.append((char) ch);
            p += 4;
            break;
          }
          default:
            throw err("invalid escape character '" + e + "'", p - 1);
        }
      }
      throw err("unterminated string", start - 1);
    }

    int digit16(char c) {
      if (c >= '0' && c <= '9') {
        return c - '0';
      }
      if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
      }
      if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
      }
      throw err("invalid hex digit '" + c + "'", pos);
    }

    Num parseNumber() {
      int start = pos;
      int p = pos;
      if (buf[p] == '-') {
        p++;
      }
      if (p == len || !isDigit(buf[p])) {
        throw err("invalid number", start);
      }
      if (buf[p] == '0') {
        p++;
      } else {
        while (p < len && isDigit(buf[p])) {
          p++;
        }
      }
      if (p < len && buf[p] == '.') {
        p++;
        if (p == len || !isDigit(buf[p])) {
          throw err("invalid number", start);
        }
        while (p < len && isDigit(buf[p])) {
          p++;
        }
      }
      if (p < len && (buf[p] == 'e' || buf[p] == 'E')) {
        p++;
        if (p < len && (buf[p] == '+' || buf[p] == '-')) {
          p++;
        }
        if (p == len || !isDigit(buf[p])) {
          throw err("invalid number", start);
        }
        while (p < len && isDigit(buf[p])) {
          p++;
        }
      }
      pos = p;
      return new Num(new String(buf, start, p - start));
    }
  }

  /////////////////
  // Key sorting //
  /////////////////

  /** Lexicographic by Unicode code point sequence (not UTF-16 code unit order). */
  public static final Comparator<String> CODE_POINT_ORDER = (a, b) -> {
    int la = a.length();
    int lb = b.length();
    int i = 0;
    while (i < la && i < lb) {
      char ca = a.charAt(i);
      char cb = b.charAt(i);
      if (ca == cb) {
        i++;
        continue;
      }
      int cpa = Character.isHighSurrogate(ca) && i + 1 < la ? a.codePointAt(i) : ca;
      int cpb = Character.isHighSurrogate(cb) && i + 1 < lb ? b.codePointAt(i) : cb;
      return Integer.compare(cpa, cpb);
    }
    return Integer.compare(la - i, lb - i);
  };

  @SuppressWarnings("unchecked")
  static String[] sortedKeys(Map<String, Object> m) {
    String[] ks = m.keySet().toArray(new String[0]);
    Arrays.sort(ks, CODE_POINT_ORDER);
    return ks;
  }

  ////////////////////
  // JSON rendering //
  ////////////////////

  static void writeJsonString(StringBuilder sb, String s) {
    sb.append('"');
    int len = s.length();
    for (int i = 0; i < len; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '"': sb.append("\\\""); break;
        case '\\': sb.append("\\\\"); break;
        case '\b': sb.append("\\b"); break;
        case '\f': sb.append("\\f"); break;
        case '\n': sb.append("\\n"); break;
        case '\r': sb.append("\\r"); break;
        case '\t': sb.append("\\t"); break;
        default:
          if (c < 0x20) {
            sb.append("\\u");
            sb.append(String.format("%04x", (int) c));
          } else {
            sb.append(c);
          }
      }
    }
    sb.append('"');
  }

  @SuppressWarnings("unchecked")
  static void writeJsonCompact(StringBuilder sb, Object v) {
    if (v == null) {
      sb.append("null");
    } else if (v instanceof Boolean) {
      sb.append(v.toString());
    } else if (v instanceof Num) {
      sb.append(((Num) v).text);
    } else if (v instanceof String) {
      writeJsonString(sb, (String) v);
    } else if (v instanceof List) {
      List<Object> a = (List<Object>) v;
      sb.append('[');
      for (int i = 0; i < a.size(); i++) {
        if (i > 0) {
          sb.append(',');
        }
        writeJsonCompact(sb, a.get(i));
      }
      sb.append(']');
    } else {
      Map<String, Object> m = (Map<String, Object>) v;
      sb.append('{');
      String[] ks = sortedKeys(m);
      for (int i = 0; i < ks.length; i++) {
        if (i > 0) {
          sb.append(',');
        }
        writeJsonString(sb, ks[i]);
        sb.append(':');
        writeJsonCompact(sb, m.get(ks[i]));
      }
      sb.append('}');
    }
  }

  static void indent(StringBuilder sb, int n) {
    for (int i = 0; i < n; i++) {
      sb.append(' ');
    }
  }

  @SuppressWarnings("unchecked")
  static void writeJsonPretty(StringBuilder sb, Object v, int ind) {
    if (v instanceof List) {
      List<Object> a = (List<Object>) v;
      if (a.isEmpty()) {
        sb.append("[]");
        return;
      }
      sb.append("[\n");
      for (int i = 0; i < a.size(); i++) {
        indent(sb, ind + 2);
        writeJsonPretty(sb, a.get(i), ind + 2);
        sb.append(i + 1 < a.size() ? ",\n" : "\n");
      }
      indent(sb, ind);
      sb.append(']');
    } else if (v instanceof Map) {
      Map<String, Object> m = (Map<String, Object>) v;
      if (m.isEmpty()) {
        sb.append("{}");
        return;
      }
      sb.append("{\n");
      String[] ks = sortedKeys(m);
      for (int i = 0; i < ks.length; i++) {
        indent(sb, ind + 2);
        writeJsonString(sb, ks[i]);
        sb.append(": ");
        writeJsonPretty(sb, m.get(ks[i]), ind + 2);
        sb.append(i + 1 < ks.length ? ",\n" : "\n");
      }
      indent(sb, ind);
      sb.append('}');
    } else {
      writeJsonCompact(sb, v);
    }
  }

  ///////////////////
  // RON rendering //
  ///////////////////

  static boolean bareOkKey(String s) {
    int len = s.length();
    if (len == 0) {
      return false;
    }
    for (int i = 0; i < len; i++) {
      if (isDelim(s.charAt(i))) {
        return false;
      }
    }
    return true;
  }

  static boolean bareOkValue(String s) {
    if (!bareOkKey(s)) {
      return false;
    }
    if (s.equals("true") || s.equals("false") || s.equals("null")) {
      return false;
    }
    char c = s.charAt(0);
    return !((c == '-' || (c >= '0' && c <= '9')) && isNumberShaped(s));
  }

  static void writeRonString(StringBuilder sb, String s, boolean isKey) {
    if (isKey ? bareOkKey(s) : bareOkValue(s)) {
      sb.append(s);
      return;
    }
    int longestRun = 0;
    int run = 0;
    int len = s.length();
    for (int i = 0; i < len; i++) {
      if (s.charAt(i) == '\'') {
        run++;
        if (run > longestRun) {
          longestRun = run;
        }
      } else {
        run = 0;
      }
    }
    int delim = longestRun + 1;
    for (int i = 0; i < delim; i++) {
      sb.append('\'');
    }
    sb.append(s);
    for (int i = 0; i < delim; i++) {
      sb.append('\'');
    }
  }

  /** True when the compact rendering of v begins with one of: { [ ' " */
  static boolean startsStructural(Object v) {
    if (v instanceof Map || v instanceof List) {
      return true;
    }
    return v instanceof String && !bareOkValue((String) v);
  }

  @SuppressWarnings("unchecked")
  static void writeRonCompactValue(StringBuilder sb, Object v) {
    if (v == null) {
      sb.append("null");
    } else if (v instanceof Boolean) {
      sb.append(v.toString());
    } else if (v instanceof Num) {
      sb.append(((Num) v).text);
    } else if (v instanceof String) {
      writeRonString(sb, (String) v, false);
    } else if (v instanceof List) {
      List<Object> a = (List<Object>) v;
      sb.append('[');
      for (int i = 0; i < a.size(); i++) {
        if (i > 0) {
          sb.append(' ');
        }
        writeRonCompactValue(sb, a.get(i));
      }
      sb.append(']');
    } else {
      sb.append('{');
      writeRonCompactMembers(sb, (Map<String, Object>) v);
      sb.append('}');
    }
  }

  static void writeRonCompactMembers(StringBuilder sb, Map<String, Object> m) {
    String[] ks = sortedKeys(m);
    for (int i = 0; i < ks.length; i++) {
      if (i > 0) {
        sb.append(' ');
      }
      writeRonString(sb, ks[i], true);
      Object v = m.get(ks[i]);
      if (!startsStructural(v)) {
        sb.append(' ');
      }
      writeRonCompactValue(sb, v);
    }
  }

  @SuppressWarnings("unchecked")
  static String renderRonCompact(Object v) {
    StringBuilder sb = new StringBuilder();
    if (v instanceof Map && !((Map<String, Object>) v).isEmpty()) {
      // root object braces are elided (empty root keeps {} so output is non-empty)
      writeRonCompactMembers(sb, (Map<String, Object>) v);
    } else {
      writeRonCompactValue(sb, v);
    }
    return sb.toString();
  }

  static final int INLINE_LIMIT = 80;

  static int utf8Length(CharSequence s) {
    int bytes = 0;
    int len = s.length();
    for (int i = 0; i < len; i++) {
      char c = s.charAt(i);
      if (c < 0x80) {
        bytes += 1;
      } else if (c < 0x800) {
        bytes += 2;
      } else if (Character.isHighSurrogate(c)) {
        bytes += 4;
        i++;
      } else {
        bytes += 3;
      }
    }
    return bytes;
  }

  /**
   * Inline (single-line, space-separated) pretty rendering of v, or null when
   * v must render multiline: arrays inline when every element inlines and the
   * result is at most 80 bytes; objects inline only with exactly one key.
   * Scalars always inline.
   */
  @SuppressWarnings("unchecked")
  static String tryInline(Object v) {
    StringBuilder sb = new StringBuilder();
    if (v instanceof List) {
      List<Object> a = (List<Object>) v;
      sb.append('[');
      for (int i = 0; i < a.size(); i++) {
        if (i > 0) {
          sb.append(' ');
        }
        String s = tryInline(a.get(i));
        if (s == null) {
          return null;
        }
        sb.append(s);
      }
      sb.append(']');
      return utf8Length(sb) <= INLINE_LIMIT ? sb.toString() : null;
    }
    if (v instanceof Map) {
      Map<String, Object> m = (Map<String, Object>) v;
      if (m.isEmpty()) {
        return "{}";
      }
      if (m.size() != 1) {
        return null;
      }
      Map.Entry<String, Object> e = m.entrySet().iterator().next();
      sb.append('{');
      writeRonString(sb, e.getKey(), true);
      sb.append(' ');
      String s = tryInline(e.getValue());
      if (s == null) {
        return null;
      }
      sb.append(s);
      sb.append('}');
      return utf8Length(sb) <= INLINE_LIMIT ? sb.toString() : null;
    }
    writeRonCompactValue(sb, v); // scalar: compact and inline forms coincide
    return sb.toString();
  }

  @SuppressWarnings("unchecked")
  static void writeRonPretty(StringBuilder sb, Object v, int ind) {
    // only reached for containers that failed tryInline
    if (v instanceof List) {
      List<Object> a = (List<Object>) v;
      sb.append("[\n");
      for (Object e : a) {
        indent(sb, ind + 2);
        String s = tryInline(e);
        if (s != null) {
          sb.append(s);
        } else {
          writeRonPretty(sb, e, ind + 2);
        }
        sb.append('\n');
      }
      indent(sb, ind);
      sb.append(']');
    } else {
      Map<String, Object> m = (Map<String, Object>) v;
      sb.append("{\n");
      for (String k : sortedKeys(m)) {
        indent(sb, ind + 2);
        writeRonString(sb, k, true);
        sb.append(' ');
        Object val = m.get(k);
        String s = tryInline(val);
        if (s != null) {
          sb.append(s);
        } else {
          writeRonPretty(sb, val, ind + 2);
        }
        sb.append('\n');
      }
      indent(sb, ind);
      sb.append('}');
    }
  }

  static String renderRonPretty(Object v) {
    StringBuilder sb = new StringBuilder();
    String s = tryInline(v);
    if (s != null) {
      sb.append(s);
    } else {
      writeRonPretty(sb, v, 0);
    }
    sb.append('\n');
    return sb.toString();
  }

  ////////////////
  // Public API //
  ////////////////

  /** Parse RON source into the value model. */
  public static Object parseRon(String s) {
    return new RonParser(s.toCharArray()).parseDocument();
  }

  /** Parse JSON source into the value model (numbers kept as source text). */
  public static Object parseJson(String s) {
    return new JsonParser(s.toCharArray()).parseDocument();
  }

  /** Render a value-model tree as JSON. */
  public static String writeJson(Object model, boolean pretty) {
    StringBuilder sb = new StringBuilder();
    if (pretty) {
      writeJsonPretty(sb, model, 0);
    } else {
      writeJsonCompact(sb, model);
    }
    return sb.toString();
  }

  /** Render a value-model tree as RON. */
  public static String writeRon(Object model, boolean pretty) {
    return pretty ? renderRonPretty(model) : renderRonCompact(model);
  }

  /** Convert RON source to JSON text. */
  public static String ronToJson(String ron, boolean pretty) {
    return writeJson(parseRon(ron), pretty);
  }

  /** Convert JSON source to RON text. */
  public static String jsonToRon(String json, boolean pretty) {
    return writeRon(parseJson(json), pretty);
  }

  private Ron() {}
}
