# 1 "libforth.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 363 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "libforth.c" 2
# 537 "libforth.c"
static const char *initial_forth_program =
": smudge pwd @ 1 + dup @ hidden-mask xor swap ! _exit\n"
": (;) ' _exit , 0 state ! _exit\n"
": ; immediate (;) smudge _exit\n"
": : immediate :: smudge _exit\n"
": here h @ ; \n"
": [ immediate 0 state ! ; \n"
": ] 1 state ! ; \n"
": >mark here 0 , ; \n"
": :noname immediate -1 , here dolist , ] ; \n"
": if immediate ' ?branch , >mark ; \n"
": else immediate ' branch , >mark swap dup here swap - swap ! ; \n"
": then immediate dup here swap - swap ! ; \n"
": begin immediate here ; \n"
": until immediate ' ?branch , here - , ; \n"
": ( immediate begin key ')' = until ; \n"
": rot >r swap r> swap ; \n"
": -rot rot rot ; \n"
": tuck swap over ; \n"
": nip swap drop ; \n"
": 2drop drop drop ; \n"
": allot here + h ! ; \n"
": emit _emit drop ; \n"
": space bl emit ; \n"
": evaluate 0 evaluator ; \n"
": . (.) drop space ; \n"
": ? @ . ;\n" ;





static const char conv[] = "0123456789abcdefghijklmnopqrstuvwxzy";




enum fams {
 FAM_WO,
 FAM_RO,
 FAM_RW,
 LAST_FAM
};
# 588 "libforth.c"
static const char *fams[] = {
                  "wb",
                  "rb",
                  "w+b",
 NULL
};





enum errors
{
 INITIALIZED,
 OK,
 FATAL,
 RECOVERABLE,
};
# 635 "libforth.c"
enum header {
 MAGIC0,
 MAGIC1,
 MAGIC2,
 MAGIC3,
 CELL_SIZE,
 VERSION,
 ENDIAN,
 LOG2_SIZE,
 MAX_HEADER_FIELD
};






static const uint8_t header[MAX_HEADER_FIELD] = {
                      0xFF,
                      '4',
                      'T',
                      'H',
                      sizeof(forth_cell_t),
                      FORTH_CORE_VERSION,
                      -1,
                      -1
};
# 712 "libforth.c"
struct forth {
 uint8_t header[sizeof(header)];
 forth_cell_t core_size;
 uint8_t *s;
 forth_cell_t *S;
 forth_cell_t *vstart;
 forth_cell_t *vend;
 const struct forth_functions *calls;
 int unget;
 bool unget_set;
 size_t line;
 forth_cell_t m[];
};
# 737 "libforth.c"
enum actions_on_error
{
 ERROR_RECOVER,
 ERROR_HALT,
 ERROR_INVALIDATE,
};
# 798 "libforth.c"
enum registers {

 DIC = 6, RSTK = 7, STATE = 8, BASE = 9, PWD = 10, SOURCE_ID = 11, SIN = 12, SIDX = 13, SLEN = 14, START_ADDR = 15, FIN = 16, FOUT = 17, STDIN = 18, STDOUT = 19, STDERR = 20, ARGC = 21, ARGV = 22, DEBUG = 23, INVALID = 24, TOP = 25, INSTRUCTION = 26, STACK_SIZE = 27, ERROR_HANDLER = 28, THROW_HANDLER = 29, SIGNAL_HANDLER = 30, SCRATCH_X = 31,

};

static const char *register_names[] = {

 "h", "r", "state", "base", "pwd", "`source-id", "`sin", "`sidx", "`slen", "`start-address", "`fin", "`fout", "`stdin", "`stdout", "`stderr", "`argc", "`argv", "`debug", "`invalid", "`top", "`instruction", "`stack-size", "`error-handler", "`handler", "`signal", "`x",

 NULL
};
# 838 "libforth.c"
enum input_stream {
 FILE_IN,
 STRING_IN = -1
};
# 947 "libforth.c"
enum instructions {

 PUSH, CONST, RUN, DEFINE, IMMEDIATE, READ, LOAD, STORE, CLOAD, CSTORE, SUB, ADD, AND, OR, XOR, INV, SHL, SHR, MUL, DIV, ULESS, UMORE, EXIT, KEY, EMIT, FROMR, TOR, BRANCH, QBRANCH, PNUM, COMMA, EQUAL, SWAP, DUP, DROP, OVER, TAIL, FIND, DEPTH, SPLOAD, SPSTORE, CLOCK, EVALUATOR, PSTK, RESTART, CALL, SYSTEM, FCLOSE, FOPEN, FDELETE, FREAD, FWRITE, FPOS, FSEEK, FFLUSH, FRENAME, TMPFILE, RAISE, DATE, MEMMOVE, MEMCHR, MEMSET, MEMCMP, ALLOCATE, FREE, RESIZE, GETENV, BYE, LAST_INSTRUCTION,

};
# 961 "libforth.c"
static const char *instruction_names[] = {

 "push", "const", "run", "define", "immediate", "read", "@", "!", "c@", "c!", "-", "+", "and", "or", "xor", "invert", "lshift", "rshift", "*", "/", "u<", "u>", "exit", "key", "_emit", "r>", ">r", "branch", "?branch", "(.)", ",", "=", "swap", "dup", "drop", "over", "tail", "find", "depth", "sp@", "sp!", "clock", "evaluator", ".s", "restart", "call", "system", "close-file", "open-file", "delete-file", "read-file", "write-file", "file-position", "reposition-file", "flush-file", "rename-file", "temporary-file", "raise", "date", "memory-copy", "memory-locate", "memory-set", "memory-compare", "allocate", "free", "resize", "getenv", "(bye)", NULL,

};





static const int stack_bounds[] = {

 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 0, 0, 1, 0, 1, 0, 1, 1, 1, 2, 2, 1, 1, 2, 0, 0, 0, 0, 0, 0, 3, 0, 1, 0, 2, 1, 3, 2, 3, 3, 1, 2, 1, 4, 0, 1, 0, 3, 3, 3, 3, 1, 1, 2, 2, 1, 0,

};
# 1014 "libforth.c"
static struct constants {
 const char *name;
 forth_cell_t value;
} constants[] = {

 { "dictionary-start", (((32u)+(32u))) }, { "r/o", (FAM_RO) }, { "r/w", (FAM_RW) }, { "w/o", (FAM_WO) }, { "size", (sizeof(forth_cell_t)) }, { "#tib", ((32u) * sizeof(forth_cell_t)) }, { "tib", ((32u) * sizeof(forth_cell_t)) }, { "SIGABRT", (-SIGABRT+(-512)) }, { "SIGFPE", (-SIGFPE +(-512)) }, { "SIGILL", (-SIGILL +(-512)) }, { "SIGINT", (-SIGINT +(-512)) }, { "SIGSEGV", (-SIGSEGV+(-512)) }, { "SIGTERM", (-SIGTERM+(-512)) }, { "bias-signal", ((-512)) }, { "bias-errno", ((-256)) }, { "instruction-mask", ((0x7f)) }, { "word-mask", ((0x1f)) }, { "hidden-bit", ((7)) }, { "hidden-mask", (1u << (7)) }, { "compile-bit", ((15)) }, { "dolist", (RUN) }, { "dolit", (2) }, { "doconst", (CONST) }, { "bl", (' ') }, { "')'", (')') }, { "cell", (1) },

 { NULL, 0 }
};





static int ferrno(void)
{
 return errno ? (-errno) + (-256) : 0;
}

const char *forth_strerror(void)
{
 static const char *unknown = "unknown reason";
 const char *r = errno ? strerror(errno) : unknown;
 if(!r)
  r = unknown;
 return r;
}

int forth_logger(const char *prefix, const char *func,
  unsigned line, const char *fmt, ...)
{
 int r;
 va_list ap;
 assert(prefix);
        assert(func);
        assert(fmt);
 fprintf(stderr, "[%s %u] %s: ", func, line, prefix);
 va_start(ap, fmt);
 r = vfprintf(stderr, fmt, ap);
 va_end(ap);
 fputc('\n', stderr);
 return r;
}
# 1088 "libforth.c"
static int forth_get_char(forth_t *o)
{
 assert(o);
 int r = 0;
 if(o->unget_set) {
  o->unget_set = false;
  return o->unget;
 }
 switch(o->m[SOURCE_ID]) {
 case FILE_IN:
  r = fgetc((FILE*)(o->m[FIN]));
  break;
 case STRING_IN:
  r = o->m[SIDX] >= o->m[SLEN] ?
   EOF :
   ((char*)(o->m[SIN]))[o->m[SIDX]++];
   break;
 default: r = EOF;
 }
 if(r == '\n')
  o->line++;
 return r;
}







static int forth_unget_char(forth_t *o, int ch)
{
 assert(o);
 if(o->unget_set)
  return -1;
 o->unget_set = true;
 o->unget = ch;
 return o->unget;
}
# 1142 "libforth.c"
static int forth_get_word(forth_t *o, uint8_t *s, forth_cell_t length)
{
 int ch;
 memset(s, 0, length);
 for(;;) {
  ch = forth_get_char(o);
  if(ch == EOF || !ch)
   return -1;
  if(!isspace(ch))
   break;
 }
 s[0] = ch;
 for(size_t i = 1; i < (length - 1); i++) {
  ch = forth_get_char(o);
  if(ch == EOF || isspace(ch) || !ch)
   goto unget;
  s[i] = ch;
 }
 return 0;
unget:
 forth_unget_char(o, ch);
 return 0;
}
# 1241 "libforth.c"
static forth_cell_t compile(forth_t *o, forth_cell_t code, const char *str,
  forth_cell_t compiling, forth_cell_t hide)
{
 assert(o && code < LAST_INSTRUCTION);
 forth_cell_t *m = o->m, header = m[DIC], l = 0, cf = 0;


 strcpy((char *)(o->m + header), str);

 l = strlen(str) + 1;
 l = (l + (sizeof(forth_cell_t) - 1)) & ~(sizeof(forth_cell_t) - 1);
 l = l/sizeof(forth_cell_t);
 m[DIC] += l;

 m[m[DIC]++] = m[PWD];
 m[PWD] = m[DIC] - 1;

 assert(l < (0x1f));
 cf = m[DIC];
 m[m[DIC]++] =
  ((!!compiling) << (15))
  | (l << (8))
  | (hide << (7))
  | code;
 return cf;
}






int forth_string_to_cell(int base, forth_cell_t *n, const char *s)
{
 char *end = NULL;
 errno = 0;
 *n = strtol(s, &end, base);
 return errno || *s == '\0' || *end != '\0';
}
# 1300 "libforth.c"
static int istrcmp(const char *a, const char *b)
{
 for(; ((*a == *b) || (tolower(*a) == tolower(*b))) && *a && *b; a++, b++)
  ;
 return tolower(*a) - tolower(*b);
}





static int match(forth_cell_t *m, forth_cell_t pwd, const char *s)
{
 forth_cell_t len = (((m[pwd + 1]) >> (8)) & (0x1f));
 return !((m[pwd+1]) & 0x80) && !istrcmp(s, (char*)(&m[pwd-len]));
}
# 1326 "libforth.c"
forth_cell_t forth_find(forth_t *o, const char *s)
{
 forth_cell_t *m = o->m, pwd = m[PWD];
# 1350 "libforth.c"
 for (;pwd > ((32u)+(32u)) && !match(m, pwd, s);)
  pwd = m[pwd];

 return pwd > ((32u)+(32u)) ? pwd + 1 : 0;
}
# 1363 "libforth.c"
static int print_cell(forth_t *o, FILE *out, forth_cell_t u)
{
 int i = 0, r = 0;
 char s[64 + 1] = {0};
 unsigned base = o->m[BASE];
 base = base ? base : 10 ;
 if(base >= 37)
  return -1;
 if(base == 10)
  return fprintf(out, "%"PRIdCell, u);
 do
  s[i++] = conv[u % base];
 while ((u /= base));
 for(r = --i; i >= 0; i--)
  if(fputc(s[i], out) != s[i])
   return -1;
 return r;
}
# 1389 "libforth.c"
static forth_cell_t check_bounds(forth_t *o, jmp_buf *on_error,
  forth_cell_t f, unsigned line, forth_cell_t bound)
{
 if(o->m[DEBUG] >= FORTH_DEBUG_CHECKS)
  debug("0x%"PRIxCell " %u", f, line);
 if(f >= bound) {
  fatal("bounds check failed (%"PRIdCell" >= %zu) C line %u Forth Line %zu",
    f, (size_t)bound, line, o->line);
  longjmp(*on_error, FATAL);
 }
 return f;
}





static void check_depth(forth_t *o, jmp_buf *on_error,
  forth_cell_t *S, forth_cell_t expected, unsigned line)
{
 if(o->m[DEBUG] >= FORTH_DEBUG_CHECKS)
  debug("0x%"PRIxCell " %u", (forth_cell_t)(S - o->vstart), line);
 if((uintptr_t)(S - o->vstart) < expected) {
  error("stack underflow %p -> %u (line %zu)", S - o->vstart, line, o->line);
  longjmp(*on_error, RECOVERABLE);
 } else if(S > o->vend) {
  error("stack overflow %p -> %u (line %zu)", S - o->vend, line, o->line);
  longjmp(*on_error, RECOVERABLE);
 }
}




static forth_cell_t check_dictionary(forth_t *o, jmp_buf *on_error,
  forth_cell_t dptr)
{
 if((o->m + dptr) >= (o->vstart)) {
  fatal("dictionary pointer is in stack area %"PRIdCell, dptr);
  forth_invalidate(o);
  longjmp(*on_error, FATAL);
 }
 return dptr;
}
# 1441 "libforth.c"
static void check_is_asciiz(jmp_buf *on_error, char *s, forth_cell_t end)
{
 if(*(s + end) != '\0') {
  error("not an ASCIIZ string at %p", s);
  longjmp(*on_error, RECOVERABLE);
 }
}






static char *forth_get_string(forth_t *o, jmp_buf *on_error,
  forth_cell_t **S, forth_cell_t f)
{
 forth_cell_t length = f + 1;
 char *string = ((char*)o->m) + **S;
 (*S)--;
 check_is_asciiz(on_error, string, length);
 return string;
}






static const char* forth_get_fam(jmp_buf *on_error, forth_cell_t f)
{
 if(f >= LAST_FAM) {
  error("Invalid file access method %"PRIdCell, f);
  longjmp(*on_error, RECOVERABLE);
 }
 return fams[f];
}




static void print_stack(forth_t *o, FILE *out, forth_cell_t *S, forth_cell_t f)
{
 forth_cell_t depth = (forth_cell_t)(S - o->vstart);
 fprintf(out, "%"PRIdCell": ", depth);
 if(!depth)
  return;
 for(forth_cell_t j = (S - o->vstart), i = 1; i < j; i++) {
  print_cell(o, out, *(o->S + i + 1));
  fputc(' ', out);
 }
 print_cell(o, out, f);
 fputc(' ', out);
}







static void trace(forth_t *o, forth_cell_t instruction,
  forth_cell_t *S, forth_cell_t f)
{
 if(o->m[DEBUG] < FORTH_DEBUG_INSTRUCTION)
  return;
 fprintf(stderr, "\t( %s\t ", instruction_names[instruction]);
 print_stack(o, stderr, S, f);
 fputs(" )\n", stderr);
}





void forth_set_file_input(forth_t *o, FILE *in)
{
 assert(o);
 assert(in);
 o->unget_set = false;
 o->m[SOURCE_ID] = FILE_IN;
 o->m[FIN] = (forth_cell_t)in;
}

void forth_set_file_output(forth_t *o, FILE *out)
{
 assert(o);
        assert(out);
 o->m[FOUT] = (forth_cell_t)out;
}

void forth_set_block_input(forth_t *o, const char *s, size_t length)
{
 assert(o);
 assert(s);
 o->unget_set = false;
 o->m[SIDX] = 0;
 o->m[SLEN] = length;
 o->m[SOURCE_ID] = STRING_IN;
 o->m[SIN] = (forth_cell_t)s;
}

void forth_set_string_input(forth_t *o, const char *s)
{
 assert(s);
 forth_set_block_input(o, s, strlen(s) + 1);
}

int forth_eval_block(forth_t *o, const char *s, size_t length)
{
 assert(o);
 assert(s);
 forth_set_block_input(o, s, length);
 return forth_run(o);
}

int forth_eval(forth_t *o, const char *s)
{
 assert(o);
 assert(s);
 forth_set_string_input(o, s);
 return forth_run(o);
}

int forth_define_constant(forth_t *o, const char *name, forth_cell_t c)
{
 assert(o);
 assert(name);
 compile(o, CONST, name, true, false);
 if(strlen(name) >= (32u))
  return -1;
 if(o->m[DIC] + 1 >= o->core_size)
  return -1;
 o->m[o->m[DIC]++] = c;
 return 0;
}

void forth_set_args(forth_t *o, int argc, char **argv)
{
 assert(o);
 o->m[ARGC] = argc;
 o->m[ARGV] = (forth_cell_t)argv;
}

int forth_is_invalid(forth_t *o)
{
 assert(o);
 return !!(o->m[INVALID]);
}

void forth_invalidate(forth_t *o)
{
 assert(o);
 o->m[INVALID] = 1;
}

void forth_set_debug_level(forth_t *o, enum forth_debug_level level)
{
 assert(o);
 o->m[DEBUG] = level;
}

FILE *forth_fopen_or_die(const char *name, char *mode)
{
 FILE *file;
 assert(name);
 assert(mode);
 errno = 0;
 file = fopen(name, mode);
 if(!file) {
  fatal("opening file \"%s\" => %s", name, forth_strerror());
  exit(EXIT_FAILURE);
 }
 return file;
}
# 1630 "libforth.c"
static void forth_make_default(forth_t *o, size_t size, FILE *in, FILE *out)
{
 assert(o && size >= MINIMUM_CORE_SIZE && in && out);
 o->core_size = size;
 o->m[STACK_SIZE] = size / (64u) > (64u) ?
    size / (64u) :
    (64u);

 o->s = (uint8_t*)(o->m + (32u));
 o->m[FOUT] = (forth_cell_t)out;
 o->m[START_ADDR] = (forth_cell_t)&(o->m);
 o->m[STDIN] = (forth_cell_t)stdin;
 o->m[STDOUT] = (forth_cell_t)stdout;
 o->m[STDERR] = (forth_cell_t)stderr;
 o->m[RSTK] = size - o->m[STACK_SIZE];
 o->m[ARGC] = o->m[ARGV] = 0;
 o->S = o->m + size - (2 * o->m[STACK_SIZE]);
 o->vstart = o->m + size - (2 * o->m[STACK_SIZE]);
 o->vend = o->vstart + o->m[STACK_SIZE];
 forth_set_file_input(o, in);
}






static void make_header(uint8_t *dst, uint8_t log2size)
{
 memcpy(dst, header, sizeof header);

 dst[ENDIAN] = !IS_BIG_ENDIAN;
 dst[LOG2_SIZE] = log2size;
}





forth_cell_t forth_blog2(forth_cell_t x)
{
 forth_cell_t b = 0;
 while(x >>= 1)
  b++;
 return b;
}




forth_cell_t forth_round_up_pow2(forth_cell_t r)
{
 forth_cell_t up = 1;
 while(up < r)
  up <<= 1;
 return up;
}
# 1695 "libforth.c"
forth_t *forth_init(size_t size, FILE *in, FILE *out,
  const struct forth_functions *calls)
{
 forth_cell_t *m, i, w, t, pow;
 forth_t *o;
 assert(in);
 assert(out);
 assert(sizeof(forth_cell_t) >= sizeof(uintptr_t));
 size = forth_round_up_pow2(size);
 pow = forth_blog2(size);
# 1727 "libforth.c"
 do { if(!(size >= MINIMUM_CORE_SIZE)) { abort(); } } while(0);
 if(!(o = calloc(1, sizeof(*o) + sizeof(forth_cell_t)*size)))
  return NULL;




 forth_make_default(o, size, in, out);





 make_header(o->header, pow);

 o->calls = calls;
 m = o->m;
# 1766 "libforth.c"
 o->m[PWD] = 0;
 t = m[DIC] = ((32u)+(32u));
 m[m[DIC]++] = TAIL;
 w = m[DIC];
 m[m[DIC]++] = READ;
 m[m[DIC]++] = RUN;
 o->m[INSTRUCTION] = m[DIC];
 m[m[DIC]++] = w;
 m[m[DIC]++] = t;
 m[m[DIC]++] = o->m[INSTRUCTION] - 1;
# 1791 "libforth.c"
 compile(o, DEFINE, ":", false, false);
 compile(o, DEFINE, "::", true, false);
 compile(o, IMMEDIATE, "immediate", false, false);
# 1804 "libforth.c"
 for(i = READ, w = READ; instruction_names[i]; i++)
  compile(o, w++, instruction_names[i], true, false);
 compile(o, EXIT, "_exit", true, false);
 compile(o, PUSH, "'", true, false);





 for(i = 0; register_names[i]; i++)
  do { if(!(forth_define_constant(o, register_names[i], i+DIC) >= 0)) { abort(); } } while(0);




 w = size - (2 * o->m[STACK_SIZE]);
 do { if(!(forth_define_constant(o, "stack-start", w) >= 0)) { abort(); } } while(0);
 do { if(!(forth_define_constant(o, "max-core", size) >= 0)) { abort(); } } while(0);

 for(i = 0; constants[i].name; i++)
  do { if(!(forth_define_constant(o, constants[i].name, constants[i].value) >= 0)) { abort(); } } while(0);






 do { if(!(forth_eval(o, initial_forth_program) >= 0)) { abort(); } } while(0);






 forth_set_file_input(o, in);
 o->line = 1;
 return o;
}






int forth_dump_core(forth_t *o, FILE *dump)
{
 assert(o);
        assert(dump);
 size_t w = sizeof(*o) + sizeof(forth_cell_t) * o->core_size;
 return w != fwrite(o, 1, w, dump) ? -1: 0;
}







int forth_save_core_file(forth_t *o, FILE *dump)
{
 assert(o && dump);
 uint64_t r1, r2, core_size = o->core_size;
 if(forth_is_invalid(o))
  return -1;
 r1 = fwrite(o->header, 1, sizeof(o->header), dump);
 r2 = fwrite(o->m, 1, sizeof(forth_cell_t) * core_size, dump);
 if(r1+r2 != (sizeof(o->header) + sizeof(forth_cell_t) * core_size))
  return -1;
 return 0;
}
# 1886 "libforth.c"
forth_t *forth_load_core_file(FILE *dump)
{
 uint8_t actual[sizeof(header)] = {0},
  expected[sizeof(header)] = {0};
 forth_t *o = NULL;
 uint64_t w = 0, core_size = 0;
 assert(dump);
 make_header(expected, 0);
 if(sizeof(actual) != fread(actual, 1, sizeof(actual), dump)) {
  goto fail;
 }
 if(memcmp(expected, actual, sizeof(header)-1)) {
  goto fail;
 }
 core_size = 1 << actual[LOG2_SIZE];

 if(core_size < MINIMUM_CORE_SIZE) {
  error("core size of %"PRIdCell" is too small", core_size);
  goto fail;
 }
 w = sizeof(*o) + (sizeof(forth_cell_t) * core_size);
 errno = 0;
 if(!(o = calloc(w, 1))) {
  error("allocation of size %"PRId64" failed, %s", w, forth_strerror());
  goto fail;
 }
 w = sizeof(forth_cell_t) * core_size;
 if(w != fread(o->m, 1, w, dump)) {
  error("file too small (expected %"PRId64")", w);
  goto fail;
 }
 o->core_size = core_size;
 memcpy(o->header, actual, sizeof(o->header));
 forth_make_default(o, core_size, stdin, stdout);
 return o;
fail:
 free(o);
 return NULL;
}




forth_t *forth_load_core_memory(char *m, size_t size)
{
 assert(m);
 assert((size / sizeof(forth_cell_t)) >= MINIMUM_CORE_SIZE);
 forth_t *o;
 size_t offset = sizeof(o->header);
 size -= offset;
 errno = 0;
 o = calloc(sizeof(*o) + size, 1);
 if(!o) {
  error("allocation of size %zu failed, %s",
    sizeof(*o) + size, forth_strerror());
  return NULL;
 }
 make_header(o->header, forth_blog2(size));
 memcpy(o->m, m + offset, size);
 forth_make_default(o, size / sizeof(forth_cell_t), stdin, stdout);
 return o;
}





char *forth_save_core_memory(forth_t *o, size_t *size)
{
 assert(o && size);
 char *m;
 *size = 0;
 errno = 0;
 uint64_t w = o->core_size;
 m = malloc(w * sizeof(forth_cell_t) + sizeof(o->header));
 if(!m) {
  error("allocation of size %zu failed, %s",
    o->core_size * sizeof(forth_cell_t), forth_strerror());
  return NULL;
 }
 memcpy(m, o->header, sizeof(o->header));
 memcpy(m + sizeof(o->header), o->m, w);
 *size = o->core_size * sizeof(forth_cell_t) + sizeof(o->header);
 return m;
}





void forth_free(forth_t *o)
{
 assert(o);


 forth_invalidate(o);
 free(o);
}





struct forth_functions *forth_new_function_list(forth_cell_t count)
{
 struct forth_functions *ff = NULL;
 errno = 0;
 ff = calloc(sizeof(*ff), 1);
 ff->functions = calloc(sizeof(ff->functions[0]) * count, 1);
 if(!ff || !ff->functions)
  warning("calloc failed: %s", forth_strerror());
 else
  ff->count = count;
 return ff;
}

void forth_delete_function_list(struct forth_functions *calls)
{
 assert(calls);
 free(calls->functions);
 free(calls);
}
# 2025 "libforth.c"
void forth_push(forth_t *o, forth_cell_t f)
{
 assert(o);
        assert(o->S < o->m + o->core_size);
 *++(o->S) = o->m[TOP];
 o->m[TOP] = f;
}

forth_cell_t forth_pop(forth_t *o)
{
 assert(o);
 assert(o->S > o->m);
 forth_cell_t f = o->m[TOP];
 o->m[TOP] = *(o->S)--;
 return f;
}

forth_cell_t forth_stack_position(forth_t *o)
{
 assert(o);
 return o->S - o->vstart;
}

void forth_signal(forth_t *o, int sig)
{
 assert(o);
 o->m[SIGNAL_HANDLER] = (forth_cell_t)((sig * -1) + (-512));
}

char *forth_strdup(const char *s)
{
 assert(s);
 char *str;
 if (!(str = malloc(strlen(s) + 1)))
  return NULL;
 strcpy(str, s);
 return str;
}

void forth_free_words(char **s, size_t length)
{
 for(size_t i = 0; i < length; i++)
  free(s[i]);
 free(s);
}

char **forth_words(forth_t *o, size_t *length)
{
 assert(o);
 assert(length);
 forth_cell_t pwd = o->m[PWD], len;
 forth_cell_t *m = o->m;
 size_t i;
 char **n, **s = calloc(2, sizeof(*s));
 if(!s)
  return NULL;
 for (i = 0 ;pwd > ((32u)+(32u)); pwd = m[pwd], i++) {
  len = (((m[pwd + 1]) >> (8)) & (0x1f));
  s[i] = forth_strdup((char*)(&m[pwd-len]));
  if(!s[i]) {
   forth_free_words(s, i);
   *length = 0;
   return NULL;
  }
  n = realloc(s, sizeof(*s) * (i+2));
  if(!n) {
   forth_free_words(s, i);
   *length = 0;
   return NULL;
  }
  s = n;
 }
 *length = i;
 return s;
}
# 2112 "libforth.c"
int forth_run(forth_t *o)
{
 int errorval = 0, rval = 0;
 assert(o);
 jmp_buf on_error;
 if(forth_is_invalid(o)) {
  fatal("refusing to run an invalid forth, %"PRIdCell, forth_is_invalid(o));
  return -1;
 }






 if ((errorval = setjmp(on_error)) || forth_is_invalid(o)) {

  if(forth_is_invalid(o))
   return -1;
  switch(errorval) {
   default:
   case FATAL:
    forth_invalidate(o);
    return -1;



   case RECOVERABLE:
    switch(o->m[ERROR_HANDLER]) {
    case ERROR_INVALIDATE:
     forth_invalidate(o);
    case ERROR_HALT:
     return -forth_is_invalid(o);
    case ERROR_RECOVER:
     o->m[RSTK] = o->core_size - o->m[STACK_SIZE];
     break;
    }
   case OK:
    break;
  }
 }

 forth_cell_t *m = o->m,
       pc,
       *S = o->S,
       I = o->m[INSTRUCTION],
       f = o->m[TOP],
       w,
       clk;

 assert(m);
 assert(S);

 clk = (1000 * clock()) / CLOCKS_PER_SEC;
# 2264 "libforth.c"
 for(;(pc = m[check_bounds(o, &on_error, (I++), 2264, o->core_size)]);) {
 INNER:
  w = ((m[check_bounds(o, &on_error, (pc++), 2266, o->core_size)]) & (0x7f));
  if(w < LAST_INSTRUCTION) {
   check_depth(o, &on_error, S, (stack_bounds[w]), 2268);
   trace(o,w,S,f);
  }

  switch (w) {







  case PUSH: *++S = f; f = m[check_bounds(o, &on_error, (I++), 2280, o->core_size)]; break;
  case CONST: *++S = f; f = m[check_bounds(o, &on_error, (pc), 2281, o->core_size)]; break;
  case RUN: m[check_bounds(o, &on_error, (++m[RSTK]), 2282, o->core_size)] = I; I = pc; break;
# 2300 "libforth.c"
  case DEFINE:
   m[STATE] = 1;
   if(forth_get_word(o, o->s, (32u)) < 0)
    goto end;
   compile(o, RUN, (char*)o->s, true, false);
   break;
# 2318 "libforth.c"
  case IMMEDIATE:
   w = m[PWD] + 1;
   m[w] &= ~(1u << (15));
   break;
  case READ:
# 2352 "libforth.c"
   if(forth_get_word(o, o->s, (32u)) < 0)
    goto end;
   if ((w = forth_find(o, (char*)o->s)) > 1) {
    pc = w;
    if(m[STATE] && (m[check_bounds(o, &on_error, (pc), 2356, o->core_size)] & (1u << (15)))) {
     m[check_dictionary(o, &on_error, (m[DIC]++))] = pc;
     break;
    }
    goto INNER;
   } else if(forth_string_to_cell(o->m[BASE], &w, (char*)o->s)) {
    error("'%s' is not a word (line %zu)", o->s, o->line);
    longjmp(on_error, RECOVERABLE);
    break;
   }

   if (m[STATE]) {
    m[check_dictionary(o, &on_error, (m[DIC]++))] = 2;
    m[check_dictionary(o, &on_error, (m[DIC]++))] = w;
   } else {
    *++S = f;
    f = w;
   }
   break;
# 2385 "libforth.c"
  case LOAD: f = m[check_bounds(o, &on_error, (f), 2385, o->core_size)]; break;
  case STORE: m[check_bounds(o, &on_error, (f), 2386, o->core_size)] = *S--; f = *S--; break;
  case CLOAD: f = *(((uint8_t*)m) + check_bounds(o, &on_error, (f), 2387, o->core_size * sizeof(forth_cell_t))); break;
  case CSTORE: ((uint8_t*)m)[check_bounds(o, &on_error, (f), 2388, o->core_size * sizeof(forth_cell_t))] = *S--; f = *S--; break;
  case SUB: f = *S-- - f; break;
  case ADD: f = *S-- + f; break;
  case AND: f = *S-- & f; break;
  case OR: f = *S-- | f; break;
  case XOR: f = *S-- ^ f; break;
  case INV: f = ~f; break;
  case SHL: f = *S-- << f; break;
  case SHR: f = *S-- >> f; break;
  case MUL: f = *S-- * f; break;
  case DIV:
   if(f) {
    f = *S-- / f;
   } else {
    error("divide %"PRIdCell" by zero ", *S--);
    longjmp(on_error, RECOVERABLE);
   }
   break;
  case ULESS: f = *S-- < f; break;
  case UMORE: f = *S-- > f; break;
  case EXIT: I = m[check_bounds(o, &on_error, (m[RSTK]--), 2408, o->core_size)]; break;
  case KEY: *++S = f; f = forth_get_char(o); break;
  case EMIT: f = fputc(f, (FILE*)o->m[FOUT]); break;
  case FROMR: *++S = f; f = m[check_bounds(o, &on_error, (m[RSTK]--), 2411, o->core_size)]; break;
  case TOR: m[check_bounds(o, &on_error, (++m[RSTK]), 2412, o->core_size)] = f; f = *S--; break;
  case BRANCH: I += m[check_bounds(o, &on_error, (I), 2413, o->core_size)]; break;
  case QBRANCH: I += f == 0 ? m[I] : 1; f = *S--; break;
  case PNUM: f = print_cell(o, (FILE*)(o->m[FOUT]), f); break;
  case COMMA: m[check_dictionary(o, &on_error, (m[DIC]++))] = f; f = *S--; break;
  case EQUAL: f = *S-- == f; break;
  case SWAP: w = f; f = *S--; *++S = w; break;
  case DUP: *++S = f; break;
  case DROP: f = *S--; break;
  case OVER: w = *S; *++S = f; f = w; break;
# 2448 "libforth.c"
  case TAIL:
   m[RSTK]--;
   break;





  case FIND:
   *++S = f;
   if(forth_get_word(o, o->s, (32u)) < 0)
    goto end;
   f = forth_find(o, (char*)o->s);
   f = f < ((32u)+(32u)) ? 0 : f;
   break;
# 2471 "libforth.c"
  case DEPTH:
   w = S - o->vstart;
   *++S = f;
   f = w;
   break;




  case SPLOAD:
   *++S = f;
   f = (forth_cell_t)(S - o->m);
   break;




  case SPSTORE:
   w = *S--;
   S = (forth_cell_t*)(f + o->m - 1);
   f = w;
   break;





  case CLOCK:
   *++S = f;
   f = ((1000 * clock()) - clk) / CLOCKS_PER_SEC;
   break;
# 2512 "libforth.c"
  case EVALUATOR:
  {

   forth_cell_t sin = o->m[SIN], sidx = o->m[SIDX],
    slen = o->m[SLEN], fin = o->m[FIN],
    source = o->m[SOURCE_ID], r = m[RSTK];
   char *s = NULL;
   FILE *file = NULL;
   forth_cell_t length;
   int file_in = 0;
   file_in = f;
   f = *S--;
   if(file_in) {
    file = (FILE*)(*S--);
    f = *S--;
   } else {
    s = ((char*)o->m + *S--);
    length = f;
    f = *S--;
   }

   o->S = S;
   o->m[TOP] = f;

   m[RSTK]++;
   if(file_in) {
    forth_set_file_input(o, file);
    w = forth_run(o);
   } else {
    w = forth_eval_block(o, s, length);
   }

   m[RSTK] = r;
   S = o->S;
   *++S = o->m[TOP];
   f = w;

   o->m[SIN] = sin;
   o->m[SIDX] = sidx;
   o->m[SLEN] = slen;
   o->m[FIN] = fin;
   o->m[SOURCE_ID] = source;
   if(forth_is_invalid(o))
    return -1;
   break;
  }
  case PSTK: print_stack(o, (FILE*)(o->m[STDOUT]), S, f);
         fputc('\n', (FILE*)(o->m[STDOUT]));
         break;
  case RESTART: longjmp(on_error, f); break;
# 2574 "libforth.c"
  case CALL:
  {
   if(!(o->calls) || !(o->calls->count)) {

    f = -1;
    break;
   }
   forth_cell_t i = f;
   if(i >= (o->calls->count)) {
    f = -1;
    break;
   }

   assert(o->calls->functions[i].function);

   check_depth(o, &on_error, S, (o->calls->functions[i].depth), 2589);

   f = *S--;

   o->S = S;
   o->m[TOP] = f;

   w = o->calls->functions[i].function(o);

   S = o->S;
   f = o->m[TOP];

   *++S = f;
   f = w;
   break;
  }
# 2616 "libforth.c"
  case SYSTEM: f = system(forth_get_string(o, &on_error, &S, f)); break;

  case FCLOSE:
         errno = 0;
         f = fclose((FILE*)f) ? ferrno() : 0;
         break;
  case FDELETE:
         errno = 0;
         f = remove(forth_get_string(o, &on_error, &S, f)) ? ferrno() : 0;
         break;
  case FFLUSH:
         errno = 0;
         f = fflush((FILE*)f) ? ferrno() : 0;
         break;
  case FSEEK:
   {
    errno = 0;
    int r = fseek((FILE*)(*S--), f, SEEK_SET);
    f = r == -1 ? errno ? ferrno() : -1 : 0;
    break;
   }
  case FPOS:
   {
    errno = 0;
    int r = ftell((FILE*)f);
    *++S = r;
    f = r == -1 ? errno ? ferrno() : -1 : 0;
    break;
   }
  case FOPEN:
   {
    const char *fam = forth_get_fam(&on_error, f);
    f = *S--;
    char *file = forth_get_string(o, &on_error, &S, f);
    errno = 0;
    *++S = (forth_cell_t)fopen(file, fam);
    f = ferrno();
   }
   break;
  case FREAD:
   {
    FILE *file = (FILE*)f;
    forth_cell_t count = *S--;
    forth_cell_t offset = *S--;
    *++S = fread(((char*)m)+offset, 1, count, file);
    f = ferror(file);
    clearerr(file);
   }
   break;
  case FWRITE:
   {
    FILE *file = (FILE*)f;
    forth_cell_t count = *S--;
    forth_cell_t offset = *S--;
    *++S = fwrite(((char*)m)+offset, 1, count, file);
    f = ferror(file);
    clearerr(file);
   }
   break;
  case FRENAME:
   {
    const char *f1 = forth_get_fam(&on_error, f);
    f = *S--;
    char *f2 = forth_get_string(o, &on_error, &S, f);
    errno = 0;
    f = rename(f2, f1) ? ferrno() : 0;
   }
   break;
  case TMPFILE:
   {
    *++S = f;
    errno = 0;
    *++S = (forth_cell_t)tmpfile();
    f = errno ? ferrno() : 0;
   }
   break;
  case RAISE:
   f = raise((f*-1) - (-512));
   break;
  case DATE:
   {
    time_t raw;
    struct tm *gmt;
    time(&raw);
    gmt = gmtime(&raw);
    *++S = f;
    *++S = gmt->tm_sec;
    *++S = gmt->tm_min;
    *++S = gmt->tm_hour;
    *++S = gmt->tm_mday;
    *++S = gmt->tm_mon + 1;
    *++S = gmt->tm_year + 1900;
    *++S = gmt->tm_wday;
    *++S = gmt->tm_yday;
    f = gmt->tm_isdst;
    break;
   }
# 2723 "libforth.c"
  case MEMMOVE:
   w = *S--;
   memmove((char*)(*S--), (char*)w, f);
   f = *S--;
   break;
  case MEMCHR:
   w = *S--;
   f = (forth_cell_t)memchr((char*)(*S--), w, f);
   break;
  case MEMSET:
   w = *S--;
   memset((char*)(*S--), w, f);
   f = *S--;
   break;
  case MEMCMP:
   w = *S--;
   f = memcmp((char*)(*S--), (char*)w, f);
   break;
  case ALLOCATE:
   errno = 0;
   *++S = (forth_cell_t)calloc(f, 1);
   f = ferrno();
   break;
  case FREE:






   errno = 0;
   free((char*)f);
   f = ferrno();
   break;
  case RESIZE:
   errno = 0;
   w = (forth_cell_t)realloc((char*)(*S--), f);
   *++S = w;
   f = ferrno();
   break;
  case GETENV:
  {
   char *s = getenv(forth_get_string(o, &on_error, &S, f));
   f = s ? strlen(s) : 0;
   *++S = (forth_cell_t)s;
   break;
  }
  case BYE:
   rval = f;
   f = *S--;
   goto end;




  default:
   fatal("illegal operation %" PRIdCell, w);
   longjmp(on_error, FATAL);
  }
 }







end:
 o->S = S;
 o->m[TOP] = f;
 return rval;
}
# 2817 "libforth.c"
int main_forth(int argc, char **argv)
{
 FILE *core = fopen("forth.core", "rb");
 forth_t *o = NULL;
 int r = 0;
 if(core) {
  o = forth_load_core_file(core);
  fclose(core);
 }
 if(!o)
  o = forth_init(DEFAULT_CORE_SIZE, stdin, stdout, NULL);
 if(!o) {
  fatal("failed to initialize forth: %s", forth_strerror());
  return -1;
 }
 forth_set_args(o, argc, argv);
 if((r = forth_run(o)) < 0)
  return r;
 errno = 0;
 if(!(core = fopen("forth.core", "wb"))) {
  fatal("failed to save core file: %s", forth_strerror());
  return -1;
 }
 fclose(core);
 r = forth_save_core_file(o, core);
 forth_free(o);
 return r;
}
