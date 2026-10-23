/* The runtime's clock and signal handlers.

   OCaml has no monotonic clock in its distribution, and an OCaml signal
   handler runs only at the next point where OCaml code may run: a signal
   that lands after the runtime's last check and before select(2) blocks is
   not seen until something else wakes the loop, and SIGABRT, which abort()
   raises and never returns from, reaches no such point at all. These
   handlers are C: they record the signal and write one byte to the wake
   pipe, which is async-signal-safe, so a waiting loop always wakes. */

#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#endif

/* Clock */

CAMLprim value matrix_now_ns(value unit) {
  (void)unit;
#ifdef _WIN32
  static LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  if (frequency.QuadPart == 0) QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&counter);
  return Val_long((intnat)(counter.QuadPart / frequency.QuadPart) * 1000000000 +
                  (intnat)(counter.QuadPart % frequency.QuadPart) * 1000000000 /
                      frequency.QuadPart);
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return Val_long((intnat)ts.tv_sec * 1000000000 + ts.tv_nsec);
#endif
}

#ifdef _WIN32

CAMLprim value matrix_set_wake_fd(value fd) { (void)fd; return Val_unit; }
CAMLprim value matrix_signal_take(value s, value always, value restart) {
  (void)s; (void)always; (void)restart;
  return Val_false;
}
CAMLprim value matrix_signal_restore(value s) { (void)s; return Val_unit; }
CAMLprim value matrix_signal_ignored(value s) { (void)s; return Val_false; }
CAMLprim value matrix_signal_pending(value s) { (void)s; return Val_false; }
CAMLprim value matrix_signal_terminating(value u) { (void)u; return Val_false; }
CAMLprim value matrix_signal_raise(value s) { (void)s; return Val_unit; }
CAMLprim value matrix_signal_stop_group(value u) { (void)u; return Val_unit; }
CAMLprim value matrix_abort_tty(value i, value o) {
  (void)i; (void)o;
  return Val_unit;
}
CAMLprim value matrix_abort_clear(value u) { (void)u; return Val_unit; }
CAMLprim value matrix_abort_reset(value s) { (void)s; return Val_unit; }

#else

/* The wake pipe's write end, which the handlers write to. */
static volatile int wake_w = -1;

CAMLprim value matrix_set_wake_fd(value fd) {
  wake_w = Int_val(fd);
  return Val_unit;
}

/* What [abort()] leaves: the terminal's saved settings, restored on the
   input, and the bytes that reset the modes, written to the output. */
static int abort_in = -1, abort_out = -1;
static struct termios abort_termios;
static volatile int abort_have_termios = 0;
static char *volatile abort_bytes = NULL;
static volatile size_t abort_length = 0;

static void restore_after_abort(void) {
  char *bytes = abort_bytes;
  size_t length = abort_length;
  if (abort_out >= 0 && bytes != NULL) {
    size_t off = 0;
    while (off < length) {
      ssize_t n = write(abort_out, bytes + off, length - off);
      if (n > 0) off += (size_t)n;
      else if (n < 0 && errno == EINTR) continue;
      else break;
    }
  }
  if (abort_have_termios && abort_in >= 0)
    tcsetattr(abort_in, TCSANOW, &abort_termios);
}

#define SIGNALS 65
static volatile sig_atomic_t pending[SIGNALS];
static struct sigaction previous[SIGNALS];
static volatile sig_atomic_t ours[SIGNALS];

static void on_signal(int s, siginfo_t *info, void *context) {
  int saved_errno = errno;
  if (s > 0 && s < SIGNALS) pending[s] = 1;
  if (s == SIGABRT) {
    restore_after_abort();
    /* Blocked while this handler runs, the signal raised again is delivered
       with its default action once the handler returns. */
    struct sigaction dfl;
    memset(&dfl, 0, sizeof dfl);
    dfl.sa_handler = SIG_DFL;
    sigemptyset(&dfl.sa_mask);
    sigaction(SIGABRT, &dfl, NULL);
    raise(SIGABRT);
  } else if (s == SIGWINCH || s == SIGCONT) {
    /* A handler installed before is still called. */
    struct sigaction *p = &previous[s];
    if (p->sa_flags & SA_SIGINFO) {
      if (p->sa_sigaction != NULL) p->sa_sigaction(s, info, context);
    } else if (p->sa_handler != SIG_DFL && p->sa_handler != SIG_IGN) {
      p->sa_handler(s);
    }
  }
  if (wake_w >= 0) {
    char byte = 0;
    ssize_t n = write(wake_w, &byte, 1);
    (void)n;
  }
  errno = saved_errno;
}

extern int caml_convert_signal_number(int);

static int system_signal(value s) {
  int n = caml_convert_signal_number(Int_val(s));
  if (n <= 0 || n >= SIGNALS) caml_invalid_argument("Matrix: signal number");
  return n;
}

/* Takes [s] if its action is the default, or whatever it is when [always].
   A signal already taken is left as it is. [restart] resumes the system
   calls the signal interrupts. */
CAMLprim value matrix_signal_take(value vs, value always, value restart) {
  int s = system_signal(vs);
  struct sigaction old, sa;
  if (ours[s]) return Val_false;
  if (sigaction(s, NULL, &old) != 0) return Val_false;
  int is_default = !(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_DFL;
  if (!Bool_val(always) && !is_default) return Val_false;
  memset(&sa, 0, sizeof sa);
  sa.sa_sigaction = on_signal;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | (Bool_val(restart) ? SA_RESTART : 0);
  previous[s] = old;
  pending[s] = 0;
  ours[s] = 1;
  if (sigaction(s, &sa, NULL) != 0) {
    ours[s] = 0;
    return Val_false;
  }
  return Val_true;
}

/* Whether the action of [s] is to ignore it. */
CAMLprim value matrix_signal_ignored(value vs) {
  int s = system_signal(vs);
  struct sigaction old;
  if (sigaction(s, NULL, &old) != 0) return Val_false;
  return Val_bool(!(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_IGN);
}

/* Puts back the action [s] had before it was taken. */
CAMLprim value matrix_signal_restore(value vs) {
  int s = system_signal(vs);
  if (ours[s]) {
    ours[s] = 0;
    sigaction(s, &previous[s], NULL);
    pending[s] = 0;
  }
  return Val_unit;
}

/* Whether [s] was received since the last call. */
CAMLprim value matrix_signal_pending(value vs) {
  int s = system_signal(vs);
  int p = pending[s];
  pending[s] = 0;
  return Val_bool(p);
}

/* Whether a signal that ends the process was received and not yet taken. */
CAMLprim value matrix_signal_terminating(value unit) {
  (void)unit;
  return Val_bool(pending[SIGINT] || pending[SIGTERM] || pending[SIGHUP] ||
                  pending[SIGQUIT]);
}

static void unblock(int s) {
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, s);
  pthread_sigmask(SIG_UNBLOCK, &set, NULL);
}

/* Raises [s] with its default action, which the caller has restored. */
CAMLprim value matrix_signal_raise(value vs) {
  int s = system_signal(vs);
  unblock(s);
  raise(s);
  return Val_unit;
}

/* Sends SIGTSTP to the process's group, as the terminal's suspend character
   does. */
CAMLprim value matrix_signal_stop_group(value unit) {
  (void)unit;
  unblock(SIGTSTP);
  kill(0, SIGTSTP);
  return Val_unit;
}

/* Saves the settings of the terminal on [input], restored with the mode
   reset on [output] if the process aborts. */
CAMLprim value matrix_abort_tty(value input, value output) {
  struct termios t;
  abort_have_termios = 0;
  abort_in = Int_val(input);
  abort_out = Int_val(output);
  if (tcgetattr(abort_in, &t) == 0) {
    abort_termios = t;
    abort_have_termios = 1;
  }
  return Val_unit;
}

CAMLprim value matrix_abort_clear(value unit) {
  (void)unit;
  abort_have_termios = 0;
  abort_in = -1;
  abort_out = -1;
  return Val_unit;
}

/* The bytes that reset the modes the run set. Replaced when a run begins,
   on its own thread, so no handler is reading the copy it frees. */
CAMLprim value matrix_abort_reset(value bytes) {
  size_t length = caml_string_length(bytes);
  char *old = abort_bytes;
  char *copy = malloc(length + 1);
  abort_bytes = NULL;
  abort_length = 0;
  free(old);
  if (copy == NULL) return Val_unit;
  memcpy(copy, String_val(bytes), length);
  abort_length = length;
  abort_bytes = copy;
  return Val_unit;
}

#endif
