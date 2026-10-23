/* The terminal device: its window, its line settings, the monotonic clock,
   and the process's signals as the holders of terminals share them. */

#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
/* On the Windows ports Unix.file_descr is a custom block wrapping a HANDLE,
   accessed with Handle_val; on Unix it is an immediate int. Both ports ship
   caml/unixsupport.h. */
#include <caml/unixsupport.h>

#ifdef _WIN32
#include <windows.h>
#ifndef DISABLE_NEWLINE_AUTO_RETURN
#define DISABLE_NEWLINE_AUTO_RETURN 0x0008
#endif
#else
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#endif

/* The window, [Some (cols, rows)], or [None] where the descriptor reports
   none. */
CAMLprim value matrix_tty_window(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(size, result);
  int cols = 0, rows = 0;
#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE h = Handle_val(fd);
  if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
    cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
  }
#else
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == 0) {
    cols = ws.ws_col;
    rows = ws.ws_row;
  }
#endif
  if (cols <= 0 || rows <= 0) CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(cols));
  Store_field(size, 1, Val_int(rows));
  result = caml_alloc_some(size);
  CAMLreturn(result);
}

CAMLprim value matrix_tty_enable_vt(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  HANDLE h = Handle_val(fd);
  if (h == INVALID_HANDLE_VALUE) caml_failwith("Invalid handle");
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) caml_failwith("GetConsoleMode failed");
  /* DISABLE_NEWLINE_AUTO_RETURN defers the wrap after the last column until
     the next printable character, as a VT100 does (terminfo's xenl): without
     it, a line that fills the width moves the cursor down at once, and the
     line feed that ends it leaves a blank row. */
  if (!SetConsoleMode(h, mode | ENABLE_PROCESSED_OUTPUT |
                             ENABLE_VIRTUAL_TERMINAL_PROCESSING |
                             DISABLE_NEWLINE_AUTO_RETURN))
    caml_failwith("SetConsoleMode failed");
  /* UTF-8 output. This affects the whole process, not only this handle. */
  SetConsoleOutputCP(CP_UTF8);
#else
  (void)fd;
#endif
  CAMLreturn(Val_unit);
}

/* IEXTEN is not representable in OCaml's Unix.terminal_io, so the settings
   reach it through these. With IEXTEN set, the line discipline processes
   VDISCARD (^O) and VLNEXT (^V) even in non-canonical mode, and those bytes
   never reach the program. */

CAMLprim value matrix_tty_get_iexten(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  (void)fd;
  CAMLreturn(Val_false);
#else
  struct termios tio;
  if (tcgetattr(Int_val(fd), &tio) != 0) caml_uerror("tcgetattr", Nothing);
  CAMLreturn(Val_bool((tio.c_lflag & IEXTEN) != 0));
#endif
}

CAMLprim value matrix_tty_set_iexten(value fd, value on) {
  CAMLparam2(fd, on);
#ifdef _WIN32
  (void)fd;
  (void)on;
  CAMLreturn(Val_unit);
#else
  struct termios tio;
  if (tcgetattr(Int_val(fd), &tio) != 0) caml_uerror("tcgetattr", Nothing);
  if (Bool_val(on))
    tio.c_lflag |= IEXTEN;
  else
    tio.c_lflag &= ~(tcflag_t)IEXTEN;
  if (tcsetattr(Int_val(fd), TCSANOW, &tio) != 0)
    caml_uerror("tcsetattr", Nothing);
  CAMLreturn(Val_unit);
#endif
}

/* Whether [fd] is the process's controlling terminal: only that terminal
   has a foreground process group the process can read. */
CAMLprim value matrix_tty_is_controlling(value fd) {
#ifdef _WIN32
  (void)fd;
  return Val_false;
#else
  return Val_bool(tcgetpgrp(Int_val(fd)) != -1);
#endif
}

/* OCaml has no monotonic clock in its distribution. */
CAMLprim value matrix_tty_now(value unit) {
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

/* Signals

   An OCaml signal handler runs only at the next point where OCaml code may
   run: a signal that lands after a loop's last check and before select(2)
   blocks is not seen until something else wakes the loop, and SIGABRT, which
   abort() raises and never returns from, reaches no such point at all. These
   handlers are C: they record the signal for each holder it concerns and
   write one byte to that holder's wake pipe, which is async-signal-safe, so a
   waiting loop always wakes. */

#ifdef _WIN32

CAMLprim value matrix_tty_take(value s) {
  (void)s;
  return Val_false;
}
CAMLprim value matrix_tty_give_back(value s) {
  (void)s;
  return Val_unit;
}
CAMLprim value matrix_tty_ignored(value s) {
  (void)s;
  return Val_false;
}
CAMLprim value matrix_tty_raise(value s) {
  (void)s;
  return Val_unit;
}
CAMLprim value matrix_tty_stop_group(value u) {
  (void)u;
  return Val_unit;
}
CAMLprim value matrix_tty_hold(value controlling, value output) {
  (void)controlling;
  (void)output;
  caml_failwith("Tty.Signal.hold: no wake pipe on Windows");
}
CAMLprim value matrix_tty_unhold(value id) {
  (void)id;
  return Val_unit;
}
CAMLprim value matrix_tty_received(value id, value s) {
  (void)id;
  (void)s;
  return Val_false;
}
CAMLprim value matrix_tty_terminating(value id) {
  (void)id;
  return Val_false;
}
CAMLprim value matrix_tty_keep_settings(value id, value i) {
  (void)id;
  (void)i;
  return Val_unit;
}
CAMLprim value matrix_tty_forget_settings(value id) {
  (void)id;
  return Val_unit;
}
CAMLprim value matrix_tty_set_reset(value id, value s) {
  (void)id;
  (void)s;
  return Val_unit;
}
CAMLprim value matrix_tty_restore_all(value u) {
  (void)u;
  return Val_unit;
}

#else

#define SIGNALS 65

/* The holders of the process. A holder registers its wake pipe's write end
   and whether its terminal is the controlling one; the handlers record each
   signal for the holders it concerns and wake them. The window and
   job-control signals concern the controlling terminal's holders; the others
   every holder.

   A holder also registers what restores its terminal if the process ends
   where no OCaml code runs: its terminal's saved settings, on its input, and
   the bytes that reset what it set, written to its output. The SIGABRT
   handler restores every holder's terminal so, and {!restore_all} does it
   from OCaml for a holder that ends the process on a termination signal.

   The list only grows, from its head, under [holders_lock]; a holder that
   ends leaves its node to the next one. A handler walks it without the lock:
   it reads the head once, and a node's fields are set before the node is
   published or marked active.

   The reset bytes are as long as the holder needs, so they are allocated,
   and a handler may be writing them when the holder replaces them: new bytes
   are published whole, by one pointer, and the old ones are freed only once
   no reader is counted. A reader counts itself before it reads the pointer,
   and the holder counts the readers after it replaced it, so a reader it does
   not count reads the new bytes. Bytes a counted reader may hold wait in
   [retired] for a moment when none is counted. */
struct reset {
  struct reset *next;
  size_t length;
  char bytes[];
};

struct holder {
  struct holder *next;
  int id;
  volatile sig_atomic_t active;
  volatile sig_atomic_t controlling;
  int wake_r, wake;
  volatile sig_atomic_t pending[SIGNALS];
  volatile int in, out;
  struct termios settings;
  volatile sig_atomic_t have_settings;
  struct reset *reset;
  int readers;
  struct reset *retired;
};

static struct holder *holders = NULL;
static int holders_count = 0;
static pthread_mutex_t holders_lock = PTHREAD_MUTEX_INITIALIZER;

static struct holder *holders_head(void) {
  return __atomic_load_n(&holders, __ATOMIC_ACQUIRE);
}

static struct holder *find_holder(value id) {
  int n = Int_val(id);
  for (struct holder *h = holders_head(); h != NULL; h = h->next)
    if (h->id == n) return h;
  caml_invalid_argument("Tty.Signal: unknown holder");
}

/* Registers a holder whose reset bytes go to [output], if [Some], and
   gives its id and its wake pipe's ends, [(id, read, write)]. A node is reused once its holder has ended, with its pipe,
   emptied: a pipe is never closed, so a handler that wakes a holder as it
   ends never writes to a descriptor reused since. */
CAMLprim value matrix_tty_hold(value controlling, value output) {
  CAMLparam2(controlling, output);
  CAMLlocal1(result);
  pthread_mutex_lock(&holders_lock);
  struct holder *h = holders_head();
  while (h != NULL && h->active) h = h->next;
  if (h == NULL) {
    int fds[2];
    h = calloc(1, sizeof *h);
    if (h == NULL || pipe2(fds, O_CLOEXEC | O_NONBLOCK) != 0) {
      int err = errno;
      free(h);
      pthread_mutex_unlock(&holders_lock);
      caml_unix_error(err, "pipe2", Nothing);
    }
    h->wake_r = fds[0];
    h->wake = fds[1];
    h->id = holders_count++;
    h->next = holders;
    __atomic_store_n(&holders, h, __ATOMIC_RELEASE);
  } else {
    char buf[64];
    while (read(h->wake_r, buf, sizeof buf) > 0) {
    }
  }
  for (int s = 0; s < SIGNALS; s++) h->pending[s] = 0;
  h->have_settings = 0;
  h->in = -1;
  h->out = Is_some(output) ? Int_val(Some_val(output)) : -1;
  h->controlling = Bool_val(controlling);
  __atomic_store_n(&h->active, 1, __ATOMIC_RELEASE);
  int id = h->id, rd = h->wake_r, wr = h->wake;
  pthread_mutex_unlock(&holders_lock);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(id));
  Store_field(result, 1, Val_int(rd));
  Store_field(result, 2, Val_int(wr));
  CAMLreturn(result);
}

/* Publishes [r] as [h]'s reset bytes and retires the ones it replaces.
   Called under [holders_lock]. */
static void publish_reset(struct holder *h, struct reset *r) {
  struct reset *old = __atomic_exchange_n(&h->reset, r, __ATOMIC_SEQ_CST);
  if (old != NULL) {
    old->next = h->retired;
    h->retired = old;
  }
  if (__atomic_load_n(&h->readers, __ATOMIC_SEQ_CST) == 0) {
    while (h->retired != NULL) {
      struct reset *next = h->retired->next;
      free(h->retired);
      h->retired = next;
    }
  }
}

CAMLprim value matrix_tty_unhold(value id) {
  struct holder *h = find_holder(id);
  pthread_mutex_lock(&holders_lock);
  h->active = 0;
  h->have_settings = 0;
  publish_reset(h, NULL);
  pthread_mutex_unlock(&holders_lock);
  return Val_unit;
}

/* Whether a signal that ends the process was received for the holder and
   not yet taken. */
CAMLprim value matrix_tty_terminating(value id) {
  struct holder *h = find_holder(id);
  return Val_bool(h->pending[SIGINT] || h->pending[SIGTERM] ||
                  h->pending[SIGHUP] || h->pending[SIGQUIT]);
}

/* Saves the settings of the holder's terminal on [input], restored if the
   process ends where no OCaml code runs. */
CAMLprim value matrix_tty_keep_settings(value id, value input) {
  struct holder *h = find_holder(id);
  struct termios t;
  h->have_settings = 0;
  h->in = Int_val(input);
  if (tcgetattr(h->in, &t) == 0) {
    h->settings = t;
    h->have_settings = 1;
  }
  return Val_unit;
}

CAMLprim value matrix_tty_forget_settings(value id) {
  struct holder *h = find_holder(id);
  h->have_settings = 0;
  return Val_unit;
}

/* The bytes that reset what the holder set; empty once it gave the terminal
   back. */
CAMLprim value matrix_tty_set_reset(value id, value bytes) {
  struct holder *h = find_holder(id);
  size_t length = caml_string_length(bytes);
  struct reset *r = NULL;
  if (length > 0) {
    r = malloc(sizeof *r + length);
    if (r == NULL) caml_raise_out_of_memory();
    r->next = NULL;
    r->length = length;
    memcpy(r->bytes, String_val(bytes), length);
  }
  pthread_mutex_lock(&holders_lock);
  publish_reset(h, r);
  pthread_mutex_unlock(&holders_lock);
  return Val_unit;
}

static void restore_holder(struct holder *h) {
  int out = h->out;
  __atomic_add_fetch(&h->readers, 1, __ATOMIC_SEQ_CST);
  struct reset *r = __atomic_load_n(&h->reset, __ATOMIC_SEQ_CST);
  if (out >= 0 && r != NULL) {
    size_t off = 0;
    while (off < r->length) {
      ssize_t n = write(out, r->bytes + off, r->length - off);
      if (n > 0)
        off += (size_t)n;
      else if (n < 0 && errno == EINTR)
        continue;
      else
        break;
    }
  }
  __atomic_sub_fetch(&h->readers, 1, __ATOMIC_SEQ_CST);
  if (h->have_settings && h->in >= 0) tcsetattr(h->in, TCSANOW, &h->settings);
}

/* Restores every holder's terminal that has not been given back. */
static void restore_holders(void) {
  for (struct holder *h = holders_head(); h != NULL; h = h->next)
    if (h->active) restore_holder(h);
}

CAMLprim value matrix_tty_restore_all(value unit) {
  (void)unit;
  restore_holders();
  return Val_unit;
}

/* The policy, one per signal. SIGWINCH and SIGCONT are taken whatever their
   action, a handler installed before still called; the others only at
   their default action. A signal that ends the process resumes no system
   call it interrupts; the others resume them. */
static int in_policy(int s) {
  return s == SIGINT || s == SIGTERM || s == SIGHUP || s == SIGQUIT ||
         s == SIGABRT || s == SIGTSTP || s == SIGWINCH || s == SIGCONT;
}

static int taken_always(int s) { return s == SIGWINCH || s == SIGCONT; }

static int resumes(int s) {
  return s == SIGWINCH || s == SIGCONT || s == SIGTSTP || s == SIGABRT;
}

static int concerns_controlling_only(int s) {
  return s == SIGWINCH || s == SIGCONT || s == SIGTSTP;
}

static struct sigaction previous[SIGNALS];
static volatile sig_atomic_t ours[SIGNALS];

static void on_signal(int s, siginfo_t *info, void *context) {
  int saved_errno = errno;
  if (s == SIGABRT) {
    restore_holders();
    /* Blocked while this handler runs, the signal raised again is delivered
       with its default action once the handler returns. */
    struct sigaction dfl;
    memset(&dfl, 0, sizeof dfl);
    dfl.sa_handler = SIG_DFL;
    sigemptyset(&dfl.sa_mask);
    sigaction(SIGABRT, &dfl, NULL);
    raise(SIGABRT);
  } else if (taken_always(s)) {
    /* A handler installed before is still called. */
    struct sigaction *p = &previous[s];
    if (p->sa_flags & SA_SIGINFO) {
      if (p->sa_sigaction != NULL) p->sa_sigaction(s, info, context);
    } else if (p->sa_handler != SIG_DFL && p->sa_handler != SIG_IGN) {
      p->sa_handler(s);
    }
  }
  if (s > 0 && s < SIGNALS) {
    for (struct holder *h = holders_head(); h != NULL; h = h->next) {
      if (!h->active) continue;
      if (concerns_controlling_only(s) && !h->controlling) continue;
      h->pending[s] = 1;
      char byte = 0;
      ssize_t n = write(h->wake, &byte, 1);
      (void)n;
    }
  }
  errno = saved_errno;
}

extern int caml_convert_signal_number(int);

static int system_signal(value s) {
  int n = caml_convert_signal_number(Int_val(s));
  if (n <= 0 || n >= SIGNALS)
    caml_invalid_argument("Tty.Signal: signal number");
  return n;
}

/* Takes [s] as the policy says. Holders side by side take a signal
   together: the first installs the handler, and the action it replaced
   comes back when the last gives it back. */
static int takers[SIGNALS];
static pthread_mutex_t takers_lock = PTHREAD_MUTEX_INITIALIZER;

CAMLprim value matrix_tty_take(value vs) {
  int s = system_signal(vs);
  if (!in_policy(s))
    caml_invalid_argument("Tty.Signal.take: not a terminal's signal");
  struct sigaction old, sa;
  int taken = 0;
  pthread_mutex_lock(&takers_lock);
  if (ours[s]) {
    takers[s]++;
    taken = 1;
  } else if (sigaction(s, NULL, &old) == 0) {
    int is_default = !(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_DFL;
    if (taken_always(s) || is_default) {
      memset(&sa, 0, sizeof sa);
      sa.sa_sigaction = on_signal;
      sigemptyset(&sa.sa_mask);
      sa.sa_flags = SA_SIGINFO | (resumes(s) ? SA_RESTART : 0);
      previous[s] = old;
      ours[s] = 1;
      if (sigaction(s, &sa, NULL) == 0) {
        takers[s] = 1;
        taken = 1;
      } else
        ours[s] = 0;
    }
  }
  pthread_mutex_unlock(&takers_lock);
  return Val_bool(taken);
}

/* Whether the action of [s] is to ignore it. */
CAMLprim value matrix_tty_ignored(value vs) {
  int s = system_signal(vs);
  struct sigaction old;
  if (sigaction(s, NULL, &old) != 0) return Val_false;
  return Val_bool(!(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_IGN);
}

/* Gives [s] back: the action it had before it was taken comes back once no
   holder holds it. */
CAMLprim value matrix_tty_give_back(value vs) {
  int s = system_signal(vs);
  pthread_mutex_lock(&takers_lock);
  if (ours[s] && --takers[s] == 0) {
    ours[s] = 0;
    sigaction(s, &previous[s], NULL);
  }
  pthread_mutex_unlock(&takers_lock);
  return Val_unit;
}

/* Whether [s] was received for the holder since the last call. */
CAMLprim value matrix_tty_received(value id, value vs) {
  int s = system_signal(vs);
  struct holder *h = find_holder(id);
  int p = h->pending[s];
  h->pending[s] = 0;
  return Val_bool(p);
}

static void unblock(int s) {
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, s);
  pthread_sigmask(SIG_UNBLOCK, &set, NULL);
}

/* Raises [s] with its default action. */
CAMLprim value matrix_tty_raise(value vs) {
  int s = system_signal(vs);
  struct sigaction dfl;
  memset(&dfl, 0, sizeof dfl);
  dfl.sa_handler = SIG_DFL;
  sigemptyset(&dfl.sa_mask);
  sigaction(s, &dfl, NULL);
  ours[s] = 0;
  takers[s] = 0;
  unblock(s);
  raise(s);
  return Val_unit;
}

/* Sends SIGTSTP to the process's group, as the terminal's suspend character
   does. */
CAMLprim value matrix_tty_stop_group(value unit) {
  (void)unit;
  unblock(SIGTSTP);
  kill(0, SIGTSTP);
  return Val_unit;
}

#endif
