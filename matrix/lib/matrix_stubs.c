/* The runtime's clock and signal handlers.

   OCaml has no monotonic clock in its distribution, and an OCaml signal
   handler runs only at the next point where OCaml code may run: a signal
   that lands after the runtime's last check and before select(2) blocks is
   not seen until something else wakes the loop, and SIGABRT, which abort()
   raises and never returns from, reaches no such point at all. These
   handlers are C: they record the signal for each run it concerns and
   write one byte to that run's wake pipe, which is async-signal-safe, so a
   waiting loop always wakes. */

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
#include <fcntl.h>
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

CAMLprim value matrix_signal_take(value s, value always, value restart) {
  (void)s; (void)always; (void)restart;
  return Val_false;
}
CAMLprim value matrix_signal_restore(value s) { (void)s; return Val_unit; }
CAMLprim value matrix_signal_ignored(value s) { (void)s; return Val_false; }
CAMLprim value matrix_signal_raise(value s) { (void)s; return Val_unit; }
CAMLprim value matrix_signal_stop_group(value u) { (void)u; return Val_unit; }
CAMLprim value matrix_is_controlling(value fd) { (void)fd; return Val_false; }
CAMLprim value matrix_run_register(value controlling) {
  (void)controlling;
  caml_failwith("Matrix: no wake pipe on Windows");
}
CAMLprim value matrix_run_unregister(value id) { (void)id; return Val_unit; }
CAMLprim value matrix_run_pending(value id, value s) {
  (void)id; (void)s;
  return Val_false;
}
CAMLprim value matrix_run_terminating(value id) { (void)id; return Val_false; }
CAMLprim value matrix_run_settings(value id, value i, value o) {
  (void)id; (void)i; (void)o;
  return Val_unit;
}
CAMLprim value matrix_run_forget_settings(value id) {
  (void)id;
  return Val_unit;
}
CAMLprim value matrix_run_reset_bytes(value id, value s) {
  (void)id; (void)s;
  return Val_unit;
}
CAMLprim value matrix_fatal_restore(value u) { (void)u; return Val_unit; }

#else

/* Whether [fd] is the process's controlling terminal: only that terminal
   has a foreground process group the process can read. */
CAMLprim value matrix_is_controlling(value fd) {
  return Val_bool(tcgetpgrp(Int_val(fd)) != -1);
}

#define SIGNALS 65

/* The runs of the process. A run registers its wake pipe's write end and
   whether its terminal is the controlling one; the handlers record each
   signal for the runs it concerns and wake them. The window and job-control
   signals concern the controlling terminal's runs; the others every run.

   A run also registers what restores its terminal if the process ends where
   no OCaml code runs: its terminal's saved settings, on its input, and the
   bytes that reset what it set, written to its output. The SIGABRT handler
   restores every run's terminal so, and so does the run that ends the
   process on a termination signal, for the other runs.

   The list only grows, from its head, under [runs_lock]; a run that ends
   leaves its node to the next run. A handler walks it without the lock: it
   reads the head once, and a node's fields are set before the node is
   published or marked active. */
#define RESET_MAX 256

struct run {
  struct run *next;
  int id;
  volatile sig_atomic_t active;
  volatile sig_atomic_t controlling;
  int wake_r, wake;
  volatile sig_atomic_t pending[SIGNALS];
  volatile int in, out;
  struct termios settings;
  volatile sig_atomic_t have_settings;
  char reset[RESET_MAX];
  volatile sig_atomic_t reset_length;
};

static struct run *runs = NULL;
static int runs_count = 0;
static pthread_mutex_t runs_lock = PTHREAD_MUTEX_INITIALIZER;

static struct run *runs_head(void) {
  return __atomic_load_n(&runs, __ATOMIC_ACQUIRE);
}

static struct run *find_run(value id) {
  int n = Int_val(id);
  for (struct run *r = runs_head(); r != NULL; r = r->next)
    if (r->id == n) return r;
  caml_invalid_argument("Matrix: unknown run");
}

/* Registers a run and gives its id and its wake pipe's ends, [(id, read,
   write)]. A node is reused once its run has ended, with its pipe, emptied:
   a pipe is never closed, so a handler that wakes a run as it ends never
   writes to a descriptor reused since. */
CAMLprim value matrix_run_register(value controlling) {
  CAMLparam1(controlling);
  CAMLlocal1(result);
  pthread_mutex_lock(&runs_lock);
  struct run *r = runs_head();
  while (r != NULL && r->active) r = r->next;
  if (r == NULL) {
    int fds[2];
    r = calloc(1, sizeof *r);
    if (r == NULL || pipe2(fds, O_CLOEXEC | O_NONBLOCK) != 0) {
      int err = errno;
      free(r);
      pthread_mutex_unlock(&runs_lock);
      unix_error(err, "pipe2", Nothing);
    }
    r->wake_r = fds[0];
    r->wake = fds[1];
    r->id = runs_count++;
    r->next = runs;
    __atomic_store_n(&runs, r, __ATOMIC_RELEASE);
  } else {
    char buf[64];
    while (read(r->wake_r, buf, sizeof buf) > 0) {
    }
  }
  for (int s = 0; s < SIGNALS; s++) r->pending[s] = 0;
  r->have_settings = 0;
  r->reset_length = 0;
  r->in = r->out = -1;
  r->controlling = Bool_val(controlling);
  __atomic_store_n(&r->active, 1, __ATOMIC_RELEASE);
  int id = r->id, rd = r->wake_r, wr = r->wake;
  pthread_mutex_unlock(&runs_lock);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(id));
  Store_field(result, 1, Val_int(rd));
  Store_field(result, 2, Val_int(wr));
  CAMLreturn(result);
}

CAMLprim value matrix_run_unregister(value id) {
  struct run *r = find_run(id);
  pthread_mutex_lock(&runs_lock);
  r->active = 0;
  r->have_settings = 0;
  r->reset_length = 0;
  pthread_mutex_unlock(&runs_lock);
  return Val_unit;
}


/* Whether a signal that ends the process was received for the run and not
   yet taken. */
CAMLprim value matrix_run_terminating(value id) {
  struct run *r = find_run(id);
  return Val_bool(r->pending[SIGINT] || r->pending[SIGTERM] ||
                  r->pending[SIGHUP] || r->pending[SIGQUIT]);
}

/* Saves the settings of the run's terminal on [input], restored with its
   reset bytes written to [output] if the process ends where no OCaml code
   runs. */
CAMLprim value matrix_run_settings(value id, value input, value output) {
  struct run *r = find_run(id);
  struct termios t;
  r->have_settings = 0;
  r->in = Int_val(input);
  r->out = Int_val(output);
  if (tcgetattr(r->in, &t) == 0) {
    r->settings = t;
    r->have_settings = 1;
  }
  return Val_unit;
}

CAMLprim value matrix_run_forget_settings(value id) {
  struct run *r = find_run(id);
  r->have_settings = 0;
  return Val_unit;
}

/* The bytes that reset what the run set; empty once it gave the terminal
   back. */
CAMLprim value matrix_run_reset_bytes(value id, value bytes) {
  struct run *r = find_run(id);
  size_t length = caml_string_length(bytes);
  if (length > RESET_MAX) caml_invalid_argument("Matrix: reset bytes");
  r->reset_length = 0;
  memcpy(r->reset, String_val(bytes), length);
  __atomic_store_n(&r->reset_length, (sig_atomic_t)length, __ATOMIC_RELEASE);
  return Val_unit;
}

static void restore_run(struct run *r) {
  int out = r->out;
  sig_atomic_t length = r->reset_length;
  if (out >= 0 && length > 0) {
    size_t off = 0;
    while (off < (size_t)length) {
      ssize_t n = write(out, r->reset + off, (size_t)length - off);
      if (n > 0) off += (size_t)n;
      else if (n < 0 && errno == EINTR) continue;
      else break;
    }
  }
  if (r->have_settings && r->in >= 0) tcsetattr(r->in, TCSANOW, &r->settings);
}

/* Restores every run's terminal that has not been given back. */
static void restore_runs(void) {
  for (struct run *r = runs_head(); r != NULL; r = r->next)
    if (r->active) restore_run(r);
}

CAMLprim value matrix_fatal_restore(value unit) {
  (void)unit;
  restore_runs();
  return Val_unit;
}

static struct sigaction previous[SIGNALS];
static volatile sig_atomic_t ours[SIGNALS];

static int concerns_controlling_only(int s) {
  return s == SIGWINCH || s == SIGCONT || s == SIGTSTP;
}

static void on_signal(int s, siginfo_t *info, void *context) {
  int saved_errno = errno;
  if (s == SIGABRT) {
    restore_runs();
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
  if (s > 0 && s < SIGNALS) {
    for (struct run *r = runs_head(); r != NULL; r = r->next) {
      if (!r->active) continue;
      if (concerns_controlling_only(s) && !r->controlling) continue;
      r->pending[s] = 1;
      char byte = 0;
      ssize_t n = write(r->wake, &byte, 1);
      (void)n;
    }
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
   [restart] resumes the system calls the signal interrupts. Runs side by
   side take a signal together: the first installs the handler, and the
   action it replaced comes back when the last gives it back. */
static int takers[SIGNALS];
static pthread_mutex_t takers_lock = PTHREAD_MUTEX_INITIALIZER;

CAMLprim value matrix_signal_take(value vs, value always, value restart) {
  int s = system_signal(vs);
  struct sigaction old, sa;
  int taken = 0;
  pthread_mutex_lock(&takers_lock);
  if (ours[s]) {
    takers[s]++;
    taken = 1;
  } else if (sigaction(s, NULL, &old) == 0) {
    int is_default = !(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_DFL;
    if (Bool_val(always) || is_default) {
      memset(&sa, 0, sizeof sa);
      sa.sa_sigaction = on_signal;
      sigemptyset(&sa.sa_mask);
      sa.sa_flags = SA_SIGINFO | (Bool_val(restart) ? SA_RESTART : 0);
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
CAMLprim value matrix_signal_ignored(value vs) {
  int s = system_signal(vs);
  struct sigaction old;
  if (sigaction(s, NULL, &old) != 0) return Val_false;
  return Val_bool(!(old.sa_flags & SA_SIGINFO) && old.sa_handler == SIG_IGN);
}

/* Gives [s] back: the action it had before it was taken comes back once
   no run holds it. */
CAMLprim value matrix_signal_restore(value vs) {
  int s = system_signal(vs);
  pthread_mutex_lock(&takers_lock);
  if (ours[s] && --takers[s] == 0) {
    ours[s] = 0;
    sigaction(s, &previous[s], NULL);
  }
  pthread_mutex_unlock(&takers_lock);
  return Val_unit;
}

/* Whether [s] was received for the run since the last call. */
CAMLprim value matrix_run_pending(value id, value vs) {
  int s = system_signal(vs);
  struct run *r = find_run(id);
  int p = r->pending[s];
  r->pending[s] = 0;
  return Val_bool(p);
}

static void unblock(int s) {
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, s);
  pthread_sigmask(SIG_UNBLOCK, &set, NULL);
}

/* Raises [s] with its default action. */
CAMLprim value matrix_signal_raise(value vs) {
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
CAMLprim value matrix_signal_stop_group(value unit) {
  (void)unit;
  unblock(SIGTSTP);
  kill(0, SIGTSTP);
  return Val_unit;
}

#endif
