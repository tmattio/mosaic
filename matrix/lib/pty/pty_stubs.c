// POSIX PTY stubs. On platforms without POSIX PTYs — including Windows —
// every entry point raises Unix_error(ENOSYS, ...). A previous ConPTY
// backend stored a custom block where the OCaml side expects a
// Unix.file_descr, which made every I/O call reinterpret unrelated memory
// as a handle; failing loudly is the honest scope until a sound Windows
// implementation exists.

#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define MATRIX_HAS_POSIX_PTY 1
#endif

#ifdef MATRIX_HAS_POSIX_PTY
#define _GNU_SOURCE  // For ptsname on Linux
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#ifdef __linux__
#include <sys/syscall.h>
#endif
#endif  // MATRIX_HAS_POSIX_PTY

#include <errno.h>

// OCaml C FFI headers
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifdef MATRIX_HAS_POSIX_PTY

// A C struct to hold the file descriptor pair.
struct pty_fds {
  int master_fd;
  int slave_fd;
};

// This is the modern, portable way to open a PTY.
struct pty_fds c_open_pty_posix() {
  struct pty_fds fds = {-1, -1};

  // Enter blocking section for potentially blocking syscalls
  caml_enter_blocking_section();

  // Both ends are close-on-exec (see open_pty in pty.mli), set by the open
  // itself so that no child forked meanwhile by another thread inherits them.
  int master_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master_fd == -1) {
    caml_leave_blocking_section();
    return fds;
  }

  // Grant permissions to the slave device
  if (grantpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  // Unlock the slave device
  if (unlockpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  caml_leave_blocking_section();

  // Get slave name - use thread-safe ptsname_r where available
  char *slave_name = NULL;
#ifdef __GLIBC__
  char slave_name_buf[PATH_MAX];
  if (ptsname_r(master_fd, slave_name_buf, sizeof(slave_name_buf)) != 0) {
    close(master_fd);
    return fds;
  }
  slave_name = slave_name_buf;
#else
  slave_name = ptsname(master_fd);
  if (slave_name == NULL) {
    close(master_fd);
    return fds;
  }
#endif

  caml_enter_blocking_section();
  fds.slave_fd = open(slave_name, O_RDWR | O_NOCTTY | O_CLOEXEC);
  caml_leave_blocking_section();

  if (fds.slave_fd == -1) {
    close(master_fd);
    // CRITICAL FIX: Reset master_fd to indicate failure
    fds.master_fd = -1;
    fds.slave_fd = -1;
    return fds;
  }

  fds.master_fd = master_fd;
  return fds;
}

CAMLprim value ocaml_pty_open(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);

  struct pty_fds fds = c_open_pty_posix();

  if (fds.master_fd == -1) {
    uerror("open_pty", Nothing);
  }

  res = caml_alloc(2, 0);
  Store_field(res, 0, Val_int(fds.master_fd));
  Store_field(res, 1, Val_int(fds.slave_fd));
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_get_winsize(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);
  struct winsize ws;

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCGWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCGWINSZ)", Nothing);
  }

  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_int(ws.ws_row));
  Store_field(res, 1, Val_int(ws.ws_col));
  Store_field(res, 2, Val_int(ws.ws_xpixel));
  Store_field(res, 3, Val_int(ws.ws_ypixel));
  CAMLreturn(res);
}

// Byte version for set_winsize is needed for records with non-float fields
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  CAMLparam2(fd, ws_val);
  struct winsize ws;
  ws.ws_row = Int_val(Field(ws_val, 0));
  ws.ws_col = Int_val(Field(ws_val, 1));
  ws.ws_xpixel = Int_val(Field(ws_val, 2));
  ws.ws_ypixel = Int_val(Field(ws_val, 3));

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCSWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCSWINSZ)", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}

// What the child of spawn needs between fork and exec, built by the parent.
struct child {
  int slave;
  int report;           // the write end of the error pipe
  const char *cwd;      // NULL to stay in the parent's directory
  char **paths;         // where to look for the program, in order
  char **argv, **envp;  // NULL-terminated
  char **sh_argv;       // {"/bin/sh", NULL, argv[1], ...}
  int fd_limit;         // no descriptor is opened at this number or above
  int last_signal;
  struct sigaction default_action;
  sigset_t no_signals;
};

// The steps of the child that can fail, as the error names them.
enum step { SETSID, SETCTTY, DUP2, CHDIR, EXEC };
static const char *const step_names[] = {"setsid", "ioctl(TIOCSCTTY)",
                                         "dup2", "chdir", "execvp"};

// Closes every descriptor from [low] up. POSIX has no call for this, nor a
// way to list a process's descriptors: Linux (5.9) has close_range and the
// BSDs closefrom, one system call each. Elsewhere, as on macOS, and on older
// Linux, close each number below [limit], the RLIMIT_NOFILE soft limit. A
// descriptor opened before that limit was lowered can lie above it, and
// stays open there.
static void close_from(int low, int limit) {
#if defined(__linux__) && defined(SYS_close_range)
  if (syscall(SYS_close_range, low, ~0U, 0) == 0) return;
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
    defined(__DragonFly__)
  closefrom(low);
  return;
#endif
  for (int fd = low; fd < limit; fd++) close(fd);
}

// The child's side of spawn, which returns only on failure, with the step
// that failed and errno set. The parent may run other threads, among them
// OCaml domains, whose locks the fork copied in whatever state they were in:
// POSIX lets the child of a multithreaded process call only async-signal-safe
// functions until it execs. Every call here is one, except ioctl, a system
// call that POSIX does not define, and close_range or closefrom, system calls
// that it does not define either. Nothing here allocates: the parent built
// the strings and arrays.
static enum step exec_child(struct child *c) {
  // Keep the report off the std fds, which the slave replaces.
  int report = fcntl(c->report, F_DUPFD_CLOEXEC, 3);
  if (report == -1) return DUP2;
  c->report = report;
  if (setsid() == -1) return SETSID;
  if (ioctl(c->slave, TIOCSCTTY, 0) == -1) return SETCTTY;
  // The slave is close-on-exec. dup2 clears the flag on a copy, but does
  // nothing when the slave already is that std fd.
  for (int fd = 0; fd <= 2; fd++)
    if ((c->slave == fd ? fcntl(fd, F_SETFD, 0) : dup2(c->slave, fd)) == -1)
      return DUP2;
  // Exec keeps every descriptor that is not close-on-exec, so the program
  // would inherit any the parent opened without the flag, and hold pipes and
  // terminals open that their readers wait to see closed. Keep only the std
  // fds: park the report at 3, still close-on-exec, and close every other.
  if (c->report != 3) {
    if (dup2(c->report, 3) == -1 || fcntl(3, F_SETFD, FD_CLOEXEC) == -1)
      return DUP2;
    c->report = 3;
  }
  close_from(4, c->fd_limit);
  if (c->cwd != NULL && chdir(c->cwd) == -1) return CHDIR;
  // Exec keeps the signal mask, and the signals the parent ignores, such as
  // the SIGPIPE that many programs ignore, stay ignored: start the program
  // with every signal's default action and none blocked. Signals have been
  // blocked since before the fork, so no handler of the parent's runs here.
  // sigaction fails, harmlessly, for SIGKILL, SIGSTOP and the signals that
  // the C library reserves.
  for (int sig = 1; sig <= c->last_signal; sig++)
    sigaction(sig, &c->default_action, NULL);
  pthread_sigmask(SIG_SETMASK, &c->no_signals, NULL);
  // As execvp does: a directory without the program, or that is not a
  // directory, moves on to the next; a file without permission is
  // remembered; any other failure ends the search. A file the kernel cannot
  // execute (ENOEXEC) is a script without #!, which /bin/sh runs.
  int err = ENOENT;
  for (char **path = c->paths; *path != NULL; path++) {
    execve(*path, c->argv, c->envp);
    if (errno == ENOEXEC) {
      c->sh_argv[1] = *path;
      execve("/bin/sh", c->sh_argv, c->envp);
    }
    if (errno == EACCES)
      err = EACCES;
    else if (errno != ENOENT && errno != ENOTDIR) {
      err = errno;
      break;
    }
  }
  errno = err;
  return EXEC;
}

// A pipe whose ends are both close-on-exec. macOS has no pipe2: a fork by
// another thread between pipe and fcntl leaks the ends into that child.
static int cloexec_pipe(int fds[2]) {
#ifdef __APPLE__
  if (pipe(fds) == -1) return -1;
  fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  return 0;
#else
  return pipe2(fds, O_CLOEXEC);
#endif
}

static void check_strings(value v, const char *cmd) {
  for (mlsize_t i = 0; i < Wosize_val(v); i++)
    if (!caml_string_is_c_safe(Field(v, i)))
      caml_unix_error(EINVAL, cmd, Field(v, i));
}

// The child reports a failure before exec on a close-on-exec pipe: the
// parent reads the step and errno, or the end of file that a successful
// exec gives when it closes the pipe. So spawn returns once the program
// runs, and raises the child's error otherwise, having reaped the child.
CAMLprim value ocaml_pty_fork_exec(value slave, value paths, value argv,
                                   value env, value cwd) {
  CAMLparam5(slave, paths, argv, env, cwd);
  check_strings(paths, "execvp");
  check_strings(argv, "execvp");
  check_strings(env, "execvp");
  if (Is_some(cwd)) caml_unix_check_path(Some_val(cwd), "chdir");

  int pipe_fds[2];
  if (cloexec_pipe(pipe_fds) == -1) caml_uerror("pipe", Nothing);
  struct child c;
  c.slave = Int_val(slave);
  c.report = pipe_fds[1];
  c.cwd = Is_some(cwd) ? caml_stat_strdup(String_val(Some_val(cwd))) : NULL;
  c.paths = caml_unix_cstringvect(paths, "execvp");
  c.argv = caml_unix_cstringvect(argv, "execvp");
  c.envp = caml_unix_cstringvect(env, "execvp");
  mlsize_t argc = Wosize_val(argv);
  c.sh_argv = caml_stat_alloc((argc + 2) * sizeof(char *));
  c.sh_argv[0] = "/bin/sh";
  c.sh_argv[1] = NULL;
  for (mlsize_t i = 1; i <= argc; i++) c.sh_argv[i + 1] = c.argv[i];
  struct rlimit nofile;
  c.fd_limit = getrlimit(RLIMIT_NOFILE, &nofile) == 0 &&
                       nofile.rlim_cur != RLIM_INFINITY &&
                       nofile.rlim_cur < INT_MAX
                   ? (int)nofile.rlim_cur
                   : INT_MAX;
#ifdef SIGRTMAX
  c.last_signal = SIGRTMAX > NSIG - 1 ? SIGRTMAX : NSIG - 1;
#else
  c.last_signal = NSIG - 1;
#endif
  memset(&c.default_action, 0, sizeof c.default_action);
  c.default_action.sa_handler = SIG_DFL;
  sigemptyset(&c.default_action.sa_mask);
  sigemptyset(&c.no_signals);

  sigset_t all_signals, parent_signals;
  sigfillset(&all_signals);
  pthread_sigmask(SIG_SETMASK, &all_signals, &parent_signals);
  pid_t pid = fork();
  if (pid == 0) {
    int failure[2];
    failure[0] = exec_child(&c);
    failure[1] = errno;
    // One write of a few bytes to a pipe is atomic.
    (void)!write(c.report, failure, sizeof failure);
    _exit(127);
  }
  int fork_errno = errno;
  pthread_sigmask(SIG_SETMASK, &parent_signals, NULL);
  close(pipe_fds[1]);
  int failure[2];
  ssize_t n = 0;
  if (pid != -1) {
    caml_enter_blocking_section();
    do n = read(pipe_fds[0], failure, sizeof failure);
    while (n == -1 && errno == EINTR);
    if (n == sizeof failure)
      while (waitpid(pid, NULL, 0) == -1 && errno == EINTR);
    caml_leave_blocking_section();
  }
  close(pipe_fds[0]);

  caml_stat_free((char *)c.cwd);
  caml_unix_cstringvect_free(c.paths);
  caml_unix_cstringvect_free(c.argv);
  caml_unix_cstringvect_free(c.envp);
  caml_stat_free(c.sh_argv);
  if (pid == -1) caml_unix_error(fork_errno, "fork", Nothing);
  if (n == sizeof failure) {
    enum step step = failure[0];
    value arg = step == CHDIR  ? Some_val(cwd)
                : step == EXEC ? Field(argv, 0)
                               : Nothing;
    caml_unix_error(failure[1], step_names[step], arg);
  }
  CAMLreturn(Val_int(pid));
}

#else  // Unsupported platforms, including Windows: fail loudly.

static value pty_unsupported(const char *cmd) {
  errno = ENOSYS;
  uerror(cmd, caml_copy_string("PTYs are not supported on this platform"));
  return Val_unit;  // Unreachable
}

CAMLprim value ocaml_pty_open(value unit) {
  (void)unit;
  return pty_unsupported("open_pty");
}
CAMLprim value ocaml_pty_get_winsize(value fd) {
  (void)fd;
  return pty_unsupported("get_winsize");
}
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  (void)fd;
  (void)ws_val;
  return pty_unsupported("set_winsize");
}
CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}
CAMLprim value ocaml_pty_fork_exec(value slave, value paths, value argv,
                                   value env, value cwd) {
  (void)slave;
  (void)paths;
  (void)argv;
  (void)env;
  (void)cwd;
  return pty_unsupported("spawn");
}

#endif  // MATRIX_HAS_POSIX_PTY

// Whether spawn holds the slave in the parent until close, so that the
// child's exit is not the slave's last close. Two paths of the kernel decide.
//
// - The slave's last close waits a bounded time for the master to read the
//   output, then discards what is left: 0.6 s on macOS (ptsclose sets
//   t_timeout to 60 ticks), 5 s on OpenBSD and NetBSD (ttywflush).
// - A session leader's exit, while the slave is still open elsewhere, waits
//   without bound for its terminal's output to be read (ttywait with no
//   timeout), then revokes the terminal, which ends the master's output.
//
// Holding the slave puts the child's exit on the second path, so its output
// waits for the read. OpenBSD and NetBSD are held on the reading of their
// sources, untested.
//
// The others gain nothing from holding it. Linux keeps the unread output
// after the last close, and does not hang up a PTY when its session leader
// exits, so a held slave would hide the end of output. FreeBSD's exit revokes
// the terminal without waiting, flushing the output at once, where its last
// close waits about 1 s. DragonFly's exit waits 1 s, no longer than its last
// close waits.
CAMLprim value ocaml_pty_hold_slave(value unit) {
  (void)unit;
#if defined(__APPLE__) || defined(__OpenBSD__) || defined(__NetBSD__)
  return Val_true;
#else
  return Val_false;
#endif
}
