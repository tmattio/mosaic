# XTest driver: press chords in the xterm under the pointer; log times.
import ctypes, ctypes.util, sys, time
X = ctypes.CDLL(ctypes.util.find_library("X11") or "libX11.so.6")
T = ctypes.CDLL(sys.argv[1])
X.XOpenDisplay.restype = ctypes.c_void_p
X.XKeysymToKeycode.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
X.XStringToKeysym.restype = ctypes.c_ulong
X.XStringToKeysym.argtypes = [ctypes.c_char_p]
X.XFlush.argtypes = [ctypes.c_void_p]
T.XTestFakeKeyEvent.argtypes = [ctypes.c_void_p, ctypes.c_uint, ctypes.c_int, ctypes.c_ulong]
T.XTestFakeMotionEvent.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_ulong]
d = X.XOpenDisplay(None)
T.XTestFakeMotionEvent(d, -1, 100, 100, 0); X.XFlush(d); time.sleep(0.2)
def kc(name): 
    k = X.XKeysymToKeycode(d, X.XStringToKeysym(name.encode()))
    if not k: raise SystemExit("no keycode for " + name)
    return k
def key(name, down): T.XTestFakeKeyEvent(d, kc(name), 1 if down else 0, 0); X.XFlush(d)
log = open(sys.argv[2], "w")
for line in open(sys.argv[3]):
    chord = line.strip()
    if not chord: continue
    parts = chord.split("+")
    log.write("%.3f\t%s\n" % (time.time(), chord)); log.flush()
    for p in parts: key(p, True); time.sleep(0.01)
    for p in reversed(parts): key(p, False); time.sleep(0.01)
    time.sleep(0.25)
