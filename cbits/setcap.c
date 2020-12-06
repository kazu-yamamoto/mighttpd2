#if defined(__linux__)
#include <linux/securebits.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#include "Rts.h"

void set_capabilities (uint32_t cap) {
  cap_user_header_t header = malloc(sizeof(*header));
  header->version = _LINUX_CAPABILITY_VERSION_3;
  header->pid = 0;

  cap_user_data_t data = malloc(sizeof(*data));
  data->effective   = cap;
  data->permitted   = cap;
  data->inheritable = 0;

  capset(header,data);

  free(header);
  free(data);
}

void handler(int signum) {
  uint32_t cap = 1 << CAP_NET_BIND_SERVICE;
  set_capabilities (cap);
}

void FlagDefaultsHook () {
  if (geteuid() == 0) {
    prctl(PR_SET_SECUREBITS, SECBIT_KEEP_CAPS, 0L, 0L, 0L);

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = handler;
    sa.sa_flags = SA_RESTART;
    sigaction(SIGUSR1, &sa, NULL);
  }
}

void send_signal (int tid, int sig) {
  int tgid = getpid();
  syscall(SYS_tgkill, tgid, tid, sig);
}
#else
void send_signal (int tid, int sig) {}
#endif
