#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

struct env {
  int x;
};

struct __attribute__((packed)) thunk {
  unsigned char push;
  struct env * env_addr;
  unsigned char call;
  signed long call_offset;
  unsigned char add_esp[3];
  unsigned char ret;
};

struct thunk default_thunk = {0x68, 0, 0xe8, 0, {0x83, 0xc4, 0x04}, 0xc3};

typedef void (* cfunc)();

struct thunk * make_thunk(struct env * env, void * code)
{
  struct thunk * thunk = (struct thunk *)mmap(0,sizeof(struct thunk), PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  *thunk = default_thunk;
  thunk->env_addr = env;
  thunk->call_offset = code - &thunk->add_esp[0]; // Pretty!
  mprotect(thunk,sizeof(struct thunk), PROT_EXEC);
  return thunk;
}


void block(struct env * env) {
  env->x += 1;
  printf ("block: x is %d\n", env->x);
}

cfunc foo (int x)
{
  struct env * env = (struct env *)malloc(sizeof(struct env));
  env->x = x;

  printf ("x is %d\n",env->x);

  return (cfunc)make_thunk(env,(void *)&block);
}

int main() {
  cfunc c = foo(5);
  
  c();
  c();
}
