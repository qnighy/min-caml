#include <cstdio>
#include <cstdint>

#define BE

union uu {
  uint32_t u32;
  int i;
  float f;
};

float read_float() {
  float ret;
  scanf("%f", &ret);
  // fprintf(stderr, "%f\n", ret);
#ifdef BE
  uu u;
  u.f = ret;
  char bytes[4] = {
    (char)(u.u32>>24),
    (char)(u.u32>>16),
    (char)(u.u32>>8),
    (char)(u.u32>>0)};
  fwrite(bytes, sizeof(char), 4, stdout);
#else
  fwrite(&ret, sizeof(float), 1, stdout);
#endif
  return ret;
}
int read_int() {
  int ret;
  scanf("%d", &ret);
  // fprintf(stderr, "%d\n", ret);
#ifdef BE
  uu u;
  u.i = ret;
  char bytes[4] = {
    (char)(u.u32>>24),
    (char)(u.u32>>16),
    (char)(u.u32>>8),
    (char)(u.u32>>0)};
  fwrite(bytes, sizeof(char), 4, stdout);
#else
  fwrite(&ret, sizeof(int), 1, stdout);
#endif
  return ret;
}

int main() {
  read_float();
  read_float();
  read_float();
  read_float();
  read_float();
  read_int();
  read_float();
  read_float();
  read_float();
  for(int i = 0; i < 60; ++i) {
    int texture = read_int();
    if(texture == -1) break;
    read_int();
    read_int();
    int isrot_p = read_int();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    read_float();
    if(isrot_p) {
      read_float();
      read_float();
      read_float();
    }
  }
  for(;;) {
    int len = 0;
    for(;;) {
      int item = read_int();
      if(item == -1) break;
      len++;
    }
    if(len == 0) break;
  }
  for(;;) {
    int len = 0;
    for(;;) {
      int item = read_int();
      if(item == -1) break;
      len++;
    }
    if(len == 0) break;
  }
  return 0;
}
