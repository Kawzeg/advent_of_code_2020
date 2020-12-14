/*
  main.c solves Advent of Code 2020/14
*/

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>

struct kv {
  uint64_t k;
  uint64_t v;
};

/**
 * Returns the masks written to mems
 */
uint64_t parseMask(char* mask, uint64_t index, uint64_t* indices) {
  uint64_t orMask = 0;
  for (int i = 7; i < 7+36; ++i) {
    orMask <<= 1;
    if (mask[i] == '1') {
      ++orMask;
    }
  }
  index |= orMask;
  uint64_t num_indices = 1;
  indices[0] = index;
  for (int i = 7; i < 7+36; ++i) {
    if (mask[i] == 'X') {
      for (int j = 0; j < num_indices; ++j) {
        // 1 << (42-i) is not long enough because 1 is not 64 bit
        uint64_t y = 1;
        y <<= 42-i;
        uint64_t x =  indices[j] ^ y;
        indices[num_indices + j] = x;
      }
      num_indices <<= 1; // Double num_indices
    }
  }
  return num_indices;
}

/**
 * Given a line "mask = XXX..."
 */
uint64_t getMask(char* mask, char c) {
  uint64_t r = c == '0';
  for (int i = 7; i < 7+36; ++i) {
    r <<= 1;
    r += c == '0';
    if (mask[i] == c) {
      if (c == '0') { // And mask needs some zeroes
        --r;
      } else { // Or mask needs some ones
        ++r;
      }
    }
  }
  return r;
}

void updateKv(struct kv* mem, uint64_t k, uint64_t v) {
  size_t i = 0;
  while (mem[i].k != 0) { // O(n) Update baybee
    if (mem[i].k == k) {
      mem[i].v = v;
      return;
    }
    ++i;
  }
  mem[i].k = k;
  mem[i].v = v;
}

void update(char* line, char* mask, struct kv* sparseMem, size_t read) {
  if (line[1] == 'a') {
    strcpy(mask, line);
  } else {
    uint64_t indices[1<<17];
    char* eoi;
    uint64_t i = strtol(&line[4], &eoi, 10);
    uint64_t x = strtol(&eoi[4], &eoi, 10);
    uint64_t num_indices = parseMask(mask, i, indices);
    for (size_t i = 0; i < num_indices; ++i) {
      updateKv(sparseMem, indices[i], x);
    }
  }
}

void updateMem(char* line, size_t read, uint64_t* mem, uint64_t* and, uint64_t* or) {
  if (line[1] == 'a') { // mask = ...
    *and = getMask(line, '0');
    *or = getMask(line, '1');
  } else { // mem[abc] = xyz
    char* eoi;
    size_t i = strtol(&line[4], &eoi, 10);
    uint64_t x = strtol(&eoi[4], &eoi, 10);
    x &= *and;
    x |= *or;
    mem[i] = x;
  }
}

int main()
{
  static uint64_t mem[1<<16];

  // Modifying a char* x = "abc" is UB!
  // See https://cs50.stackexchange.com/questions/8899/difference-between-char-and-char-in-c
  static char mask[] = "mask = 000000000000000000000000000000000000\n";
  uint64_t andMask = getMask(mask, '0');
  uint64_t orMask = getMask(mask, '1');
  printf("andMask=0x%.16" PRIX64 "\n", andMask);
  printf("orMask =0x%.16" PRIX64 "\n", orMask);

  FILE * fp;
  char * line = NULL;
  size_t len = 0;
  ssize_t read;

  fp = fopen("input", "r");
  if (fp == NULL)
    return 1;

  while ((read = getline(&line, &len, fp)) != -1) {
    updateMem(line, read, mem, &andMask, &orMask);
  }

  fclose(fp);
  if (line)
    free(line);

  uint64_t sum = 0;
  for (size_t i = 0; i < 1<<16; ++i) {
    sum += mem[i];
  }
  printf("Part 1 answer is: %lu\n", sum);
  memset(mem, 0, sizeof mem);

  fp = fopen("input", "r");
  if (fp == NULL)
    return 1;

  struct kv sparseMem[1<<17];

  while ((read = getline(&line, &len, fp)) != -1) {
    update(line, mask, sparseMem, read);
  }

  sum = 0;
  size_t i = 0;
  uint64_t v;
  do  {
    v = sparseMem[i].v;
    sum += v;
    ++i;
  } while (v != 0);
  printf("Part 2 answer is: %lu\n", sum);

  fclose(fp);
  if (line)
    free(line);
  return 0;
}
