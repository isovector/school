/**
  CS 240 A5 starter files

  2012 Edward Lee and Brad Lushman */

#ifndef __BITBUFFER_H__
#define __BITBUFFER_H__

#include <stdint.h>
#include <climits>
#include <cstdio>
#include <cstdlib>

using namespace std;

class BitBuffer {
private:
  uint8_t* buffer;
  size_t length;
  size_t written;
  size_t read;
  uint8_t last;
  int8_t offset;
public:
  BitBuffer();
  ~BitBuffer();
public:
  void addBit(uint8_t);
  bool byteReady();
  bool isEmpty();
  uint8_t getByte();
private:
  void resizeBuffer();
};
 

#endif 
