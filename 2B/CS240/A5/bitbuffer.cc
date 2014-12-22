/*
  CS 240 A5 starter files

  2012 Edward Lee and Brad Lushman */

#include "bitbuffer.h"
#include <stdexcept>

using namespace std;

BitBuffer::BitBuffer() : buffer(NULL), length(0), last(0), offset(7), written(0), read(0)
{
}

BitBuffer::~BitBuffer()
{
  free(buffer);
}

void BitBuffer::resizeBuffer()
{
  buffer = static_cast<uint8_t*>(realloc(buffer, 2*(length + 10)));
  length = 2*(length + 10);
  if(! buffer)
    throw runtime_error("BitBuffer::resizeBuffer() - out of memory!");
}

void BitBuffer::addBit(uint8_t bit)
{
  last |= (bit & 1) << offset;
  offset --;
  /** Did we write a full byte? */
  if(offset < 0)
  {
    /** If we did, shift it into the buffer */
    offset = 7;   
    if(length == written)
      resizeBuffer();
    buffer[written++] = last;
    last = 0;
  }
}

bool BitBuffer::byteReady()
{
  return read < written;
}

bool BitBuffer::isEmpty()
{
  return read == written && offset == 7;
}

uint8_t BitBuffer::getByte()
{
  if(read < written)
    return buffer[read++];
  else if(read == written)
  {
    if(length == written)
      resizeBuffer();
    buffer[written++] = last;
    offset = 7; 
    last = 0;
    return buffer[read++];
  }
  else
    return 0;
} 
