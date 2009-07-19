#include "CSHADigest.h"

#include <iostream>
using namespace std;

#include <string.h>

int main(int argc, char* argv[]) {
  CSHADigest SHADigest;

  char test[] = "test";

  
  for (int i = 0; i < 1000000; i++) {
    SHADigest.Start();
    SHADigest.ComputeInputBuffer((uByte*)test, strlen(test));
    SHADigest.GetByteResult()
    SHADigest.Stop();
  }

}
