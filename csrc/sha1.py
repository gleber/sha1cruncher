#!/usr/bin/env python 

from __future__ import generators

chars = range(ord('a'), ord('z')) + range(ord('A'), ord('Z'))

def rand5char():
    for a in chars:
        for b in chars:
            for c in chars:
                for d in chars:
                    for e in chars:
                        yield chr(a) + chr(b) + chr(c) + chr(d) + chr(e)

import hashlib
from hashlib import sha1
import random
import sys

class hamming(object):
    def __init__(self):
        self.bitcount = []
        for x in range(256):
            self.bitcount.append(self.bits(x))

    def bits(self, n):
        x = 0
        while n > 0:
            x = x + (n & 1)
            n = n >> 1
        return x
    
    
    def distance(self, d1, d2):
        assert len(d1) == len(d2)
        return sum([self.bitcount[ord(x)^ord(y)] for x,y in zip(d1, d2)])

h = hamming()

import wordlist

size = len(wordlist.words)

target = wordlist.target.decode('hex')

def word_set():
    return [ random.randint(0,size-1) for i in xrange(0,12) ]

best_distance = 160

for i in xrange(1, 100):
    wordset = word_set()

    print " ".join([wordlist.words[c] for c in wordset])

    shaOrig = sha1()
    for c in wordset:
        shaOrig.update(wordlist.words[c] + " ")

    for x in rand5char():
        sha = shaOrig.copy()
        sha.update(x)
        d = h.distance(target, sha.digest())
        if d < best_distance:
            best_distance = d
            print d, " ".join([wordlist.words[c] for c in wordset]) + " " + x
    
