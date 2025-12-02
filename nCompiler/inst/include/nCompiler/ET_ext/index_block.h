#ifndef INDEX_BLOCK_H_
#define INDEX_BLOCK_H_

#include <utility>

class b__ {
public:
  typedef std::pair<long, long> StartStop;
  StartStop startStop;// would be nice to have this const, but only if we never need to set()
  bool isEmpty_;
  bool isSingle_;
  b__(long start, long stop) : startStop(start, stop), isEmpty_(false), isSingle_(false) {}
  b__(long single) : startStop(single, 0), isEmpty_(false), isSingle_(true) {}
  b__() : startStop(0, 0), isEmpty_(true), isSingle_(false) {}
  void set(long start, long stop) {startStop.first = start; startStop.second = stop; isEmpty_=false; isSingle_=false;}
  void set(long start) {startStop.first = start; startStop.second = 0; isEmpty_ = false; isSingle_=true;}
  void set() {startStop.first = 0; startStop.second = 0; isEmpty_ = true; isSingle_=false;}
  const long extent() const {return startStop.second - startStop.first + 1;}
  const bool &isSingle() const {return isSingle_;}
  const bool &isEmpty() const {return isEmpty_;}
  const long &start() const {return startStop.first;}
  const long &stop() const {return startStop.second;}
  void update(long external_start, long external_stride) {
    if(isEmpty()) return;
    startStop.first = external_start + external_stride * startStop.first;
    if(isSingle()) return;
    startStop.second = external_start + external_stride * startStop.second;
  }
};


#endif // INDEX_BLOCK_H_
