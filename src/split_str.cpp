#include <cpp11.hpp>
#include <vector>
using namespace cpp11;

// borrowed from https://stackoverflow.com/a/25022977/1091766
[[cpp11::register]]
std::vector<std::string> split_str(const std::string& str, int splitLength) {
  int NumSubstrings = str.length() / splitLength;
  std::vector<std::string> ret;

  for (auto i = 0; i < NumSubstrings; i++) {
    ret.push_back(str.substr(i * splitLength, splitLength));
  }

  // If there are leftover characters, create a shorter item at the end.
  if (str.length() % splitLength != 0) {
    ret.push_back(str.substr(splitLength * NumSubstrings));
  }

  return ret;
}
