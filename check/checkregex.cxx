#ifdef BOOSTPOSIXREGEX
#include <boost/regex.hpp>
using boost::wregex;
using boost::wsregex_iterator;
#else
#include <regex>
using std::wregex;
using std::wsregex_iterator;
#endif

int main(int argc, char *argv[]) {
    wregex rg(L"test");
    const wsregex_iterator end;
}
