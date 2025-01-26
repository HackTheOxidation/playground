#include <boost/lambda/lambda.hpp>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;


int main(int argc, char *argv[]) {
	using namespace boost::lambda;
	typedef std::istream_iterator<int> in;

	for_each(in(cin), in(), cout << (_1 * 3) << " " );

	return 0;
}
