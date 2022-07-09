#include <emscripten/bind.h>
using namespace emscripten;

float add(float x, float y) {
	return x + y;
}

EMSCRIPTEN_BINDINGS(my_module) {
	function("add", &add);
}
