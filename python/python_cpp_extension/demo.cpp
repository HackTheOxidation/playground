#define PY_SSIZE_CLEAN
#include <Python.h>

static PyObject* DemoError;

static PyObject* demo_system(PyObject* self, PyObject* args) {
  const char* command;
  int exit_code;

  if (!PyArg_ParseTuple(args, "s", &command))
    return nullptr;

  exit_code = system(command);
  return PyLong_FromLong(exit_code);
}

static PyMethodDef DemoMethods[] = {
  {"system", demo_system, METH_VARARGS,
   "Execute a shell command."},

  {NULL, NULL, 0, NULL} /* Sentinel */
};

static struct PyModuleDef demo_module = {
  PyModuleDef_HEAD_INIT,
  "demo",
  NULL,
  -1,
  DemoMethods
};

PyMODINIT_FUNC PyInit_demo(void) {
  PyObject* module = PyModule_Create(&demo_module);
  if (module == nullptr)
    return nullptr;

  DemoError = PyErr_NewException("demo.error", NULL, NULL);
  Py_XINCREF(DemoError);

  if (PyModule_AddObject(module, "error", DemoError) < 0) {
    Py_XDECREF(DemoError);
    Py_CLEAR(DemoError);
    Py_DECREF(DemoError);
    return nullptr;
  }

  return module;
}
