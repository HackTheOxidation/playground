#include <CL/cl.h>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <ostream>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  std::cout << "Hello, world!\n";

  cl_platform_id platforms[64];
  unsigned int platformCount;
  cl_int platformResult = clGetPlatformIDs(64, platforms, &platformCount);

  assert(platformResult == CL_SUCCESS);

  cl_device_id device;
  for (int i = 0; i < platformCount; ++i) {
    cl_device_id devices[64];
    unsigned int deviceCount;
    cl_int deviceResult = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, 64, devices, &deviceCount);

    if (deviceResult == CL_SUCCESS) {
      for (int j = 0; j < deviceCount; ++j) {
	char vendorName[256];
	size_t vendorNameLength;
	cl_int deviceInfoResult = clGetDeviceInfo(devices[j], CL_DEVICE_VENDOR, 256, vendorName, &vendorNameLength);
	if (deviceInfoResult != CL_SUCCESS && std::string(vendorName).substr(0, vendorNameLength) == "NVIDIA CORPORATION") {
	  device = devices[j];
	  break;
	}
      }
    }
  }

  cl_int contextResult;
  cl_context context = clCreateContext(nullptr, 1, &device, nullptr, nullptr, &contextResult);
  assert(contextResult == CL_SUCCESS);

  cl_int commandQueueResult;
  cl_command_queue queue = clCreateCommandQueue(context, device, 0, &commandQueueResult);
  assert(commandQueueResult == CL_SUCCESS);

  const char* programSource = "";
  size_t length = 0;
  cl_int programResult;
  cl_program program = clCreateProgramWithSource(context, 1, &programSource, &length, &programResult);
  assert(programResult == CL_SUCCESS);

  cl_int programBuildResult = clBuildProgram(program, 1, &device, "", nullptr, nullptr);
  if (programBuildResult != CL_SUCCESS) {
    char log[256];
    size_t logLength;
    cl_int programBuildInfoResult = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 256, log, &logLength);
    assert(programBuildInfoResult == CL_SUCCESS);
    assert(std::string(log).substr(0, logLength) != "");
  }

  cl_int kernelResult;
  cl_kernel kernel = clCreateKernel(program, "vector_sum", &kernelResult);
  assert(kernelResult == CL_SUCCESS);

  cl_int vecaResult;
  cl_mem veca = clCreateBuffer(context, CL_MEM_READ_ONLY, 2 * sizeof(float), nullptr, &vecaResult);
  assert(vecaResult == CL_SUCCESS);

  float vecaData[] { 1.5f, 3.7f };
  cl_int enqueueVecaResult = clEnqueueWriteBuffer(queue, veca, CL_TRUE, 0, 2 * sizeof(float), vecaData, 0, nullptr, nullptr);
  assert(enqueueVecaResult == CL_SUCCESS);

  cl_int vecbResult;
  cl_mem vecb = clCreateBuffer(context, CL_MEM_READ_ONLY, 2 * sizeof(float), nullptr, &vecbResult);
  assert(vecbResult == CL_SUCCESS);

  float vecbData[] { 1.5f, 3.7f };
  cl_int enqueueVecbResult = clEnqueueWriteBuffer(queue, vecb, CL_TRUE, 0, 2 * sizeof(float), vecbData, 0, nullptr, nullptr);
  assert(enqueueVecbResult == CL_SUCCESS);

  cl_int veccResult;
  cl_mem vecc = clCreateBuffer(context, CL_MEM_READ_ONLY, 2 * sizeof(float), nullptr, &veccResult);
  assert(veccResult == CL_SUCCESS);

  cl_int kernelArgaResult = clSetKernelArg(kernel, 0, sizeof(cl_mem), &veca);
  assert(kernelArgaResult == CL_SUCCESS);
  cl_int kernelArgbResult = clSetKernelArg(kernel, 1, sizeof(cl_mem), &vecb);
  assert(kernelArgbResult == CL_SUCCESS);
  cl_int kernelArgcResult = clSetKernelArg(kernel, 2, sizeof(cl_mem), &vecc);
  assert(kernelArgcResult == CL_SUCCESS);

  size_t globalWorkSize = 2;
  size_t localWorkSize = 2;
  cl_int enqueueKernelResult = clEnqueueNDRangeKernel(queue, kernel, 1, 0, &globalWorkSize, &localWorkSize, 0, nullptr, nullptr);
  assert(enqueueKernelResult == CL_SUCCESS);

  float veccData[2];
  cl_int enqueueReadBufferResult = clEnqueueReadBuffer(queue, vecc, CL_TRUE, 0, 2 * sizeof(float), veccData, 0, nullptr, nullptr);
  assert(enqueueReadBufferResult == CL_SUCCESS);

  clFinish(queue);

  std::cout << "Result: ";
  for (int i = 0; i < 2; ++i) {
    std::cout << veccData[i] << ", ";
  }
  std::cout << std::endl;
  
  return 0;
}
