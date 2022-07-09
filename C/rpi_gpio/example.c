#include <linux/i2c-dev.h>
#include <i2c/smbus.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>

int main() {
	FILE* file;
	int adapter_nr = 2;
	char filename[20];

	snprintf(filename, 19, "/dev/i2c-%d", adapter_nr);
	file = popen(filename, I2C_RDWR);
	if (file < 0) {
		exit(1);
	}

	int addr = 0x40;

	if (ioctl(file, I2C_SLAVE, addr) < 0) {
		exit(1);
	}

	pclose(file);
}
