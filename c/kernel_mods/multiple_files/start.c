#include <linux/module.h>
#include <linux/kernel.h>

MODULE_LICENSE("GPL");

int init_module(void) {
	printk(KERN_INFO "Hello, World - This is the Kernel speaking.\n");
	return 0;
}
