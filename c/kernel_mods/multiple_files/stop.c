#include <linux/module.h>
#include <linux/kernel.h>

void cleanup_module() {
	printk(KERN_INFO "Removing startstop module...\n");
}
