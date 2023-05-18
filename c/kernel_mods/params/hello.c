#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/stat.h>

// Module Information
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Tomas Hagenau Andersen");
MODULE_DESCRIPTION("A simple Hello world LKM with module parameters!");
MODULE_VERSION("0.1");

// Declare global, static variables
static short int myshort = 1;
static int myint = 420;
static long int mylong = 9999;
static char *mystring = "blah";
static int myintArray[2] = { -1, -1 };
static int arr_argc = 0;


// Declare static variables as module parameters
module_param(myshort, short, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
MODULE_PARM_DESC(myshort, "A short integer");
module_param(myint, int, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
MODULE_PARM_DESC(myint, "An integer");
module_param(mylong, long, S_IRUSR);
MODULE_PARM_DESC(mylong, "A long integer");
module_param(mystring, charp, 0000);
MODULE_PARM_DESC(mystring, "A character string");

// Declare module parameter array
module_param_array(myintArray, int, &arr_argc, 0000);
MODULE_PARM_DESC(myintArray, "An array of integers");

static int __init hello_start(void) {
	int i;
	printk(KERN_INFO "Hello World\n");

	printk(KERN_INFO "myshort: %hd\n", myshort);
	printk(KERN_INFO "myint: %d\n", myint);
	printk(KERN_INFO "mylong: %ld\n", mylong);
	printk(KERN_INFO "mystring: %s\n", mystring);

	for (i = 0; i < (sizeof myintArray / sizeof (int)); i++) {
		printk(KERN_INFO "myintArray[%d] = %d\n", i, myintArray[i]);
	}

	printk(KERN_INFO "got %d arguments for myintArray.\n", arr_argc);

	return 0;
}

static void __exit hello_end(void) {
	printk(KERN_INFO "Removing hello module...\n");
}

// Set init and exit procedures.
module_init(hello_start);
module_exit(hello_end);
