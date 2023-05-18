/*
** This file is only an example of functions for File I/O and doesn't compile
 */

#include <stdio.h>

int main(int argc, char *argv[]) {

    FILE *cfPtr;                                                                     // FILE pointer

    char *filename;

    char *string;

    if ((cfPtr = fopen(filename, "w")) == NULL) {                                     // Checks if the file can be opened
        puts("The file %s could not be opened");
    } else {


        while (!feof(cfPtr) ) {                                                       // Checks for end of file
            fgetc(cfPtr);                                                             // Reads one char from the file
            fgets(cfPtr);                                                             // Reads one line from the file
            fprintf(cfPtr, "Write somethind to the file");                            // Write to file
            fwrite("This", sizeof(int), 1, cfPtr);                                    // Write to file with maximum sizeof(datatype)
            fread(string, sizeof(int), 1, cfPtr);                                     // Read sizeof(datatype) bytes from file
            fscanf(cfPtr, "%30s", string);                                            // Read from file
        }

        rewind(cfPtr);                                                                // Returns to the beginning of the file
        fseek(cfPtr, 3);                                                              // Seek position in file
        fclose(cfPtr);                                                                // Closes the file
    }

    return 0;
}
