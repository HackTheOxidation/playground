#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

pthread_mutex_t mtx;
pthread_cond_t cond;
unsigned int entry = 0;

void *routine(void *args) {
    pthread_mutex_lock(&mtx);
    while (entry > 0)
        pthread_cond_wait(&cond, &mtx);
    ++entry;

    printf("Entered the critical section.\n");

    --entry;
    pthread_mutex_unlock(&mtx);
    
    pthread_exit(NULL);
}

int main() {
    pthread_t p1, p2;

    pthread_mutex_init(&mtx, NULL);
    pthread_cond_init(&cond, NULL);
    entry = 0;

    pthread_create(&p1, NULL, routine, NULL);
    pthread_create(&p2, NULL, routine, NULL);

    pthread_join(p1, NULL);
    pthread_join(p2, NULL);

    pthread_mutex_destroy(&mtx);
    pthread_cond_destroy(&cond);
    
    return 0;
}
