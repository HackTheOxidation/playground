#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>

sem_t semEntryEmpty;
sem_t semEntryFull;
sem_t semExitEmpty;
sem_t semExitFull;

pthread_mutex_t mtx_entryEmpty;
pthread_cond_t cond_entryEmpty;
unsigned int entryEmpty;

pthread_mutex_t mtx_entryFull;
pthread_cond_t cond_entryFull;
unsigned int entryFull;

pthread_mutex_t mtx_exitEmpty;
pthread_cond_t cond_exitEmpty;
unsigned int exitEmpty;

pthread_mutex_t mtx_exitFull;
pthread_cond_t cond_exitFull;
unsigned int exitFull;

void *entry_park(void *args) {
  while (1) {
    sem_wait(&semEntryFull);
    /*pthread_mutex_lock(&mtx_entryFull);
    while (entryFull > 0)
      pthread_cond_wait(&cond_entryFull, &mtx_entryFull);
    --entryFull;
    pthread_mutex_unlock(&mtx_entryFull);
    */

    printf("The entry gate is open.\n");

    sem_post(&semEntryEmpty);
    /*pthread_mutex_lock(&mtx_entryEmpty);
    ++entryEmpty;
    pthread_mutex_unlock(&mtx_entryEmpty);
    pthread_cond_signal(&cond_entryEmpty);
    */

    printf("The entry gate is closed.\n");
  }

  pthread_exit(NULL);
}

void *exit_park(void *args) {
  while (1) {
    sem_wait(&semExitFull);
    /*pthread_mutex_lock(&mtx_exitFull);
    while (exitFull > 0)
      pthread_cond_wait(&cond_exitFull, &mtx_exitFull);
    --exitFull;
    pthread_mutex_unlock(&mtx_exitFull);
    */

    printf("The exit gate is open.\n");

    sem_post(&semExitEmpty);
    /*    pthread_mutex_lock(&mtx_exitEmpty);
    ++exitEmpty;
    pthread_mutex_unlock(&mtx_exitEmpty);
    pthread_cond_signal(&cond_exitEmpty);
    */
    printf("The exit gate is closed.\n");
  }

  pthread_exit(NULL);
}

void *car_cycle(void *args) {
  int id = *((int *) args);
  
  while (1) {
    sem_wait(&semEntryEmpty);
    /*pthread_mutex_lock(&mtx_entryEmpty);
    while (entryEmpty < 1)
      pthread_cond_wait(&cond_entryEmpty, &mtx_entryEmpty);
    --entryEmpty;
    pthread_mutex_unlock(&mtx_entryEmpty);
    */

    sem_post(&semEntryFull);
    /*pthread_mutex_lock(&mtx_entryFull);
    ++entryFull;
    pthread_mutex_unlock(&mtx_entryFull);
    pthread_cond_signal(&cond_entryFull);
    */

    printf("The car %d entered the parking lot.\n", id);

    sleep(1);

    sem_wait(&semExitEmpty);
    /*pthread_mutex_lock(&mtx_exitEmpty);
    while (exitEmpty < 1)
      pthread_cond_wait(&cond_exitEmpty, &mtx_exitEmpty);
    --exitEmpty;
    pthread_mutex_unlock(&mtx_exitEmpty);
    */

    sem_post(&semExitFull);
    /*pthread_mutex_lock(&mtx_exitFull);
    ++exitFull;
    pthread_mutex_unlock(&mtx_exitFull);
    pthread_cond_signal(&cond_exitFull);
    */

    printf("The car %d left the parking lot.\n", id);

    sleep(1);
  }

  pthread_exit(NULL);
}

int main() {
  pthread_t cars[3];
  pthread_t entry_guard, exit_guard;
  int ids[3] = {0, 1, 2};

  sem_init(&semEntryEmpty, 0, 1);
  sem_init(&semEntryFull, 0, 0);
  sem_init(&semExitEmpty, 0, 1);
  sem_init(&semExitFull, 0, 0);

  /*
  pthread_mutex_init(&mtx_entryEmpty, NULL);
  pthread_cond_init(&cond_entryEmpty, NULL);
  entryEmpty = 1;
  
  pthread_mutex_init(&mtx_entryFull, NULL);
  pthread_cond_init(&cond_entryFull, NULL);
  entryFull = 0;

  pthread_mutex_init(&mtx_exitEmpty, NULL);
  pthread_cond_init(&cond_exitEmpty, NULL);
  exitEmpty = 1;

  pthread_mutex_init(&mtx_exitFull, NULL);
  pthread_cond_init(&cond_exitFull, NULL);
  exitFull = 0;
  */
  
  int s;
  s = pthread_create(&entry_guard, NULL, entry_park, NULL);
  s = pthread_create(&exit_guard, NULL, exit_park, NULL);

  for (int i = 0; i < 3; i++) {
    s = pthread_create(&cars[i], NULL, car_cycle, &ids[i]);
  }
  
  for (int i = 0; i < 3; i++) {
    s = pthread_join(cars[i], NULL);
  }

  sem_destroy(&semEntryEmpty);
  sem_destroy(&semEntryFull);
  sem_destroy(&semExitEmpty);
  sem_destroy(&semExitFull);

  /*
  pthread_mutex_destroy(&mtx_entryEmpty);
  pthread_mutex_destroy(&mtx_entryFull);
  pthread_mutex_destroy(&mtx_exitEmpty);
  pthread_mutex_destroy(&mtx_exitFull);

  pthread_cond_destroy(&cond_entryEmpty);
  pthread_cond_destroy(&cond_entryFull);
  pthread_cond_destroy(&cond_exitEmpty);
  pthread_cond_destroy(&cond_exitFull);
  */

  return 0;
}
