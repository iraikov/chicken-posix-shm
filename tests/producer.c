#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <unistd.h>
#include <fcntl.h>           /* For O_* constants */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define BUFFER_SIZE 32768
#define BUFFER_NAME "/shmtest1"

void buffer_create(int size, int *fileno, char **addr) 
{
  int perms = 0600;
  char *laddr;
  *addr = 0; 

  *fileno = shm_open(BUFFER_NAME, O_CREAT | O_RDWR, perms);
  if (errno > 0) 
    {
      perror("failed to create shared memory file handle");
      exit (1);
    }

  if ((ftruncate(*fileno, size)) == -1)
    {    /* Sets the size */
      perror("ftruncate failed on shared memory file handle");
      exit(1);
    }

  laddr = (char*)mmap(NULL, size, PROT_WRITE, MAP_SHARED, *fileno, 0);
  if (errno > 0) 
    {
      perror ("failed to mmap shared memory file handle");
      exit (1);
    }

  *addr = laddr;

}


void buffer_clear(char **sbuff, char *start) 
{
  *sbuff = start;
}

int buffer_size(char *sbuff, char *start) 
{
  return (sbuff-start);
}

void buffer_close(int fileno, char *start) 
{
  if ((munmap(start, BUFFER_SIZE)) == -1)
    {
      perror("munmap failed on shared buffer");
      exit(1);
    }
    
  if ((close(fileno)) == -1)
    {
      perror("close failed on shared buffer file handle");
      exit(1);
    }
    
}

void insert_item(char *item, char **shared_buffer, char *shared_start) 
{
  int n = strlen(item);
  memcpy (*shared_buffer, item, n);
  *shared_buffer = *shared_buffer+n;
}


int main(int argc, const char **argv)
{
  char *shared_start, *shared_buffer;
  int shared_fileno;

  buffer_create(BUFFER_SIZE, &shared_fileno, &shared_start);

  buffer_clear(&shared_buffer, shared_start); // prepare buffer for jobs

  int i = 0;
  char s[30];

  while(i<1000) 
    {
      memset(s,sizeof(s),0);
      sprintf (s, "Hello world #%d", i);
      insert_item(s, &shared_buffer, shared_start);
      printf ("shared_buffer = %p\n", shared_buffer);
      i = i+1;
    }

  buffer_close (shared_fileno, shared_start);
 
  return 0;
}
