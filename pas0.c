/* pas0.c */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void pmain();

static int save_argc;
static char **save_argv;
static FILE *infile;

int main(int argc, char **argv) {
     save_argc = argc;
     save_argv = argv;
     infile = stdin;
     pmain();
     return 0;
}

int argc(void) {
     return save_argc;
}

void argv(int i, char *s, int n) {
     /* The result is null-terminated even if it has to be truncated */
     strncpy(s, save_argv[i], n-1);
     s[n-1] = '\0';
}

void print_string(char *s, int n) {
     /* Print no more than n characters, but only up to the first null. */
     printf("%.*s", n, s);
}

void print_num(int n) {
     printf("%d", n);
}

void print_char(int c) {
     printf("%c", c);
}

void newline(void) {
     printf("\n");
}

void read_char(char *c) {
     int c0 = fgetc(infile);
     *c = (c0 == EOF ? 127 : c0);
}

int open_in(char *s, int n) {
     /* Assume s is null-terminated */
     FILE *f = fopen(s, "r");
     if (f == NULL) return 0;
     if (infile != stdin) fclose(infile);
     infile = f;
     return 1;
}

void close_in(void) {
     if (infile == stdin) return;
     fclose(infile);
     infile = stdin;
}

void check(int n) {
     fprintf(stderr, "Array bound error on line %d\n", n);
     exit(2);
}

void slicecheck(int n) {
     fprintf(stderr, "Slice error on line %d\n", n);
     exit(2);
}

void nullcheck(int n) {
     fprintf(stderr, "Null pointer check on line %d\n", n);
     exit(2);
}

void *palloc(int n) {
     char *q = malloc(n);
     if (q == NULL) {
          fprintf(stderr, "Out of memory space\n");
          exit(2);
     }
     return q;
}

void *palloc2(int n, int s) {
     unsigned *q;
     if (n < 0) {
          fprintf(stderr, "Allocating array with negative bound\n");
          exit(2);
     }
          
     q = palloc(n*s+4);
     q[0] = n;
     return &q[1];
}

int int_div(int a, int b) {
     int quo = a / b, rem = a % b;
     if (rem != 0 && (rem ^ b) < 0) quo--;
     return quo;
}

int int_mod(int a, int b) {
     int rem = a % b;
     if (rem != 0 && (rem ^ b) < 0) rem += b;
     return rem;
}

