#include <stdio.h>

void begin();

void loop(char *line);

void end();

int main(int argc, char **argv) {

  // Filename not specified
  if (argc < 2) {
    printf("You suck");
  }
  else {
    char *filename = argv[1];
    FILE *fp = fopen(filename, "rw");
    char buffer[256];
    while (fgets(buffer, 256, fp)) {
      loop(buffer);
    }
  }
}
