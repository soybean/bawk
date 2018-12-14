#include <stdio.h>
#include <string.h>

void loop(char *line);

void end();

int main(int argc, char **argv) {

  // Filename not specified
  if (argc < 2) {
    printf("usage: ./bawk.sh [bawk file] [input file]\n");
  }
  else {
    char *filename = argv[1];
    FILE *fp = fopen(filename, "rw");
    char buffer[256];
		char *rs = "\n";
    while (fgets(buffer, 256, fp)) {
			buffer[strcspn(buffer, rs)] = '\0';
      loop(buffer);
    }

    end();
  }
}
