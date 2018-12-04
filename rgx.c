#include <regex.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>


// this function returns true if a matches the rgx b, false otherwise
int equals(char *a, char *b) {
  if (b[0] == '\'') {
    b++;
  }
  if (b[strlen(b) - 1] == '\'') {
    b[strlen(b) - 1] = 0;
  }
  regex_t regex;
  int comp = regcomp(&regex, b, REG_EXTENDED|REG_NOSUB);
  if (comp) {
    printf("Regex expression could not be compiled");
    return 0;
  }
  comp = regexec(&regex, a, 0, NULL, 0);
  if (!comp) {
    return 1;
  }
  return 0;
}

int nequals(char *a, char *b) {
  int opp = equals(a, b);
  return !opp;
}
