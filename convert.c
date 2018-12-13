#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* This function converts a string to an int */
int string_to_int(char *a) {
  char *cleaned = malloc(strlen(a) + 1);
  strcpy(cleaned, a);
  return atoi(cleaned);
}

int streq(char *a, char *b) {
  return (strcmp(a, b) == 0);
}

int strneq(char *a, char *b) {
  return (strcmp(a, b) != 0);
}

int strgreater(char *a, char *b) {
  return (strcmp(a, b) > 0);
}

int strless(char *a, char *b) {
  return (strcmp(a, b) < 0);
}

int strgeq(char *a, char *b) {
  return (strcmp(a, b) >= 0);
}

int strleq(char *a, char *b) {
  return (strcmp(a, b) <= 0);
}

char *concat(char *a, char *b) {
  char *str = malloc(strlen(a) + strlen(b));
  size_t len1 = strlen(a), len2 = strlen(b);
  char *concat = (char*) malloc(len1 + len2 + 1);

  memcpy(concat, a, len1);
  memcpy(concat+len1, b, len2+1);
  return concat;
}

char *bool_to_string(int a) {
  if(a) {
    return "true";
  }
  else {
    return "false";
  }
  return "";
}

char* int_to_string(int a) {
  int length = snprintf( NULL, 0, "%d", a);
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", a );
  return str;
}

char *rgx_to_string(char *a) {
  if (a[0] == '\'') {
    a++;
  }
  if (a[strlen(a)-1] == '\'') {
    a[strlen(a)-1] = '\0';
  }
  return a;
}

int numfields(char *line) {
    if (strlen(line)==1 && line[0] == '\n') {
      return 0;
    }
    if (strlen(line)==0) {
      return 0;
    }
    int numSpaces = 1;

    for(int i = 0; i < strlen(line); i++) {
      if(line[i] ==' '){
        numSpaces++;
      }
    }
    return numSpaces;
}

char *access(char *line, int field) {
  if(field == 0){
    return line;
  }
  else {
    char *array[strlen(line)+1];

    for (int k = 0; k < strlen(line) + 1; k++) {
      array[k] = NULL;
    }

    int i=0;

    array[i] = strtok(line," ");

    while(array[i]!=NULL)
    {
      array[++i] = strtok(NULL," ");
    }

    field = field - 1;
    if (array[field] != NULL) {
      return array[field];
    }
    else {
      return "";
    }
  }
}
