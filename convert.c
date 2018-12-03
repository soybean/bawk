#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* This function converts a string to an int */
/*int string_to_int(char *a) {
  char *cleaned = malloc(strlen(a) + 1);
  strcpy(cleaned, a);
  cleaned++;
  cleaned[strlen(cleaned)-1] = 0;
  return atoi(cleaned);
}*/

int string_to_int(char *a) {
  return atoi(a);
}

char* int_to_string(int a) {
  int length = snprintf( NULL, 0, "%d", a);
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", a );
  return str;
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
