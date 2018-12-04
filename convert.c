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

char *concat(char *a, char *b) {
  char *str = malloc(strlen(a) + strlen(b));
  size_t len1 = strlen(a), len2 = strlen(b);
  char *concat = (char*) malloc(len1 + len2 + 1);

  memcpy(concat, a, len1);
  memcpy(concat+len1, b, len2+1);
  return concat;
}

int string_to_int(char *a) {
  return atoi(a);
}

char *bool_to_string(int a) {
  if(a) {
    return "True";
  }
  else {
    return "False";
  }
  /*printf("%c", a);*/
  return "";
  /*if (strcmp(a,"true") == 0){
    return "True";
  }
  else {
    return "False";
  }
  */
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
