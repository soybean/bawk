/* Authors: Ashley, Christine, Melanie */
/* Built-in conversion functions, string operations, NF, $ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char *FS;

/* Convert a string to an int */
int string_to_int(char *a) {
  char *cleaned = malloc(strlen(a) + 1);
  strcpy(cleaned, a);
  return atoi(cleaned);
}

/* string a == string b */
int streq(char *a, char *b) {
  return (strcmp(a, b) == 0);
}

/* string a != string b */
int strneq(char *a, char *b) {
  return (strcmp(a, b) != 0);
}

/* string a > string b */
int strgreater(char *a, char *b) {
  return (strcmp(a, b) > 0);
}

/* string a < string b */
int strless(char *a, char *b) {
  return (strcmp(a, b) < 0);
}

/* string a >= string b */
int strgeq(char *a, char *b) {
  return (strcmp(a, b) >= 0);
}

/* string a <= string b */
int strleq(char *a, char *b) {
  return (strcmp(a, b) <= 0);
}

/* Concatenate string b to string a and then return the new string */
char *concat(char *a, char *b) {
  char *str = malloc(strlen(a) + strlen(b));
  size_t len1 = strlen(a), len2 = strlen(b);
  char *concat = (char*) malloc(len1 + len2 + 1);

  memcpy(concat, a, len1);
  memcpy(concat+len1, b, len2+1);
  return concat;
}

/* Convert a bool to a string */
char *bool_to_string(int a) {
  if(a) {
    return "true";
  }
  else {
    return "false";
  }
  return "";
}

/* Convert an int to a string */
char *int_to_string(int a) {
  int length = snprintf( NULL, 0, "%d", a);
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", a );
  return str;
}

/* Convert a regex to a string */
char *rgx_to_string(char *a) {
  if (a[0] == '\'') {
    a++;
  }
  if (a[strlen(a)-1] == '\'') {
    a[strlen(a)-1] = '\0';
  }
  return a;
}

/* Return the number of fields in a line */
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

/* Return whatever is at the specified field */
char *access(char *line, int field) {
   if (field == 0) {
     return line;
   }
   char *copy = malloc(strlen(line) + 1);
   strcpy(copy, line);
   char *token;
   int count = 1;

   token = strtok(copy, FS);
   
   /* walk through other tokens */
   while( token != NULL ) {
      if (count == field) {
        return token;
      }
      token = strtok(NULL, FS);
      count++;
   }
   return "";
}
