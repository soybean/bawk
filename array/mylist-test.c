#include <stdio.h>
#include "mylist.h"

// function given to print ints
static void printInt(void *p)
{	
	printf("%d ", *(int *)p);
}

// function given to compare ints
int compare (const void *a, const void *b)
{
    if (*(int *)a < *(int *)b) return -1;
    if (*(int *)a > *(int *)b) return 1;
    return 0;
}

int main()
{
	int arr[] = {10, 2, 3, 7, 50};
	struct List list;

	//empty array
	initList(&list);

	// array literals
	for(int i = 0; i < sizeof(arr) / sizeof(int); i++) {
 		addFront(&list, &arr[i]);
  }     
  reverseList(&list);

  // length
  printf("Length of list: %d\n", length(&list));

  // print
	printf("Print contents of list: ");
  traverseList(&list, &printInt);
  printf("\n");

  // access
  int a = 1;
  printf("Find Node at index 1: ");
  struct Node *node_by_index = findByIndex(&list, &a);
  printf("%d\n", *(int *)node_by_index->data);

  // index_of
  int b = 2;
	printf("Element 2 is at index: %d\n", findIndexOfNode(&list, &b, (int (*)(const void *, const void *))compare));
  
	// contains 
  int c = 7;
  printf("Does list contain element 7: ");
  struct Node *node = findNode(&list, &c, (int (*)(const void *, const void *))compare);
  if(node)
  	printf("YES\n");
  else
		printf("NO\n");

	// contains
  int c1 = -7;
 	printf("Does list contain element -7: ");
 	struct Node *node1 = findNode(&list, &c1, (int (*)(const void *, const void *))compare);
 	if(node1)
 		printf("YES\n");
 	else
 		printf("NO\n");

	// insert
  int d = 8;
  printf("Insert element 8 after element 7: ");
  addAfter(&list, node, &d);
  traverseList(&list, &printInt);
  printf("\n");

	// assign
  int e = 20;
  printf("Set element 7 to be 20 instead: ");
  node->data = &e;
  traverseList(&list, &printInt);
  printf("\n");

	// delete
  int f = 2;
	printf("Remove element at index 2: ");
	removeNode(&list, &f);
  traverseList(&list, &printInt);
  printf("\n");

	removeAllNodes(&list);
	return 0;
}
