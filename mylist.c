/* 
 * Authors: Ashley, Christine
 * Arrays
 * Loosely based on Lab 3 from COMS 3157
 */

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* A node in a linked list */
struct Node {
	unsigned long data;
	struct Node *next;
};

/* A linked list. 'head' points to the first node in the list */
struct List {
	struct Node *head;
	size_t size_of_type;
	int depth;
};

/* Traverse list to find length of list */
int length(struct List *list) {
	struct Node *node = list->head;
	int count = 0;
	while(node) {
		node = node->next;
		count++;
	}
	return count;
}

/* Compare two bools. Return -1 if a < b, return 1 if a > b, and return 0 if a == b */
int compareBools(unsigned long a, unsigned long b)
{
	if ((bool)a < (bool)b) return -1;
	if ((bool)a > (bool)b) return 1;
	return 0;
}

/* Compare two ints. Return -1 if a < b, return 1 if a > b, and return 0 if a == b */
int compareInts(unsigned long a, unsigned long b)
{
	if ((int)a < (int)b) return -1;
	if ((int)a > (int)b) return 1;
	return 0;
}

/* Compare two strings. Return -1 if a < b, return 1 if a > b, and return 0 if a == b */
int compareStrs(unsigned long a, unsigned long b)
{
	return strcmp((const char *)a, (const char *)b);
}

/* Compare two lists. Return -1 if a < b, return 1 if a > b, and return 0 if a == b */
int compareLists(unsigned long a, unsigned long b, int (*compar)(unsigned long, unsigned long))
{
	struct List *lista = (struct List *)a;
	struct List *listb = (struct List *)b;
	if ( length(lista) != length(listb) )
		return -1;
	if ( lista->depth > 1 ) {
		struct Node *nodea = lista->head;
 		struct Node *nodeb = listb->head;
 		int total = 0;
		while( nodea ) {
 			total += abs(compareLists(nodea->data, nodeb->data, compar));
 			nodea = nodea->next;
			nodeb = nodeb->next;
		} 
		if ( total == 0 )
 			return 0;
		else
			return -1;
	}
	if ( lista->depth == 1) {
		struct Node *nodea = lista->head;
		struct Node *nodeb = listb->head;
		int total = 0;
		while( nodea ) {
 			total += abs(compar(nodea->data, nodeb->data));
			nodea = nodea->next;
			nodeb = nodeb->next;
 		}
		if ( total == 0 )
			return 0;
		else
			return -1;
	}
	return -1;
}

/* Initialize and return an empty list */
struct List *initList(size_t size_of_type, int depth)
{
	struct List *list = malloc( sizeof(struct List) );
	if(list == NULL){
		perror("malloc returned NULL");
		exit(1);
	}
	list->head = 0;
	list->size_of_type = size_of_type;
	list->depth = depth;
	return list;
}

/* Traverse the list until node at index found. NULL if not found */
struct Node *findByIndex(struct List *list, int indexSought) 
{
  int arr_len = length(list);
  
	struct Node *node = list->head;
	int indexAt = 0;
	while(node) {
		if( indexSought == indexAt )
			return node;
		node = node->next;
		indexAt++;
	}	
	return NULL;
}

/*
 * Traverse the list, comparing each data item with 'dataSought' using
 * 'compar' function.  ('compar' returns 0 if the data pointed to by
 * the two parameters are equal, non-zero value otherwise.)
 *
 * Returns the first node containing the matching data, 
 * NULL if not found.
 */
struct Node *findNode(struct List *list, unsigned long dataSought, int (*compar)(unsigned long, unsigned long))
{
	struct Node *node = list->head;
	if(list->depth == 1) {
		while(node) {
			if( compar(dataSought, node->data) == 0 )
				return node;
			node = node->next;
		}
	}
	if(list->depth > 1) {
		while(node) {
			if( compareLists(dataSought, node->data, compar) == 0 )
				return node;
			node = node->next;
		}
	}
	return NULL;    
}

/* Returns 1 if dataSought is found in list, and 0 otherwise */
int contains(struct List *list, unsigned long dataSought, int (*compar)(unsigned long, unsigned long)) {
	struct Node *found = findNode(list, dataSought, compar);
	if (found)
		return 1;
	return 0;
}

/* Traverse list to find index of node */
int findIndexOfNode(struct List *list, unsigned long dataSought, int (*compar)(unsigned long, unsigned long))
{ 
	struct Node *node = list->head;
	int count = 0;
	if(list->depth == 1) {
		while(node) {
			if( compar(dataSought, node->data) == 0 ) {
				return count;
			}
			node = node->next;
			count++;
		}
	}
	if(list->depth > 1) {
		while(node) {
			if( compareLists(dataSought, node->data, compar) == 0 ) {
				return count;
			}
			node = node->next;
			count++;
		}
	}
	return count;
}

/* 
 * Remove node at specific index and return the 'data' pointer that was stored in the node. 
 * Returns NULL if the list is empty 
 */
unsigned long removeNode(struct List *list, int indexSought)
{ 
	struct Node *node = list->head;
	int indexAt = 0;

  int arr_len = length(list);
  if (indexSought >= arr_len || indexSought < 0) {
    fprintf(stderr, "%s\n", "Index Out of Bounds Error.");
    exit(0);
  }

	// check that list is not empty
	if ( list->head ) {
		// if remove first index, it's just like popFront()
		if ( indexSought == 0 ) {
			unsigned long data = list->head->data;
			struct Node *node = list->head;
			list->head = list->head->next;
			free(node);
			return data;
		}

		// remove index other than first index
		while(node) {
			if( indexSought - 1 == indexAt ) {
				unsigned long data = node->next->data;
				struct Node *deletedNode = node->next;
				node->next = node->next->next;
				free(deletedNode);
				return data;
			}
			node = node->next;
			indexAt++;
		}
	}	
	return 0;
}

/*
 * Remove the first node from the list, and return the 'data' pointer that was stored in the node.
 * Returns NULL is the list is empty.
 */
unsigned long popFront(struct List *list)
{
	if(list->head) {
		unsigned long data = list->head->data;
		struct Node *node = list->head;
		list->head = list->head->next;
		free(node);
		return data;
	}
	return 0;
}

/* Remove all nodes from the list */
void removeAllNodes(struct List *list)
{
	while(list->head) {
		popFront(list);
	}
}

/*
 * Create a node that holds the given data pointer,
 * and add the node to the front of the list.
 * Returns the newly created node on success and NULL on failure.
 */
struct Node *addFront(struct List *list, unsigned long data)
{
	struct Node *node = malloc( sizeof(struct Node) );
	if(node == NULL){
		perror("malloc returned NULL");
		exit(1);
	}
	node->next = list->head;
	node->data = data;
	list->head = node;
	return node;
}

/*
 * Create a node that holds the given data pointer,
 * and add the node right after the node passed in as the 'prevNode'
 * parameter.  If 'prevNode' is NULL, this function is equivalent to
 * addFront().
 * Returns the newly created node on success and NULL on failure.
 */
struct Node *addAfter(struct List *list, struct Node *prevNode, unsigned long data)
{
	struct Node *node = malloc( sizeof(struct Node) );
	if( prevNode ) {
		node->next = prevNode->next;
		node->data = data;
		prevNode->next = node;
	}
	else {
		node->next = list->head;
		node->data = data;
		list->head = node;
	}
	return node;
}

/* Reverse the list. */
void reverseList(struct List *list)
{
	struct Node *prv = NULL;
	struct Node *cur = list->head;
	struct Node *nxt;

	while (cur) {
		nxt = cur->next;
		cur->next = prv;
		prv = cur;
		cur = nxt;
	}

	list->head = prv;
}

/* Get element at specified index from a list */
unsigned long getElement(struct List *list, int index) 
{
	struct Node *node_by_index = findByIndex(list, index);
  int arr_len = length(list);
  if (index >= arr_len || index < 0) {
    fprintf(stderr, "%s\n", "Index Out of Bounds Error.");
    exit(0); 
  }
	return node_by_index->data;
}

/* Insert element in list at specified index */
void insertElement(struct List *list, int index, unsigned long insert)
{
  int arr_len = length(list);
  if (index > arr_len || index < 0) {
    fprintf(stderr, "%s\n", "Index Out of Bounds Error.");
    exit(0); 
  }
	if (index == 0)
		addFront(list, insert);
	else {
		int x = index - 1;
		struct Node *node = findByIndex(list, x);
		addAfter(list, node, insert);
	}
}

/* Assign element at specified index to a new value */
void assignElement(struct List *list, int index, unsigned long insert)
{
	struct Node *node = findByIndex(list, index);
	node->data = insert;
}
