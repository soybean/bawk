#include <stdio.h>
#include <stdlib.h>
#include "mylist.h"

// Traverse the list, calling f() with each data item.
void traverseList(struct List *list, void (*f)(void *))
{
	struct Node *node = list->head;
  while(node) {
 		f(node->data);
  	node = node->next;
	}
}

// Traverse the list until node at index found. NULL if not found.
struct Node *findByIndex(struct List *list, int *indexSought) 
{
	struct Node *node = list->head;
  int indexAt = 0;
	while(node) {
		if( *indexSought == indexAt )
			return node;
		node = node->next;
		indexAt++;
	}	
	return NULL;
}

// Traverse list to find length of list
int length(struct List *list) {
	struct Node *node = list->head;
	int count = 0;
	while(node) {
		node = node->next;
		count++;
	}
	return count;
}

/*
 * Traverse the list, comparing each data item with 'dataSought' using
 * 'compar' function.  ('compar' returns 0 if the data pointed to by
 * the two parameters are equal, non-zero value otherwise.)
 *
 * Returns the first node containing the matching data, 
 * NULL if not found.
 */
struct Node *findNode(struct List *list, const void *dataSought,
												int (*compar)(const void *, const void *))
{
	struct Node *node = list->head;
  while(node) {
 	  if( compar(dataSought, node->data) == 0 )
      return node;
    node = node->next;
  }
 	return NULL;    
}

// Traverse list to find index of node
int findIndexOfNode(struct List *list, const void *dataSought,
											int (*compar)(const void *, const void *))
{ 
	struct Node *node = list->head;
	int count = 0;
	while(node) {
		if( compar(dataSought, node->data) == 0 )
			return count;
		node = node->next;
		count++;
	}
	return count;
}

/* Remove node at specific index, deallocate the memory for the node, 
 * and return the 'data' pointer that was stored in the node. 
 * Returns NULL if the list is empty 
 */
void *removeNode(struct List *list, int *indexSought)
{ 
	struct Node *node = list->head;
	int indexAt = 0;

	// check that list is not empty
	if ( list->head ) {
		// if remove first index, it's just like popFront()
		if ( *indexSought == 0 ) {
    	void *data = list->head->data;
			struct Node *node = list->head;
			list->head = list->head->next;
			free(node);
			return data;
  	}

		// remove index other than first index
		while(node) {
			if( *indexSought - 1 == indexAt ) {
				void *data = node->next->data;
				struct Node *deletedNode = node->next;
				node->next = node->next->next;
				free(deletedNode);
				return data;
	 		}
			node = node->next;
			indexAt++;
		}
	}	
	return NULL;
}

/*
 * Remove the first node from the list, deallocate the memory for the
 * node, and return the 'data' pointer that was stored in the node.
 * Returns NULL is the list is empty.
 */
void *popFront(struct List *list)
{
	if(list->head) {
  	void *data = list->head->data;
    struct Node *node = list->head;
    list->head = list->head->next;
    free(node);
    return data;
  }
  return NULL;
}

// Remove all nodes from the list, deallocating the memory for the nodes.
void removeAllNodes(struct List *list)
{
	while(list->head) {
  	popFront(list);
  }
}

/*
 * Create a node that holds the given data pointer,
 * and add the node to the front of the list.
 *
 * Note that this function does not manage the lifetime of the object
 * pointed to by 'data'.
 * 
 * It returns the newly created node on success and NULL on failure.
 */
struct Node *addFront(struct List *list, void *data)
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
 *
 * Note that prevNode, if not NULL, is assumed to be one of the nodes
 * in the given list.  The behavior of this function is undefined if
 * prevNode does not belong in the given list.
 *
 * Note that this function does not manage the lifetime of the object
 * pointed to by 'data'.
 * 
 * It returns the newly created node on success and NULL on failure.
 */
struct Node *addAfter(struct List *list, struct Node *prevNode, void *data)
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

/* 
 * Reverse the list.
 *
 * Note that this function reverses the list purely by manipulating
 * pointers.  It does NOT call malloc directly or indirectly (which
 * means that it does not call addFront() or addAfter()).
 */
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
