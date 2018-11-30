#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// A node in a linked list.
struct Node {
	void *data;
	struct Node *next;
};

// A linked list. 'head' points to the first node in the list.
struct List {
	struct Node *head;
	size_t size_of_type;
	int depth;
};

// Initialize and return an empty list.
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
struct Node *findByIndex(struct List *list, int indexSought) 
{
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
struct Node *findNode(struct List *list, const void *dataSought, int (*compar)(const void *, const void *))
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
int findIndexOfNode(struct List *list, const void *dataSought, int (*compar)(const void *, const void *))
{ 
	struct Node *node = list->head;
	int count = 0;
	while(node) {
		if( compar(dataSought, node->data) == 0 ) {
			return count;
		}
		node = node->next;
		count++;
	}
	return count;
}

/* Remove node at specific index, deallocate the memory for the node, 
 * and return the 'data' pointer that was stored in the node. 
 * Returns NULL if the list is empty 
 */
void *removeNode(struct List *list, int indexSought)
{ 
	struct Node *node = list->head;
	int indexAt = 0;

	// check that list is not empty
	if ( list->head ) {
		// if remove first index, it's just like popFront()
		if ( indexSought == 0 ) {
			void *data = list->head->data;
			struct Node *node = list->head;
			list->head = list->head->next;
			free(node);
			return data;
		}

		// remove index other than first index
		while(node) {
			if( indexSought - 1 == indexAt ) {
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

// Array access
void *getElement(struct List *list, int index) 
{
	struct Node *node_by_index = findByIndex(list, index);
	return node_by_index->data;
}

// function given to compare bools
int compareBools(const void *a, const void *b)
{
	if (*(bool *)a < *(bool *)b) return -1;
	if (*(bool *)a > *(bool *)b) return 1;
	return 0;
}

// function given to compare ints
int compareInts(const void *a, const void *b)
{
	if (*(int *)a < *(int *)b) return -1;
	if (*(int *)a > *(int *)b) return 1;
	return 0;
}

int compareLists(const void *a, const void *b, int depth)
{
	struct List *lista = (struct List *)a;
	struct List *listb = (struct List *)b;
	if ( length(lista) != length(listb) )
		return -1;
	if ( depth > 1 ) {
		int new_depth = depth - 1;
		return compareLists(a, b, new_depth);
	}
	if ( depth == 1) {
		struct Node *nodea = lista->head;
		struct Node *nodeb = listb->head;
		int total = 0;
		while( nodea ) {
			if (lista->size_of_type == sizeof(bool)) {
 				total += abs(compareBools(nodea->data, nodeb->data));
 			}
			if (lista->size_of_type == sizeof(int)) {
				total += abs(compareInts(nodea->data, nodeb->data));
			}
			if (lista->size_of_type == sizeof(char *)) {
				total += abs(strcmp(nodea->data, nodeb->data));
			}
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

// Contains int
bool contains(struct List *list, const void *dataSought)
{
	if (list->depth == 1) {
		if (list->size_of_type == sizeof(bool)) {
			struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))compareBools);
			if (node)
				return true;
			return false;
		}
	
		if (list->size_of_type == sizeof(int)) {
			struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))compareInts);
			if (node)
				return true;
			return false;
		}
	
		if (list->size_of_type == sizeof(char *)) {
			struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))strcmp);
			if (node)
				return true;
			return false;
		}
	} else {
		struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))compareLists);
		if (node)
			return true;
		return false;	
	}
	return false;
}

int getIndex(struct List *list, const void *dataSought)
{
	if (list->depth == 1) { 
		if (list->size_of_type == sizeof(bool))
			return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))compareBools);
		if (list->size_of_type == sizeof(int)) 
			return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))compareInts);
		if (list->size_of_type == sizeof(char *))
			return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))strcmp);
	} else {
		return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))compareLists);
	}
	return -1;
}

void insertElement(struct List *list, int index, void *insert)
{
	if (index == 0)
		addFront(list, insert);
	else {
		int x = index - 1;
		struct Node *node = findByIndex(list, x);
		addAfter(list, node, insert);
	}
}

void assignElement(struct List *list, int index, void *insert)
{
	struct Node *node = findByIndex(list, index);
	node->data = insert;
}

static void printBool(void *p)
{
	printf("%s ", *(bool *)p ? "true" : "false");
}

// function given to print strings
static void printStr(void *p)
{
	printf("%s ", (char *)p);
}

// function given to print ints
static void printInt(void *p)
{	
	printf("%d ", *(int *)p);
}

int main()
{
	int nested[3][3] = {{1, 2, 3}, {10, 20, 30}, {100, 200, 300}};
	struct List *intlist;
	
	struct List *intlist1;
	size_t sub_size1 = sizeof(int);
	int sub_depth1 = 1;
	intlist1 = initList(sub_size1, sub_depth1);
	for(int j = 0; j < sizeof(nested[0])/sizeof(nested[0][0]); j++) {	
		addFront(intlist1, &nested[0][j]);
	}
	reverseList(intlist1);
	traverseList(intlist1, &printInt);
	
	struct List *intlist2;
	size_t sub_size2 = sizeof(int);
	int sub_depth2 = 1;
	intlist2 = initList(sub_size2, sub_depth2);
	for(int j = 0; j < sizeof(nested[1])/sizeof(nested[1][0]); j++) {
		addFront(intlist2, &nested[1][j]);
	}
	reverseList(intlist2);
	traverseList(intlist2, &printInt);	

	// compare lists
	int x = compareLists(intlist1, intlist2, 2);
	printf("\n%d\n", x);
	
	int y = compareLists(intlist1, intlist1, 2);
	printf("\n%d\n", y);





	// empty array
	size_t nested_size = sizeof(intlist);
	int depth = 2;
	intlist = initList(nested_size, depth);
	printf("Length of list: %d\n", length(intlist));
	
	// array literal
	for(int i = 0; i < sizeof(nested)/sizeof(nested[0]); i++) {
		size_t sub_size = sizeof(int);
		int sub_depth = 1;
		struct List *sublist = initList(sub_size, sub_depth);
		for(int j = 0; j < sizeof(nested[i])/sizeof(nested[i][0]); j++) {
			addFront(sublist, &nested[i][j]);
		}
		reverseList(sublist);
		traverseList(sublist, &printInt);
		addFront(intlist, sublist);
	}
	reverseList(intlist);
	printf("Length of list: %d\n", length(intlist));

	// print array
//	printf("Print contents of list: ");
//	traverseList(intlist, &printInt);
//	printf("\n");

	// access
	int int_a = 1;
	struct List *elem = (struct List *)getElement(intlist, int_a);
	printf("Find Node at index 1: ");
	traverseList(elem, &printInt);

/*	
	// index_of
	printf("Element [10,20,30] is at index: %d\n", getIndex(intlist, elem));
  
	// contains 
	int int_c = 7;
	printf("Does list contain element 7: ");
	if( contains(intlist, &int_c) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	int int_c1 = -7;
	printf("Does list contain element -7: ");
	if( contains(intlist, &int_c1) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	int int_d = 8;
	int int_pos = 1;
	printf("Insert element 8 at position 1: ");
	insertElement(intlist, &int_pos, &int_d);
	traverseList(intlist, &printInt);
	printf("\n");

	// assign
	int int_e = 20;
	printf("Set element at position 1 to be 20 instead: ");
	assignElement(intlist, &int_pos, &int_e);
	traverseList(intlist, &printInt);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(intlist, &int_pos);
	traverseList(intlist, &printInt);
	printf("\n");

	removeAllNodes(intlist);
*/
	return 0;
}

