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
	size_t *size_of_type;
	int *depth;
};

// Initialize and return an empty list.
struct List *initList(size_t *size_of_type, int *depth)
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

// Array access
void *getElement(struct List *list, int *index) 
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

// Contains int
bool contains(struct List *list, const void *dataSought)
{
	if (*(list->size_of_type) == sizeof(bool)) {
		struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))compareBools);
		if (node)
			return true;
		return false;
	}
	
	if (*(list->size_of_type) == sizeof(int)) {
		struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))compareInts);
		if (node)
			return true;
		return false;
	}
	
	if (*(list->size_of_type) == sizeof(char *)) {
		struct Node *node = findNode(list, dataSought, (int (*)(const void *, const void *))strcmp);
		if (node)
			return true;
		return false;
	}
	return false;
}

int getIndex(struct List *list, const void *dataSought)
{ 
	if (*(list->size_of_type) == sizeof(bool))
		return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))compareBools);
	if (*(list->size_of_type) == sizeof(int)) 
		return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))compareInts);
	if (*(list->size_of_type) == sizeof(char *))
		return findIndexOfNode(list, dataSought, (int (*)(const void *, const void *))strcmp);
	return -1;
}

void insertElement(struct List *list, int *index, void *insert)
{
	if (*index == 0)
		addFront(list, insert);
	else {
		int x = *index - 1;
		struct Node *node = findByIndex(list, &x);
		addAfter(list, node, insert);
	}
}

void assignElement(struct List *list, int *index, void *insert)
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
	/********************************************************************* BOOL ARRAYS ****************/

	bool boolarr[] = {true, true, true, true, true};
	struct List *boollist;

	// empty array
	size_t bool_size = sizeof(bool);
	int bool_depth = 1;
	boollist = initList(&bool_size, &bool_depth);
	printf("Length of list: %d\n", length(boollist));

	// print array
	printf("Print contents of list: ");
	traverseList(boollist, &printBool);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(boolarr)/sizeof(boolarr[0]); i++) {
		addFront(boollist, &boolarr[i]);
	}
	reverseList(boollist);
	printf("Length of list: %d\n", length(boollist));

	// print array
	printf("Print contents of list: ");
	traverseList(boollist, &printBool);
	printf("\n");

	// access
	int bool_a = 1;
	printf("Find Node at index 1: ");
	printf("%s\n", *(bool *)getElement(boollist, &bool_a) ? "true" : "false");

	// index_of
	bool bool_b = true;
	printf("Element 'true' is at index: %d\n", getIndex(boollist, &bool_b));
  
	// contains 
	bool bool_c = false;
	printf("Does list contain element 'false': ");
	if( contains(boollist, &bool_c) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	bool bool_c1 = true;
	printf("Does list contain element 'true': ");
	if( contains(boollist, &bool_c1) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	bool bool_d = false;
	int bool_pos = 1;
	printf("Insert element 'false' at position 1: ");
	insertElement(boollist, &bool_pos, &bool_d);
	traverseList(boollist, &printBool);
	printf("\n");

	// assign
	bool bool_e = true;
	printf("Set element at position 1 to be 'true' instead: ");
	assignElement(boollist, &bool_pos, &bool_e);
	traverseList(boollist, &printBool);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(boollist, &bool_pos);
	traverseList(boollist, &printBool);
	printf("\n");

	removeAllNodes(boollist);

	printf("\n");

	/********************************************************************* STRING ARRAYS ***************/

	char *strarr[] = {"abc", "def", "ghi", "jkl", "mno"};
	struct List *strlist;

	// empty array
	size_t str_size = sizeof(char *);
	int str_depth = 1;
	strlist = initList(&str_size, &str_depth);
	printf("Length of list: %d\n", length(strlist));

	// print array
	printf("Print contents of list: ");
	traverseList(strlist, &printStr);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(strarr)/sizeof(strarr[0]); i++) {
		addFront(strlist, strarr[i]);
	}
	reverseList(strlist);
	printf("Length of list: %d\n", length(strlist));

	// print array
	printf("Print contents of list: ");
	traverseList(strlist, &printStr);
	printf("\n");

	// access
	int str_a = 1;
	printf("Find Node at index 1: ");
	printf("%s\n", (char *)getElement(strlist, &str_a));

	// index_of
	char *str_b = "def";
	printf("Element 'def' is at index: %d\n", getIndex(strlist, str_b));
  
	// contains 
	char *str_c = "hello";
	printf("Does list contain element 'hello': ");
	if( contains(strlist, str_c) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	char *str_c1 = "abc";
	printf("Does list contain element 'abc': ");
	if( contains(strlist, str_c1) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	char *str_d = "yay";
	int str_pos = 1;
	printf("Insert element 'yay' at position 1: ");
	insertElement(strlist, &str_pos, str_d);
	traverseList(strlist, &printStr);
	printf("\n");

	// assign
	char *str_e = "wow";
	printf("Set element at position 1 to be 'wow' instead: ");
	assignElement(strlist, &str_pos, str_e);
	traverseList(strlist, &printStr);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(strlist, &str_pos);
	traverseList(strlist, &printStr);
	printf("\n");

	removeAllNodes(strlist);

	printf("\n");

	/******************************************************************** INT ARRAYS ******************/
	
	int intarr[] = {10, 2, 3, 7, 50};
	struct List *intlist;

	/*************** test creating a nested list *********************/
	printf("\nTesting creating a nested list: \n");	
	struct List *nestedlist;
	size_t nested_size = sizeof(intlist);	
	int nested_depth = 1;
	nestedlist = initList(&nested_size, &nested_depth);
	for(int i = 0; i < 4; i++) {
		addFront(nestedlist, &intarr);
	}
	reverseList(nestedlist);
	printf("Length of list: %d\n", length(nestedlist));
	printf("\n\n");

	removeAllNodes(nestedlist);
	/*************** test creating a nested list *********************/

	// empty array
	size_t int_size = sizeof(int);
	int int_depth = 1;
	intlist = initList(&int_size, &int_depth);
	printf("Length of list: %d\n", length(intlist));

	// print array
	printf("Print contents of list: ");
	traverseList(intlist, &printInt);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(intarr)/sizeof(intarr[0]); i++) {
		addFront(intlist, &intarr[i]);
	}
	reverseList(intlist);
	printf("Length of list: %d\n", length(intlist));

	// print array
	printf("Print contents of list: ");
	traverseList(intlist, &printInt);
	printf("\n");

	// access
	int int_a = 1;
	printf("Find Node at index 1: ");
	printf("%d\n", *(int *)getElement(intlist, &int_a));

	// index_of
	int int_b = 2;
	printf("Element 2 is at index: %d\n", getIndex(intlist, &int_b));
  
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

	return 0;
}

