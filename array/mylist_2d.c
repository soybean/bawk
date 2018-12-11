#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// A node in a linked list.
struct Node {
	unsigned long data;
	struct Node *next;
};

// A linked list. 'head' points to the first node in the list.
struct List {
	struct Node *head;
	size_t size_of_type;
	int depth;
};

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

int compareStrs(const void *a, const void *b)
{
	return strcmp((const char *)a, (const char *)b);
}

int compareLists(const void *a, const void *b, int (*compar)(const void *, const void *))
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
 			total += abs(compareLists((void *)(nodea->data), (void *)(nodeb->data), compar));
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
 			total += abs(compar((void *)(nodea->data), (void *)(nodeb->data)));
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
		f((void *)(node->data));
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
	if(list->depth == 1) {
		while(node) {
			if( compar(dataSought, (void *)(node->data)) == 0 )
				return node;
			node = node->next;
		}
	}
	if(list->depth > 1) {
		while(node) {
			if( compareLists(dataSought, (void *)(node->data), compar) == 0 )
				return node;
			node = node->next;
		}
	}
	return NULL;    
}

int contains(struct List *list, const void *dataSought, int (*compar)(const void *, const void *)) {
	struct Node *found = findNode(list, dataSought, compar);
	if (found)
		return 1;
	return 0;
}

// Traverse list to find index of node
int findIndexOfNode(struct List *list, const void *dataSought, int (*compar)(const void *, const void *))
{ 
	struct Node *node = list->head;
	int count = 0;
	if(list->depth == 1) {
		while(node) {
			if( compar(dataSought, (void *)(node->data)) == 0 ) {
				return count;
			}
			node = node->next;
			count++;
		}
	}
	if(list->depth > 1) {
		while(node) {
			if( compareLists(dataSought, (void *)(node->data), compar) == 0 ) {
				return count;
			}
			node = node->next;
			count++;
		}
	}
	return count;
}

/* Remove node at specific index, deallocate the memory for the node, 
 * and return the 'data' pointer that was stored in the node. 
 * Returns NULL if the list is empty 
 */
unsigned long removeNode(struct List *list, int indexSought)
{ 
	struct Node *node = list->head;
	int indexAt = 0;

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
 * Remove the first node from the list, deallocate the memory for the
 * node, and return the 'data' pointer that was stored in the node.
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
unsigned long getElement(struct List *list, int index) 
{
	struct Node *node_by_index = findByIndex(list, index);
	return node_by_index->data;
}

void insertElement(struct List *list, int index, unsigned long insert)
{
	if (index == 0)
		addFront(list, insert);
	else {
		int x = index - 1;
		struct Node *node = findByIndex(list, x);
		addAfter(list, node, insert);
	}
}

void assignElement(struct List *list, int index, unsigned long insert)
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
	struct List *nestedlist;
	
	struct List *intlist1;
	size_t sub_size1 = sizeof(int);
	int sub_depth1 = 1;
	intlist1 = initList(sub_size1, sub_depth1);
	for(int j = 0; j < sizeof(nested[0])/sizeof(nested[0][0]); j++) {	
		addFront(intlist1, (unsigned long)&nested[0][j]);
	}
	reverseList(intlist1);

	struct List *intlist2;
	size_t sub_size2 = sizeof(int);
	int sub_depth2 = 1;
	intlist2 = initList(sub_size2, sub_depth2);
	for(int j = 0; j < sizeof(nested[1])/sizeof(nested[1][0]); j++) {
		addFront(intlist2, (unsigned long)&nested[1][j]);
	}
	reverseList(intlist2);

	// compare lists
	int x = compareLists(intlist1, intlist2, (int (*)(const void *, const void *))compareInts);
	printf("[1,2,3] == [10,20,30]? %d\n", x);
	
	int y = compareLists(intlist1, intlist1, (int (*)(const void *, const void *))compareInts);
	printf("[1,2,3] == [1,2,3]? %d\n", y);

	// empty array
	size_t nested_size = sizeof(nestedlist);
	int depth = 2;
	nestedlist = initList(nested_size, depth);
	printf("Length of list: %d\n", length(nestedlist));
	
	// array literal
	for(int i = 0; i < sizeof(nested)/sizeof(nested[0]); i++) {
		size_t sub_size = sizeof(int);
		int sub_depth = 1;
		struct List *sublist = initList(sub_size, sub_depth);
		for(int j = 0; j < sizeof(nested[i])/sizeof(nested[i][0]); j++) {
			addFront(sublist, (unsigned long)&nested[i][j]);
		}
		reverseList(sublist);
		addFront(nestedlist, (unsigned long)sublist);
	}
	reverseList(nestedlist);
	printf("Length of list: %d\n", length(nestedlist));

	// print array
	printf("Print contents of list: ");
	struct Node *nested_node = nestedlist->head;
	while(nested_node) {
		traverseList((struct List *)(nested_node->data), &printInt);
		nested_node = nested_node->next;
	}
	printf("\n");

	// access
	int nested_int_a = 1;
	struct List *elem = (struct List *)getElement(nestedlist, nested_int_a);
	printf("Find Node at index 1: ");
	traverseList(elem, &printInt);
	printf("\n");

	// index_of
	printf("Element [10,20,30] is at index: %d\n", findIndexOfNode(nestedlist, elem, (int (*)(const void *, const void *))compareInts));

	// contains 
	printf("Does list contain element [10,20,30]: ");
	if( contains(nestedlist, elem, (int (*)(const void *, const void *))compareInts) )
		printf("YES\n");
	else
		printf("NO\n");

	int test[] = {0, 0, 0};
	struct List *testlist;
	size_t test_size = sizeof(int);
	int test_depth = 1;
	testlist = initList(test_size, test_depth);
	for(int j = 0; j < sizeof(test)/sizeof(test[0]); j++) {
		addFront(testlist, (unsigned long)&test[j]);
	} 
	reverseList(testlist);

	// contains
	printf("Does list contain element [0,0,0]: ");
	if( contains(nestedlist, testlist, (int (*)(const void *, const void *))compareInts) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	int nested_int_pos = 1;
	printf("Insert element [0,0,0] at position 1: ");
	insertElement(nestedlist, nested_int_pos, (unsigned long)testlist);
	struct Node *node1 = nestedlist->head;
	while(node1) {
		traverseList((struct List *)(node1->data), &printInt);
		node1 = node1->next;
	}
	printf("\n");

	// assign
	printf("Set element at position 1 to be [10,20,30] instead: ");
	assignElement(nestedlist, nested_int_pos, (unsigned long)intlist2);
	struct Node *node2 = nestedlist->head;
	while(node2) {
		traverseList((struct List *)(node2->data), &printInt);
		node2 = node2->next;
	}
	printf("\n");

	// delete
	int nested_int_pos1 = 0;
	printf("Remove element at position 0: ");
	removeNode(nestedlist, nested_int_pos1);
	struct Node *node3 = nestedlist->head;
	while(node3) {
		traverseList((struct List *)(node3->data), &printInt);
		node3 = node3->next;
	}
	printf("\n");

	removeAllNodes(nestedlist);

	printf("\n");

	/************************************************** STRING ARRAYS  ********************************************************/

	char *strarr[] = {"abc", "def", "ghi", "jkl", "mno"};
	struct List *strlist;

	// empty array
	size_t str_size = sizeof(char *);
	int str_depth = 1;
	strlist = initList(str_size, str_depth);
	printf("Length of list: %d\n", length(strlist));

	// print array
	printf("Print contents of list: ");
	traverseList(strlist, &printStr);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(strarr)/sizeof(strarr[0]); i++) {
		addFront(strlist, (unsigned long)strarr[i]);
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
	printf("%s\n", (char *)getElement(strlist, str_a));

	// index_of
	char *str_b = "def";
	printf("Element 'def' is at index: %d\n", findIndexOfNode(strlist, str_b, (int (*)(const void *, const void *))compareStrs));
  
	// contains 
	char *str_c = "hello";
	printf("Does list contain element 'hello': ");
	if( contains(strlist, str_c, (int (*)(const void *, const void *))compareStrs) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	char *str_c1 = "abc";
	printf("Does list contain element 'abc': ");
	if( contains(strlist, str_c1, (int (*)(const void *, const void *))compareStrs) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	char *str_d = "yay";
	int str_pos = 1;
	printf("Insert element 'yay' at position 1: ");
	insertElement(strlist, str_pos, (unsigned long)str_d);
	traverseList(strlist, &printStr);
	printf("\n");

	// assign
	char *str_e = "wow";
	printf("Set element at position 1 to be 'wow' instead: ");
	assignElement(strlist, str_pos, (unsigned long)str_e);
	traverseList(strlist, &printStr);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(strlist, str_pos);
	traverseList(strlist, &printStr);
	printf("\n");

	removeAllNodes(strlist);

	printf("\n");

	/********************************************************************* BOOL ARRAYS ****************/

	bool boolarr[] = {true, true, true, true, true};
	struct List *boollist;

	// empty array
	size_t bool_size = sizeof(bool);
	int bool_depth = 1;
	boollist = initList(bool_size, bool_depth);
	printf("Length of list: %d\n", length(boollist));

	// print array
	printf("Print contents of list: ");
	traverseList(boollist, &printBool);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(boolarr)/sizeof(boolarr[0]); i++) {
		addFront(boollist, (unsigned long)&boolarr[i]);
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
	printf("%s\n", *(bool *)getElement(boollist, bool_a) ? "true" : "false");

	// index_of
	bool bool_b = true;
	printf("Element 'true' is at index: %d\n", findIndexOfNode(boollist, &bool_b, (int (*)(const void *, const void *))compareBools));
  
	// contains 
	bool bool_c = false;
	printf("Does list contain element 'false': ");
	if( contains(boollist, &bool_c, (int (*)(const void *, const void *))compareBools) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	bool bool_c1 = true;
	printf("Does list contain element 'true': ");
	if( contains(boollist, &bool_c1, (int (*)(const void *, const void *))compareBools) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	bool bool_d = false;
	int bool_pos = 1;
	printf("Insert element 'false' at position 1: ");
	insertElement(boollist, bool_pos, (unsigned long)&bool_d);
	traverseList(boollist, &printBool);
	printf("\n");

	// assign
	bool bool_e = true;
	printf("Set element at position 1 to be 'true' instead: ");
	assignElement(boollist, bool_pos, (unsigned long)&bool_e);
	traverseList(boollist, &printBool);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(boollist, bool_pos);
	traverseList(boollist, &printBool);
	printf("\n");

	removeAllNodes(boollist);

	printf("\n");

		/******************************************************************** INT ARRAYS ******************/
	
	int intarr[] = {10, 2, 3, 7, 50};
	struct List *intlist;

	// empty array
	size_t int_size = sizeof(int);
	int int_depth = 1;
	intlist = initList(int_size, int_depth);
	printf("Length of list: %d\n", length(intlist));

	// print array
	printf("Print contents of list: ");
	traverseList(intlist, &printInt);
	printf("\n");

	// array literal
	for(int i = 0; i < sizeof(intarr)/sizeof(intarr[0]); i++) {
		addFront(intlist, (unsigned long)&intarr[i]);
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
	printf("%d\n", *(int *)getElement(intlist, int_a));

	// index_of
	int int_b = 2;
	printf("Element 2 is at index: %d\n", findIndexOfNode(intlist, &int_b, (int (*)(const void *, const void *))compareInts));
  
	// contains 
	int int_c = 7;
	printf("Does list contain element 7: ");
	if( contains(intlist, &int_c, (int (*)(const void *, const void *))compareInts) )
		printf("YES\n");
	else
		printf("NO\n");

	// contains
	int int_c1 = -7;
	printf("Does list contain element -7: ");
	if( contains(intlist, &int_c1, (int (*)(const void *, const void *))compareInts) )
		printf("YES\n");
	else
		printf("NO\n");

	// insert
	int int_d = 8;
	int int_pos = 1;
	printf("Insert element 8 at position 1: ");
	insertElement(intlist, int_pos, (unsigned long)&int_d);
	traverseList(intlist, &printInt);
	printf("\n");

	// assign
	int int_e = 20;
	printf("Set element at position 1 to be 20 instead: ");
	assignElement(intlist, int_pos, (unsigned long)&int_e);
	traverseList(intlist, &printInt);
	printf("\n");

	// delete
	printf("Remove element at position 1: ");
	removeNode(intlist, int_pos);
	traverseList(intlist, &printInt);
	printf("\n");

	removeAllNodes(intlist);

	return 0;
}

