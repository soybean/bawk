#ifndef _MYLIST_H_
#define _MYLIST_H_

// A node in a linked list.
struct Node {
  void *data;
  struct Node *next;
};

// A linked list. 'head' points to the first node in the list.
struct List {
	struct Node *head;
};

// Initialize an empty list.
static inline void initList(struct List *list)
{
	list->head = 0;
}

// Returns 1 if the list is empty, 0 otherwise.
static inline int isEmptyList(struct List *list)
{
	return (list->head == 0);
}

void traverseList(struct List *list, void (*f)(void *));

struct Node *findByIndex(struct List *list, int *indexSought);

int length(struct List *list);

struct Node *findNode(struct List *list, const void *dataSought,
            						int (*compar)(const void *, const void *));

int findIndexOfNode(struct List *list, const void *dataSought,
											int (*compar)(const void *, const void *));

void *removeNode(struct List *list, int *indexSought);

void *popFront(struct List *list);

void removeAllNodes(struct List *list);

struct Node *addFront(struct List *list, void *data);

struct Node *addAfter(struct List *list, struct Node *prevNode, void *data);

void reverseList(struct List *list);

#endif

