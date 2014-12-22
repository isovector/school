#include <iostream>
#include <assert.h>
using namespace std;

struct SBLnode {
    string name;
    SBLnode *left, *right, *next;
};

struct Queue {
    SBLnode *first, *last;
};

typedef struct SBLnode *BST;

struct SBL {
    Queue q;
    BST root;
    int size;
};

void testtree(BST node, int indent = 0) {
    for (int i = 0; i < indent; i++)
        cout << "    ";
    if (!node)
        cout << "empty" << endl;
    else {
        cout << node->name << endl;
        for (int i = 0; i < indent; i++)
            cout << "    ";
        cout << "left:" << endl;
        testtree(node->left, indent + 1);
        for (int i = 0; i < indent; i++)
            cout << "    ";
        cout << "right:" << endl;
        testtree(node->right, indent + 1);
    }
}

void BST_init(BST& root){
	root = NULL;
}

bool BST_isEmpty(BST root){
	return root == NULL;
}

bool SBL_isEmpty(SBL root) {
    return root.root == NULL;
}

void BST_print(BST curNode){
	if (curNode != NULL) {
		BST_print(curNode->left);
		cout << curNode->name << endl;
		BST_print(curNode->right);
	}
}

BST BST_findparent(string name, BST curNode) {
    if (BST_isEmpty(curNode)) {
		return NULL;
	} else if (name==curNode->name) {
		return NULL;
	} else if (name< curNode->name) {
        if (curNode->left->name == name)
            return curNode;
		return BST_findparent(name, curNode->left);
	} else {
        if (curNode->right->name == name)
            return curNode;
		return BST_findparent(name, curNode->right);
	}
}

BST BST_find(string name, BST curNode) {
    if (BST_isEmpty(curNode)) {
		return NULL;
	} else if (name==curNode->name) {
		return curNode;
	} else if (name< curNode->name) {
		return BST_find(name, curNode->left);
	} else {
		return BST_find(name, curNode->right);
	}
}

int BST_kids(BST curNode) {
    int count = 0;
    if (curNode->left) count++;
    if (curNode->right) count++;
    return count;
}

bool BST_has(string name, BST curNode){
    return BST_find(name, curNode) != NULL;
}

BST BST_insert(string name, BST& curNode) {
	if (curNode == NULL) {
		curNode = new SBLnode;
		curNode->name = name;
		curNode->left = NULL;
		curNode->right = NULL;
        curNode->next = NULL;
	} else if (name == curNode->name) {
		cerr << "duplicate name: " << name << endl;
		assert(false);
	} else if (name < curNode->name) {
		curNode->left = BST_insert(name, curNode->left);
	} else {
		curNode->right = BST_insert(name, curNode->right);
	}
	return curNode;
}

BST BST_onlychild(BST node) {
    if (node->left)
        return node->left;
    return node->right;
}

string BST_most(BST curNode) {
    while (curNode->right)
        curNode = curNode->right;
    return curNode->name;
}

BST& BST_parentlink(BST parent, BST child) {
    if (parent->left == child) return parent->left;
    return parent->right;
}

BST& BST_parentlink(string name, BST root) {
    BST parent = BST_findparent(name, root);
    if (parent->left && parent->left->name == name)
        return parent->left;
    return parent->right;
}

BST& BST_getlink(BST& root, BST target) {
    BST parent = BST_findparent(target->name, root);
    if (!parent) return root;
    if (parent->left == target) return parent->left;
    return parent->right;
}

void BST_swapnodes(BST a, BST b, BST root) {
    BST &alink = BST_getlink(root, a);
    BST &blink = BST_getlink(root, b);
    alink = b;
    blink = a;
    BST temp = a->left;
    a->left = b->left;
    b->left = temp;
    temp = a->right;
    a->right = b->right;
    b->right = temp;
    temp = a->next;
    a->next = b->next;
    b->next = temp;
}

BST BST_delete(BST& root, BST parent, BST node) {
    BST most, mostparent, next;
    string name;
    BST findnode = root;
    
    switch (BST_kids(node)) {
        case 0:
            if (parent)
                BST_parentlink(parent, node) = NULL;
            else root = NULL;
           
            break;
        case 1:
            if (parent)
                BST_parentlink(parent, node) = BST_onlychild(node);
            else root = BST_onlychild(node);
            break;
        case 2:
            name = BST_most(node->left);
            most = BST_find(name, node);
            BST_swapnodes(node, most, root);
            
            return BST_delete(root, parent, node);
            break;
    }
}

BST BST_delete(string name, BST& curNode) {
    BST parent = BST_findparent(name, curNode);
    BST node = BST_find(name, curNode);
    
    return BST_delete(curNode, parent, node);
}

void SBL_init(SBL& sbl) {
    sbl.q.first = sbl.q.last = sbl.root = NULL;
    sbl.size = 0;
}

int SBL_size(const SBL& sbl) {
    return sbl.size;
}

void SBL_arrive(string name, SBL& sbl) {
    BST_insert(name, sbl.root);
    BST node = BST_find(name, sbl.root);
    if (sbl.q.last)
        sbl.q.last->next = node;
    else
        sbl.q.first = node;
    sbl.q.last = node;
    sbl.size++;
}

void SBL_leave(SBL& sbl) {
    assert(sbl.q.first);
    
    BST node = sbl.q.first;
    sbl.q.first = node->next;
    if (sbl.q.first == NULL)
        sbl.q.last = NULL;
    BST_delete(node->name, sbl.root);
    sbl.size--;
}

string SBL_first(const SBL& sbl) {
    assert(sbl.q.first);
    return sbl.q.first->name;
}

bool SBL_lookup(const SBL& sbl, string name) {
    return BST_has(name, sbl.root);
}

void SBL_printInArrivalOrder(const SBL& sbl) {
    BST node = sbl.q.first;
    while (node) {
        cout << node->name << endl;
        node = node->next;
    }
}

void SBL_printInAlphabeticalOrder(const SBL& sbl) {
    BST_print(sbl.root);
}




int main() {
    SBL test;
    SBL_init(test);
    SBL_arrive("roger", test);
    SBL_arrive("graeme", test);
    SBL_arrive("zebraman", test);
    SBL_arrive("thuggnificent", test);
    SBL_arrive("charlie", test);
    SBL_arrive("sandy", test);
    SBL_arrive("gerald", test);
    //SBL_leave(test);
    
    testtree(test.root);
    
    /*SBL_leave(test);
    SBL_leave(test);
    SBL_leave(test);
    SBL_leave(test);
    SBL_leave(test);
    SBL_leave(test);*/

    //SBL_printInArrivalOrder(test);    
}