#include <iostream>
#include <assert.h>
using namespace std;

struct BST_Node {
    string key;
    BST_Node *left, *right;
};

typedef struct BST_Node *BST;

void BST_init(BST& root){
	root = NULL;
}

bool BST_isEmpty(BST root){
	return root == NULL;
}

void BST_print(BST curNode){
	if (curNode != NULL) {
		BST_print(curNode->left);
		cout<<"\t\""<<curNode->key<<"\"\n";
		BST_print(curNode->right);
	}
}

void testtree(BST node, int indent = 0) {
    for (int i = 0; i < indent; i++)
        cout << "    ";
    if (!node)
        cout << "empty" << endl;
    else {
        cout << node->key << endl;
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

BST BST_findparent(string key, BST curNode) {
    if (BST_isEmpty(curNode)) {
		return NULL;
	} else if (key==curNode->key) {
		return NULL;
	} else if (key< curNode->key) {
        if (curNode->left->key == key)
            return curNode;
		return BST_findparent(key, curNode->left);
	} else {
        if (curNode->right->key == key)
            return curNode;
		return BST_findparent(key, curNode->right);
	}
}

BST BST_find(string key, BST curNode) {
    if (BST_isEmpty(curNode)) {
		return NULL;
	} else if (key==curNode->key) {
		return curNode;
	} else if (key< curNode->key) {
		return BST_find(key, curNode->left);
	} else {
		return BST_find(key, curNode->right);
	}
}

int BST_kids(BST curNode) {
    int count = 0;
    if (curNode->left) count++;
    if (curNode->right) count++;
    return count;
}

bool BST_has(string key, BST curNode){
    return BST_find(key, curNode) != NULL;
}

BST BST_insert(string key, BST& curNode) {
	if (curNode == NULL) {
		curNode = new BST_Node;
		curNode->key = key;
		curNode->left = NULL;
		curNode->right = NULL;
	} else if (key == curNode->key) {
		cerr << "duplicate key: " << key << endl;
		assert(false);
	} else if (key < curNode->key) {
		curNode->left = BST_insert(key, curNode->left);
	} else {
		curNode->right = BST_insert(key, curNode->right);
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
    return curNode->key;
}

BST& BST_parentlink(BST parent, BST child) {
    if (parent->left == child) return parent->left;
    return parent->right;
}

BST& BST_parentlink(string key, BST root) {
    BST parent = BST_findparent(key, root);
    if (parent->left && parent->left->key == key)
        return parent->left;
    return parent->right;
}

BST BST_delete(BST& root, BST parent, BST node) {
    BST most, mostparent;
    string key;
    
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
            key = BST_most(node->left);
            most = BST_find(key, node);
            mostparent = BST_findparent(key, node);
            most->key = node->key;
            node->key = key;
            return BST_delete(root, mostparent, most);
            break;
    }
}

BST BST_delete(string key, BST& curNode) {
    BST parent = BST_findparent(key, curNode);
    BST node = BST_find(key, curNode);
    
    return BST_delete(curNode, parent, node);
}

int main() {
    BST test;
    BST_init(test);
    BST_insert("hotel", test);
    BST_insert("india", test);
    BST_insert("juliette", test);
    BST_insert("december", test);
    BST_insert("charlie", test);
    BST_insert("echo", test);
    //BST_print(test);
    BST_delete("hotel", test);
    BST_delete("echo", test);
    BST_delete("charlie", test);
    BST_delete("juliette", test);
    BST_delete("december", test);
    BST_delete("india", test);
        testtree(test);
    
    /*cout << endl << endl << endl;
    testtree(test);
    BST_delete("hotel", test);
    cout << endl << endl << endl;
    testtree(test);
    BST_delete("india", test);
    cout << endl << endl << endl;
    testtree(test);
    BST_delete("juliette", test);
    cout << endl << endl << endl;
    testtree(test);*/
}