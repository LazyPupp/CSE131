/*
 * Symbol table implementation
 *
 */
#include <stdio.h>
#include <vector>
#include <map>
#include <string>
#include <iterator>
#include "symtable.h"

using namespace std;
/*
void SymbolTable::pushTable (){
	map<string, Decl*> *m;
	m = new map<string, Decl*>();
	v.push_back(m);
      }

void SymbolTable::popTable (){
	v.pop_back();
      }

Decl* SymbolTable::lookupTable(string k){
	int i;
	i = v.size()-1;
	while(i>=0){
	  if(v[i] -> end() != v[i]->find(k) ){
	    return v[i]->at(k);
	  }
	  i--;
	}
	return NULL;	
      }

Decl* SymbolTable::lookupCur(string k){

   int i = v.size()-1;
   if (v[i] ->end() != v[i]->find(k))
   {
      return v[i]->at(k);
   }
   return NULL;
}
*/
/*
void SymbolTable::addEle(string k, Decl * declare){
	if(v[v.size()-1] -> end() == v[v.size()-1]->find(k) )
	  return false;
	else{
	  v.back()->insert(pair<string, Decl*>(k,declare));
	  return true;
	}*/
/*
v.back()->insert(pair<string, Decl*>(k,declare));
}
void SymbolTable::popEle(string k){
v.back()-> erase(k);
}

*/
