/**
 * File: symtable.h
 * ----------- 
 *  Header file for Symbol table implementation.
 */

#ifndef _H_symtable
#define _H_symtable

#include <stdio.h>
#include <vector>
#include <map>
#include "ast.h"
#include "ast_decl.h"
#include <string>
#include <iterator>

using namespace std;
class Identifier;
class Decl;

struct symType{
	Identifier *name;
	Decl *declar;
	bool *temp;
};

class SymbolTable{   //vect0or
   
   public:
      vector<map<string,Decl*>* > v;

      void pushTable();

      void popTable();

      Decl* lookupTable(string k);

      bool addEle(string k, Decl* declare);
};
/*
 * 

      bool lookupTable (string ident){ //lookup scopes
	vector<map<string,symType*>*>::iterator it;
	bool checker;
	int i = 0;
	for(it = v.begin(); it < v.end(); it++, i++){
		SymbolTable tab = new SymbolTable();
		tab = v.at(i); // SymbolTable
		checker = tab.lookupEle(ident);
		if( checker == true)
			return true;
	}
	return false;	
      }


class SymbolTable  {   //map
   protected:
      map<string,symType*> m;

   public:
      SymbolTable();
      void addEle (string key, symType val ){
	 if(lookupEle(key) == false){
	 	m.insert( pair<string,symType*>(key,*val));
	}
	else{
	return;
	}
      }
      bool lookupEle(string key){
	 map<string,symType>::iterator it;
	 int i = 0;
	 for(it = m.begin(); it < m.end(); it++, i++){
      	  	if( it == m.find(key))
			return true;
	 }
		return false;
      }
};
*/

#endif
