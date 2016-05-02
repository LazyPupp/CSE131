/**
 * File: symtable.h
 * ----------- 
 *  Header file for Symbol table implementation.
 */
#include <vector>
#include <map>
using namespace std;

class SymbolTable {
   protected:
      vector<const value_type& val> v;

   public:
      void pushTable (std::map a);
      void popTable (std::map a);
      void lookupTable ();
      void addEle (const value_type& val );
      void lookupEle();
}
