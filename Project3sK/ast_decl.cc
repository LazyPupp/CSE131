/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "symtable.h"
//NOT GIVEN        
//#include "errors.h"
#include <string>         
Decl::Decl(Identifier *n) : Node(*n->GetLocation()) {
    Assert(n != NULL);
    (id=n)->SetParent(this); 
}

VarDecl::VarDecl(Identifier *n, Type *t, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    typeq = NULL;
}

VarDecl::VarDecl(Identifier *n, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && tq != NULL);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
    type = NULL;
}

VarDecl::VarDecl(Identifier *n, Type *t, TypeQualifier *tq, Expr *e) : Decl(n) {
    Assert(n != NULL && t != NULL && tq != NULL);
    (type=t)->SetParent(this);
    (typeq=tq)->SetParent(this);
    if (e) (assignTo=e)->SetParent(this);
}
void VarDecl::Check() {
     string vard = this->id->GetName();
     map<string,Decl*>* curScope = sTable-> v.back();
     if(curScope->end() != curScope->find(vard))
     {
        ReportError::DeclConflict(this, curScope->at(vard));
        curScope->erase(curScope->find(vard));
	curScope->insert(pair<string,Decl*>(vard,this)); 
     }else{
        curScope->insert(pair<string,Decl*> (vard,this) );
     }
 }

void FnDecl::Check() {
//    sTable->pushTable();

      string vard = this->id->GetName();
      map<string,Decl*>* curScope = sTable->v.back();
    
    //printing out the function name
   if(curScope->end() != curScope->find(vard))
     {
        //printf("cool \n");
        ReportError::DeclConflict(this, curScope->at(vard));
        curScope->erase(curScope->find(vard));
	curScope->insert(pair<string,Decl*>(vard,this)); 
     }else{
        //printf("bad \n");
        curScope->insert(pair<string,Decl*> (vard,this) );
     }

   //  std::map<string,Decl*> *m = new std::map<string,Decl*>();
   //  sTable->v.push_back(m);
      StmtBlock *s = (StmtBlock *)body;      
      s->Check(formals);
      
      /*if(formals->NumElements()>0){
         for(int i = 0; i< formals->NumElements(); ++i){
            formals->Nth(i)->Check();
            //printf("patricksucks \n");
         }
      }
     body->Check();*/
    // printf(this->id->GetName());
}

void VarDecl::PrintChildren(int indentLevel) { 
   if (typeq) typeq->Print(indentLevel+1);
   if (type) type->Print(indentLevel+1);
   if (id) id->Print(indentLevel+1);
   if (assignTo) assignTo->Print(indentLevel+1, "(initializer) ");
}

FnDecl::FnDecl(Identifier *n, Type *r, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r!= NULL && d != NULL);
    (returnType=r)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
    returnTypeq = NULL;
}

FnDecl::FnDecl(Identifier *n, Type *r, TypeQualifier *rq, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r != NULL && rq != NULL&& d != NULL);
    (returnType=r)->SetParent(this);
    (returnTypeq=rq)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
}

void FnDecl::SetFunctionBody(Stmt *b) { 
    (body=b)->SetParent(this);
}

void FnDecl::PrintChildren(int indentLevel) {
    if (returnType) returnType->Print(indentLevel+1, "(return type) ");
    if (id) id->Print(indentLevel+1);
    if (formals) formals->PrintAll(indentLevel+1, "(formals) ");
    if (body) body->Print(indentLevel+1, "(body) ");
}

