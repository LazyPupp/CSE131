/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "errors.h"

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

/*
VarDecl::VarDecl(Identifier *n, Type *t) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
}*/

 
void VarDecl::PrintChildren(int indentLevel) {
   if (typeq) typeq->Print(indentLevel+1); 
   if (type) type->Print(indentLevel+1);
   if (id) id->Print(indentLevel+1);
  if (assignTo) assignTo->Print(indentLevel+1, "(initializer) ");
}

void VarDecl::Check() {
    std::map<string, Decl*> *currTable = tables->Nth(0);
    std::string var = this->id->getName();
    std::map<string, Decl*>::iterator it = currTable->find(var);
    if(it != currTable->end()){
	if(this->getType() != it->second->getType()){
            ReportError::DeclConflict(this, it->second); 
            (*currTable)[var] = this; 
        }

        else{
            ReportError::DeclConflict(this, it->second); 
            (*currTable)[var] = this; 
        }
    }
    else{ 
	if(this->assignTo != NULL){
          this->assignTo->Check();
	  if(this->assignTo->retType->IsConvertibleTo(this->getType()) == false){
          	ReportError::InvalidInitialization(this->id, this->getType(),this->assignTo->retType);
	  }
	  else{
          	(*currTable)[var] = this;
	  }
	}
	  else{
          	(*currTable)[var] = this;
	  }
    }
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

void FnDecl::Check() {
/*
    if(elements < formals->NumElements()){
    ReportError::ExtraFormals(this->GetIdentifier(), elements, formals->NumElements());
    }
    if( elements > formals->NumElements()){	
	ReportError::LessFormals(this->GetIdentifier(), elements, formals->NumElements());
    } */

    escapeAllowed = false;
    retFound = false;
    currRetType = this->returnType;

    //Check if already declared
    std::map<string, Decl*> *currTable = tables->Nth(0);
    std::string var = this->id->getName();
    std::map<string, Decl*>::iterator it = currTable->find(var);

    if(it != currTable->end()){
        ReportError::DeclConflict(this, it->second);    
    }
    else{
        (*currTable)[var] = this;
    }

    //Initial symbol table
    map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable,0);
/*   
    if(elements < formals->NumElements()){
    ReportError::ExtraFormals(this->GetIdentifier(), formals->NumElements(),elements);
    }
    if( elements > formals->NumElements()){	
	ReportError::LessFormals(this->GetIdentifier(), formals->NumElements(),elements);
    } */
    //Check for declaration conflicts in formals
    for(int i = 0; i < formals->NumElements(); i++){
        formals->Nth(i)->Check();
    }


    if(body){
        //body->Check();

        for(int i = 0; i < ((StmtBlock*)body)->getStmts()->NumElements();i++){
            ((StmtBlock*)body)->getStmts()->Nth(i)->Check();
        }
    }

    if(!retFound && (this->returnType != Type::voidType)){
        ReportError::ReturnMissing(this);
    }

    //sdfadfgsd
    tables->RemoveAt(0);
}

