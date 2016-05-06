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
    VarDecl* hello = (VarDecl*)(sTable->lookupTable(vard));

    if( hello != NULL){
printf("Found a vardecl \n");
        if((string)(this->id->GetName()) != 
                (string)(hello->GetIdentifier()->GetName())){
//        printf(this->id->GetName());
//        printf(hello->GetIdentifier()->GetName());
        printf("If the ident aren't equal to each other /n");
          if(this-> GetTypeQ() != NULL && this -> GetType() != NULL 
              && this->id != NULL){
            hello = new VarDecl(this->id, 
                     this->GetType(), this->GetTypeQ(),this->GetExpr());
           }else if (this->GetType() != NULL && this->id != NULL)
           {
             hello = new VarDecl(this->id, this->GetType(), this-> GetExpr());   
           }else if (this->id != NULL && this->GetTypeQ() != NULL){
             hello = new VarDecl(this->id, this->GetTypeQ(),this->GetExpr());
           }else{
             hello = new VarDecl();
           }

            sTable->addEle(vard,hello);
            //ReportError::DeclConflict(this, hello); 
        }
        else{
        printf("If Identifier is equal to each other \n");
            ReportError::DeclConflict(this, hello);  
            sTable->popEle(vard);
          if(this-> GetTypeQ() != NULL && this -> GetType() != NULL 
              && this->id != NULL){
            hello = new VarDecl(this->id, 
                     this->GetType(), this->GetTypeQ(),this->GetExpr());
           }else if (this->GetType() != NULL && this->id != NULL)
           {
             hello = new VarDecl(this->id, this->GetType(), this-> GetExpr());   
           }else if (this->id != NULL && this->GetTypeQ() != NULL){
             hello = new VarDecl(this->id, this->GetTypeQ(),this->GetExpr());
           }else{
             hello = new VarDecl();
           }

            sTable->addEle(vard, hello);

        }
    }
    else{
printf("Creating new map variable \n");
           // hello->GetIdentifier()->SetName() = this->id->GetName():
           // hello->GetType() 
           if(this-> GetTypeQ() != NULL && this -> GetType() != NULL 
              && this->id != NULL){
            hello = new VarDecl(this->id, 
                     this->GetType(), this->GetTypeQ(),this->GetExpr());
           }else if (this->GetType() != NULL && this->id != NULL)
           {
             hello = new VarDecl(this->id, this->GetType(), this-> GetExpr());   
           }else if (this->id != NULL && this->GetTypeQ() != NULL){
             hello = new VarDecl(this->id, this->GetTypeQ(),this->GetExpr());
           }else{
             hello = new VarDecl();
           }
            sTable->addEle(vard,hello);
    }
}

void FnDecl::Check() {
    sTable->pushTable();
    string vard = this->id->GetName();
    Decl* hello = sTable->lookupTable(vard);
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

