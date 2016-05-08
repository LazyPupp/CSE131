/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"

#include <map>
#include <algorithm>
#include "errors.h"
using namespace std;


IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
    this->retType = Type::intType;
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}

FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
    this->retType = Type::floatType;
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
    this->retType = Type::boolType;
}
void BoolConstant::PrintChildren(int indentLevel) { 
    printf("%s", value ? "true" : "false");
}

VarExpr::VarExpr(yyltype loc, Identifier *ident) : Expr(loc) {
    Assert(ident != NULL);
    this->id = ident;
}

void VarExpr::PrintChildren(int indentLevel) {
    id->Print(indentLevel+1);
}

void VarExpr::Check(){

  Decl* d;
  bool found = false;

  map<string, Decl*> *currTable;

  for(int i = 0; i < tables->NumElements(); i++){

    currTable = tables->Nth(i);
    map<string, Decl*>::iterator it = currTable->find(id->getName());

    if( it != currTable->end() ){
      d = it->second;
      found = true;
    }

  }

  if(!found){
    ReportError::IdentifierNotDeclared(id, LookingForVariable);
  }

  this->setType();

}

void VarExpr::setType(){

  bool typeset = false;
  map<string, Decl*> *currTable;

  for(int i = 0; i < tables->NumElements(); i++){

    currTable = tables->Nth(i);
    map<string, Decl*>::iterator it = currTable->find(id->getName());

    if( it != currTable->end() ){
      this->retType = it->second->getType();
      typeset = true;
      break;
    }
  }

  if(!typeset){
    this->retType = Type::errorType;
  }
}

Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}

void Operator::PrintChildren(int indentLevel) {
    printf("%s",tokenString);
}

bool Operator::IsOp(const char *op) const {		
	    return strcmp(tokenString, op) == 0;
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r) 
  : Expr(Join(l->GetLocation(), r->GetLocation())) {
    Assert(l != NULL && o != NULL && r != NULL);
    (op=o)->SetParent(this);
    (left=l)->SetParent(this); 
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r) 
  : Expr(Join(o->GetLocation(), r->GetLocation())) {
    Assert(o != NULL && r != NULL);
    left = NULL; 
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o) 
  : Expr(Join(l->GetLocation(), o->GetLocation())) {
    Assert(l != NULL && o != NULL);
    (left=l)->SetParent(this);
    (op=o)->SetParent(this);
}

void CompoundExpr::PrintChildren(int indentLevel) {
   if (left) left->Print(indentLevel+1);
   op->Print(indentLevel+1);
   if (right) right->Print(indentLevel+1);
}

ConditionalExpr::ConditionalExpr(Expr *c, Expr *t, Expr *f)
  : Expr(Join(c->GetLocation(), f->GetLocation())) {
    Assert(c != NULL && t != NULL && f != NULL);
    (cond=c)->SetParent(this);
    (trueExpr=t)->SetParent(this);
    (falseExpr=f)->SetParent(this);
}

void ConditionalExpr::PrintChildren(int indentLevel) {
    cond->Print(indentLevel+1, "(cond) ");
    trueExpr->Print(indentLevel+1, "(true) ");
    falseExpr->Print(indentLevel+1, "(false) ");
}

void ArithmeticExpr::Check(){
  if(left && right){
    left->Check();
    right->Check();
    if( (left->retType != right->retType) &&
        (left->retType != Type::errorType) && 
        (right->retType != Type::errorType)){
      ReportError::IncompatibleOperands(op, left->retType, right->retType);
    }
/*
    if(left->retType->IsEquivalentTo(ArrayType) )
	{
		printf("%s","Hello");
	}*/

    this->retType = left->retType;
  }
  else if(!left && right){
    right->Check();
    this->retType = right->retType;
    if(right->retType != Type::intType)
    {
      ReportError::IncompatibleOperand(op, right->retType);
      this->retType = Type::errorType;
    }
  }
  else if(left && !right){
    left->Check();

    this->retType = left->retType;

    if(left->retType != Type::intType)
    {
      ReportError::IncompatibleOperand(op, left->retType);
      this->retType = Type::errorType;
    }
  }
}

void RelationalExpr::Check(){
  left->Check();
  right->Check();

  if(   (left->retType != right->retType) &&
        (left->retType != Type::errorType) && 
        (right->retType != Type::errorType)){
    ReportError::IncompatibleOperands(op, left->retType, right->retType);
    this->retType = Type::errorType;
  }
  else{
    this->retType = Type::boolType;  
  }

}

void EqualityExpr::Check(){
  left->Check();
  right->Check();

  if( (left->retType != right->retType) &&
        (left->retType != Type::errorType) && 
        (right->retType != Type::errorType)){


    ReportError::IncompatibleOperands(op, left->retType, right->retType);
    this->retType = Type::errorType;
  }
  else{
    this->retType = Type::boolType;  
  }

  
}

void LogicalExpr::Check(){
  left->Check();
  right->Check();

  if( (left->retType != right->retType) &&
        (left->retType != Type::errorType) && 
        (right->retType != Type::errorType)){
    ReportError::IncompatibleOperands(op, left->retType, right->retType);
    this->retType = Type::errorType;
  }
  else{
    this->retType = Type::boolType;  
  }
}

void AssignExpr::Check(){
  left->Check();
  right->Check();

  if( (left->retType != right->retType) &&
        (left->retType != Type::errorType) && 
        (right->retType != Type::errorType)){
    ReportError::IncompatibleOperands(op, left->retType, right->retType);
    this->retType = Type::errorType;
  }
  else{
    this->retType = left->retType;
  }
}

void PostfixExpr::Check(){
  if(left) left->Check();

  if(left->retType != Type::intType && left->retType != Type::errorType)
    {
      ReportError::IncompatibleOperand(op, left->retType);
    }
}
  
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
  }
    
void ArrayAccess::Check(){
  if(base){
     base->Check();
     VarExpr *v = dynamic_cast<VarExpr *>(base);
     ArrayType *a = dynamic_cast<ArrayType *>(v->retType);
     if((a->GetElemType() != Type::intType)&&
        (a->GetElemType() != Type::floatType)&&
        (a->GetElemType() != Type::boolType)&&
        (a->GetElemType() != Type::uintType)&&
        (a->GetElemType() != Type::voidType)&&
        (a->GetElemType() != Type::mat2Type)&&
        (a->GetElemType() != Type::mat3Type)&&
        (a->GetElemType() != Type::mat4Type))
     {
//        cout<<base->retType;
//        cout<< "\n";
       // cout<<a->GetElemType();
        ReportError::NotAnArray(v->GetIdentifier());
        this-> retType = Type::errorType;
     }else{
        this->retType = a->GetElemType(); 
     }
  }

/*
  bool found = false;

  map<string, Decl*> *currTable;

  for(int i = 0; i < tables->NumElements(); i++){

    currTable = tables->Nth(i);
    map<string, Decl*>::iterator it = currTable->find(id->getName());

    if( it != currTable->end() ){
      d = it->second;
      found = true;
    }

  }

  if(!found){
    ReportError::IdentifierNotDeclared(id, LookingForVariable);
  }

  this->setType();*/

} 
FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}


  void FieldAccess::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    field->Print(indentLevel+1);
  }

void FieldAccess::Check(){

  if(base) base->Check();

  //Check if field is only [xyzw]*
  char chars[] = "xyzw";
  std::string str = field->getName();

  if(str.length() == 1){
    this->retType = Type::floatType;
  }
  else if(str.length() == 2){
    this->retType = Type::vec2Type;
  }
  else if(str.length() == 3){
    this->retType = Type::vec3Type;
  }
  else if(str.length() == 4){
    this->retType = Type::vec4Type;
  }
  else if(str.length() > 4){
      ReportError::OversizedVector(field, base);
      this->retType = Type::errorType;
  }

  //Check if vec type is big enough
  if(field->getName().find("w") != -1 && base->retType != Type::vec4Type){
    ReportError::SwizzleOutOfBound(field, base);
    this->retType = Type::errorType;
  }
  else if(field->getName().find("z") != -1 && base->retType == Type::vec2Type){
     ReportError::SwizzleOutOfBound(field, base);
     this->retType = Type::errorType;
  }

  
  

  for(unsigned int i = 0; i < strlen(chars); ++i )
  {
    str.erase(std::remove(str.begin(), str.end(), chars[i]), str.end());
  }

  if(str.length() >= 1){
    ReportError::InvalidSwizzle(field, base);
    this->retType = Type::errorType;
  }

  //Check if base is of a vectype
  if(base->retType != Type::vec2Type && base->retType != Type::vec3Type &&
    base->retType != Type::vec4Type ){
    ReportError::InaccessibleSwizzle(field, base);
    this->retType = Type::errorType;
  }
  
}

Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}

void Call::Check(){
  Decl* d;
  bool found = false;

  map<string, Decl*> *currTable;


       
  for(int i = 0; i < tables->NumElements(); i++){

    currTable = tables->Nth(i);
    map<string, Decl*>::iterator it = currTable->find(field->getName());

    if( it != currTable->end() ){
/*
    if( static_cast<int>(actuals->size()) > ( (currTable->at(field->getName()))->NumElements() ) ){
    ReportError::ExtraFormals(this->GetIdentifier(), static_cast<int>(actuals->size()),currTable->at(field->getName())->NumElements() );
    }
    if(){	
	ReportError::LessFormals(this->GetIdentifier(), static_cast<int>(actuals->size()),currTable->at(field->getName())->NumElements() );
    } */


      d = it->second;
      found = true;
    }

  }

  if(!found){
    ReportError::IdentifierNotDeclared(field, LookingForVariable);
  }

  this->setType();

}

 void Call::PrintChildren(int indentLevel) {
    if (base) base->Print(indentLevel+1);
    if (field) field->Print(indentLevel+1);
    if (actuals) actuals->PrintAll(indentLevel+1, "(actuals) ");
  }


 
