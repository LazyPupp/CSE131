/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */

#include <string.h>
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "symtable.h"

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
//make value to llvm
    this->val = llvm::ConstantInt::get(irgen.GetIntType(), val);
}
void IntConstant::PrintChildren(int indentLevel) { 
    printf("%d", value);
}

FloatConstant::FloatConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
//make value to llvm
    this->val = llvm::ConstantFP::get(irgen.GetFloatType(), val);
}
void FloatConstant::PrintChildren(int indentLevel) { 
    printf("%g", value);
}

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
    int check;		
    if(val == false){ 
	check = 0;	
    }	
    else{ 
	check = 1;
    }				
    this->val = llvm::ConstantInt::get(irgen.GetBoolType(), check);
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

void VarExpr::Emit(){		
	
	    llvm::BasicBlock *bb = irgen.GetBasicBlock();		
				
	    Decl* declare = NULL;		
	    std::map<std::string, Decl*> *currTable;
            int kyle = 0;		
	    while(kyle < tables->NumElements()){					
	        currTable = tables->Nth(kyle);					
	        std::map<std::string, Decl*>::iterator iter = 		
	          currTable->find(this->getName());					
	        if(iter != currTable->end()){ //iterate end of scope		
	          declare = iter->second;		
	          break;		
	        }
	     kyle++;		
	    }		
	
	    if(declare) 
		val = new llvm::LoadInst(declare->GetMem(), "", bb);		
	}		
			
Decl *VarExpr::GetDecl(){		
	  Decl* declare = NULL;		
	    std::map<std::string, Decl*> *currTable;
	    int z = 0;		
	    while( z < tables->NumElements()){					
	        currTable = tables->Nth(z);			
	        std::map<std::string, Decl*>::iterator iter = 		
	          currTable->find(this->getName());		
			//iterate to the end of the scope
	        if(iter != currTable->end()){		
	          declare = iter->second;		
	          break;		
	        }
	    z++;		
	    }		
	  return declare;				
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

void RelationalExpr::Emit(){

  llvm::BasicBlock *bb = irgen.GetBasicBlock();
//sets left hand sides and the right hand sides
  left->Emit();
  right->Emit();

  bool int_flag = left->GetValue()->getType() == irgen.GetIntType() ? true : false;

  llvm::CmpInst::OtherOps llvmOP =
    int_flag ? llvm::CmpInst::ICmp : llvm::CmpInst::FCmp;
  llvm::CmpInst::Predicate pred;
  if (op->getOp() == "<=") {
    pred = int_flag ? llvm::ICmpInst::ICMP_SLE : llvm::CmpInst::FCMP_OLE;
  }
  else if (op->getOp() == "<") {
    pred = int_flag ? llvm::ICmpInst::ICMP_SLT : llvm::CmpInst::FCMP_OLT;
  }
  else if (op->getOp() == ">") {
    pred = int_flag ? llvm::ICmpInst::ICMP_SGT : llvm::CmpInst::FCMP_OGT;
  }
  else {
    pred = int_flag ? llvm::ICmpInst::ICMP_SGE : llvm::CmpInst::FCMP_OGE;
  }
  val = llvm::CmpInst::Create(llvmOP, pred, left->GetValue(), right->GetValue(), "", bb);
}



CompoundExpr::CompoundExpr(Expr *l, Operator *o) 
  : Expr(Join(l->GetLocation(), o->GetLocation())) {
    Assert(l != NULL && o != NULL);
    (left=l)->SetParent(this);
    (op=o)->SetParent(this);
}

Decl *FieldAccess::GetDecl(){
  return ((VarExpr*)base)->GetDecl();
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
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}

void ArrayAccess::PrintChildren(int indentLevel) {
    base->Print(indentLevel+1);
    subscript->Print(indentLevel+1, "(subscript) ");
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

Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}

void LogicalExpr::Emit(){
//curr basic block
  llvm::BasicBlock *bb = irgen.GetBasicBlock();

  //sets the left and right sides
  left->Emit();
  right->Emit();

  if(op->getOp() == "&&"){ //logical and
    val = llvm::BinaryOperator::CreateAnd(
      left->GetValue(), right->GetValue(), "", bb);
  }
  else{ //and logical or
    val = llvm::BinaryOperator::CreateOr(
      left->GetValue(), right->GetValue(), "", bb);
  }
}



void Call::PrintChildren(int indentLevel) {
   if (base) base->Print(indentLevel+1);
   if (field) field->Print(indentLevel+1);
   if (actuals) actuals->PrintAll(indentLevel+1, "(actuals) ");
}

void ArithmeticExpr::Emit(){
  
  llvm::BasicBlock *bb = irgen.GetBasicBlock(); //current block

  if(!left){
    right->Emit();


    Decl* declare = ((VarExpr*)right)->GetDecl();

    bool int_flag = 
      right->GetValue()->getType() == irgen.GetIntType() ? true : false;
//Increment and decrement operations check
    if(op->getOp() == "++" || op->getOp() == "--"){
//different types of incs or decs for diff types
      float dec_amount = op->getOp() == "++" ? 1.0 : -1.0;

      int num_amount = op->getOp() == "++" ? 1 : -1;

      llvm::Constant *ir_i = llvm::ConstantInt::get(irgen.GetIntType(), num_amount);
      llvm::Constant *ir_f = llvm::ConstantFP::get(irgen.GetFloatType(), dec_amount);

      llvm::Constant *c = int_flag ? ir_i : ir_f;

      llvm::Value *store = llvm::BinaryOperator::CreateAdd(right->GetValue(), c, "", bb);
      if(declare) new llvm::StoreInst(store, declare->GetMem(), bb);

      val = store;
    }
    else{
      int int_neg = op->getOp() == "-" ? -1 : 1;
      float float_neg = op->getOp() == "-" ? -1.0 : 1.0;
      llvm::Constant *ir_f = llvm::ConstantFP::get(irgen.GetFloatType(), float_neg);


      llvm::Constant *ir_i = llvm::ConstantInt::get(irgen.GetIntType(), int_neg);

      llvm::Constant *cons = int_flag ? ir_i : ir_f;

      llvm::Value *store = llvm::BinaryOperator::CreateMul(right->GetValue(), cons, "", bb);
      if(declare) new llvm::StoreInst(store, declare->GetMem(), bb);

      val = store;
    }
    return;
  }
//checks if left and right hand sides are set
  left->Emit();
  right->Emit();

  bool vecScal = left->GetValue()->getType() != right->GetValue()->getType();
  if(op->getOp() == "+"){ //If add operation...
    if(vecScal){
      llvm::Value *add;
      if(left->GetValue()->getType() == irgen.GetFloatType()){
        add = llvm::ConstantVector::getSplat(right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
        val = llvm::BinaryOperator::CreateAdd(add, right->GetValue(), "", bb);
      }
      else{
        add = llvm::ConstantVector::getSplat(left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
        val = llvm::BinaryOperator::CreateAdd(left->GetValue(), add, "", bb);
      }
    }
    else{
	val = llvm::BinaryOperator::CreateAdd(left->GetValue(), right->GetValue(), "", bb);
    }
  }
  else if(op->getOp() == "/"){ //If divide operation...
    if(vecScal){
      llvm::Value *div;
      if(left->GetValue()->getType() == irgen.GetFloatType()){
        div = llvm::ConstantVector::getSplat(
          right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
        val = llvm::BinaryOperator::CreateFDiv(div, right->GetValue(), "", bb);
      }
      else{
        div = llvm::ConstantVector::getSplat(
          left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
        val = llvm::BinaryOperator::CreateFDiv(left->GetValue(), div, "", bb);
      }
    }
    else{
    val = llvm::BinaryOperator::CreateFDiv(left->GetValue(), right->GetValue(), "", bb);
    }
  }

  else if(op->getOp() == "-"){ // If subtraction operation
    if(vecScal){
      llvm::Value *min;
      if(left->GetValue()->getType() == irgen.GetFloatType()){
        min = llvm::ConstantVector::getSplat(right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
        val = llvm::BinaryOperator::CreateSub(min, right->GetValue(), "", bb);
      }
      else{
        min = llvm::ConstantVector::getSplat(left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
        val = llvm::BinaryOperator::CreateSub(left->GetValue(), min, "", bb);
      }
    }
    else{
    val = llvm::BinaryOperator::CreateSub(left->GetValue(), right->GetValue(), "", bb);
    }
  }
  else if(op->getOp() == "*"){ //If multiply operation
    if(vecScal){
      llvm::Value *mult;
      if(left->GetValue()->getType() == irgen.GetFloatType()){
        mult = llvm::ConstantVector::getSplat(right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
        val = llvm::BinaryOperator::CreateMul(mult, right->GetValue(), "", bb);
      }
      else{
        mult = llvm::ConstantVector::getSplat(left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
        val = llvm::BinaryOperator::CreateMul(left->GetValue(), mult, "", bb);
      }
    }
    else{
    val = llvm::BinaryOperator::CreateMul(left->GetValue(), right->GetValue(), "", bb);
    }
  }
  else{
    val = NULL;
  }
}

void AssignExpr::Emit(){

    llvm::BasicBlock *bb = irgen.GetBasicBlock();

    Decl* decl = left->GetDecl();
    right->Emit(); //Checks if right hand side is set

    llvm::Value *lftv = decl->GetMem();
    llvm::Value *ritv;
    std::string fa("FieldAccess");

    if(left->GetPrintNameForNode() == fa){
      left->Emit(); //Checks if left hand side is set
      llvm::Value* store;
      if(op->getOp() == "="){ // If equal op
        store = right->GetValue();
        ritv = ((FieldAccess*)left)->Store(store);
      }
      else if(op->getOp() == "+="){ //If increment assign op
        store = llvm::BinaryOperator::CreateAdd(left->GetValue(), right->GetValue(), "", bb);
        ritv = ((FieldAccess*)left)->Store(store);
      }
      else if(op->getOp() == "-="){ //If decrement assign op
        store = llvm::BinaryOperator::CreateSub(left->GetValue(), right->GetValue(), "", bb);
        ritv = ((FieldAccess*)left)->Store(store);
      }
      else if(op->getOp() == "*="){ //If multiply assign op
        store = llvm::BinaryOperator::CreateMul(left->GetValue(), right->GetValue(), "", bb);
        ritv = ((FieldAccess*)left)->Store(store);
      }
      else{
        store = llvm::BinaryOperator::CreateFDiv(
        left->GetValue(), right->GetValue(), "", bb);
        ritv = ((FieldAccess*)left)->Store(store);
      }

    }
    else{

      left->Emit();
      bool vecScal = left->GetValue()->getType() != right->GetValue()->getType();

      if(op->getOp() == "="){ //equals then right is just that value
        ritv = right->GetValue();
      }
      else if(op->getOp() == "+="){ 
      //performs adding and then assigning
        if(vecScal){
          llvm::Value *w;
          if(left->GetValue()->getType() == irgen.GetFloatType()){
            w = llvm::ConstantVector::getSplat(
              right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
            ritv = llvm::BinaryOperator::CreateAdd(w, right->GetValue(), "", bb);
          }
          else{
            w = llvm::ConstantVector::getSplat(
              left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
            ritv = llvm::BinaryOperator::CreateAdd(left->GetValue(), w, "", bb);
          }

        }
        else{
          ritv = llvm::BinaryOperator::CreateAdd(left->GetValue(), right->GetValue(), "", bb);
        }
      }
      else if(op->getOp() == "-="){
//Performs subtraction and assignment
        if(vecScal){
          llvm::Value *w;
          if(left->GetValue()->getType() == irgen.GetFloatType()){
            w = llvm::ConstantVector::getSplat(right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
            ritv = llvm::BinaryOperator::CreateSub(w, right->GetValue(), "", bb);
          }
          else{
            w = llvm::ConstantVector::getSplat(left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
            ritv = llvm::BinaryOperator::CreateSub(left->GetValue(), w, "", bb);
          }

        }
        else{
        ritv = llvm::BinaryOperator::CreateSub(left->GetValue(), right->GetValue(), "", bb);
        }
      }
      else if(op->getOp() == "*="){ // multiply and assign
        if(vecScal){
          llvm::Value *w;
          if(left->GetValue()->getType() == irgen.GetFloatType()){
            w = llvm::ConstantVector::getSplat(
              right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
            ritv = llvm::BinaryOperator::CreateMul(w, right->GetValue(), "", bb);
          }
          else{
            w = llvm::ConstantVector::getSplat(left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
            ritv = llvm::BinaryOperator::CreateMul(left->GetValue(), w, "", bb);
          }

        }
        else{
        ritv = llvm::BinaryOperator::CreateMul(left->GetValue(), right->GetValue(), "", bb);
	}
      }
      else{ // division and assign
        if(vecScal){
          llvm::Value *w;
          if(left->GetValue()->getType() == irgen.GetFloatType()){
            w = llvm::ConstantVector::getSplat(right->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(left->GetValue()));
            ritv = llvm::BinaryOperator::CreateFDiv(w, right->GetValue(), "", bb);
          }
          else{
            w = llvm::ConstantVector::getSplat(
              left->GetValue()->getType()->getVectorNumElements(), (llvm::Constant*)(right->GetValue()));
            ritv = llvm::BinaryOperator::CreateFDiv(left->GetValue(), w, "", bb);
          }

        }
        else
        ritv = llvm::BinaryOperator::CreateFDiv(left->GetValue(), right->GetValue(), "", bb);
      }


      new llvm::StoreInst(ritv, lftv, bb);    
    }
    val = ritv;

}



void EqualityExpr::Emit(){
  llvm::BasicBlock *bb = irgen.GetBasicBlock(); //grab current basic block
//sets left and right hand sides
  left->Emit();
  right->Emit();
  llvm::Type *t = left->GetValue()->getType();
  bool int_flag = (t == irgen.GetIntType() || t == irgen.GetBoolType()) ? true : false;

  llvm::CmpInst::OtherOps llvmOP =
    int_flag ? llvm::CmpInst::ICmp : llvm::CmpInst::FCmp;

  llvm::CmpInst::Predicate pred;
  if (op->getOp() == "!=") {
    pred = int_flag ? llvm::ICmpInst::ICMP_NE : llvm::CmpInst::FCMP_ONE;
  }
  else {
    pred = int_flag ? llvm::ICmpInst::ICMP_EQ : llvm::CmpInst::FCMP_OEQ;
  }

  llvm::Value *val_u = llvm::CmpInst::Create(llvmOP, pred, left->GetValue(), right->GetValue(), "", bb);

  if (val_u->getType() == irgen.GetBoolType()) val = val_u;
  else{
    llvm::Value *id0 = llvm::ConstantInt::get(irgen.GetIntType(), 0, false);
    llvm::Value *andResult = llvm::ExtractElementInst::Create(val_u, id0, "", bb);
    for(unsigned int i = 1; i < val_u->getType()->getVectorNumElements(); ++i){
      llvm::Value *id = llvm::ConstantInt::get(irgen.GetIntType(), i, false);
      llvm::Value *next = llvm::ExtractElementInst::Create(val_u, id, "", bb);
      andResult = llvm::BinaryOperator::CreateAnd(andResult, next, "", bb);
    }
    val = op->getOp() == "==" ? andResult : llvm::BinaryOperator::CreateNot(andResult, "", bb);
  }
  

}


void PostfixExpr::Emit(){

  //current basic block/env
  llvm::BasicBlock *bb = irgen.GetBasicBlock();
//sets left
  left->Emit();

  Decl* declare = ((VarExpr*)left)->GetDecl();
  float dec_amount = op->getOp() == "++" ? 1.0 : -1.0;
  bool int_flag = 
      left->GetValue()->getType() == irgen.GetIntType() ? true : false;

  int num_amount = op->getOp() == "++" ? 1 : -1;

  llvm::Constant *i = llvm::ConstantInt::get(irgen.GetIntType(), num_amount);


  llvm::Constant *flt = llvm::ConstantFP::get(irgen.GetFloatType(), dec_amount);
  llvm::Constant *cons = int_flag ? i : flt;  //Check flag retrive correct val

  llvm::Value *store = llvm::BinaryOperator::CreateAdd(left->GetValue(), cons, "", bb);
  new llvm::StoreInst(store, declare->GetMem(), bb);

  val = left->GetValue();

}

void FieldAccess::Emit(){
  llvm::BasicBlock *bb = irgen.GetBasicBlock();

  base->Emit();

  llvm::Value *baseAddr = base->GetValue();

  List<int> *swizzle = new List<int>;
  unsigned int e = 0;
  while( e < field->getName().length()){
    e++;
    if(field->getName()[e] == 'x') 
	swizzle->Append(0);
    else if(field->getName()[e] == 'y') 
	swizzle->Append(1);
    else if(field->getName()[e] == 'z')
	 swizzle->Append(2);
    else 
	swizzle->Append(3);
  }
//check the indexes specified and do appropriate stuff
   if(swizzle->NumElements() == 1){
    llvm::Constant *Lidx = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(0));
    val = llvm::ExtractElementInst::Create(baseAddr, Lidx, "", bb);
  }
  else{
    llvm::Constant *zeros = llvm::ConstantFP::get(irgen.GetFloatType(), 1.0);
    val = llvm::ConstantVector::getSplat(swizzle->NumElements(), zeros);
    int r = 0;
    while(r < swizzle->NumElements()){
      llvm::Constant *idxa = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(e));
      llvm::Constant *idxb = llvm::ConstantInt::get(irgen.GetIntType(), e);
      llvm::Value *param = llvm::ExtractElementInst::Create(baseAddr, idxa, "", bb);
      val = llvm::InsertElementInst::Create(val, param, idxb, "", bb);
      r++;
    }
  }

}

llvm::Value *FieldAccess::Store(llvm::Value* rval){

  //grab the basic block
  llvm::BasicBlock *bb = irgen.GetBasicBlock();

  //use the baseaddr to access the vector
  llvm::Value *addr = this->GetDecl()->GetMem();
  llvm::Value *bAddr = new llvm::LoadInst(addr, "", bb);

  //retrieve indices 
  List<int> *swizzle = new List<int>;
  for(unsigned int t = 0; t < field->getName().length(); t++){
    if(field->getName()[t] == 'x') swizzle->Append(0);
    else if(field->getName()[t] == 'y') swizzle->Append(1);
    else if(field->getName()[t] == 'z') swizzle->Append(2);
    else swizzle->Append(3);
  }

  if(swizzle->NumElements() == 1){
    llvm::Constant *Lidx = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(0));
    
    bAddr = llvm::InsertElementInst::Create(bAddr, rval, Lidx, "", bb);

    new llvm::StoreInst(bAddr, addr, bb);
    return rval;

  }
  else if(swizzle->NumElements() == 2){
    
    llvm::Constant *Lid1 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(0));
    llvm::Constant *Lid2 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(1));
    llvm::Constant *zero = llvm::ConstantInt::get(irgen.GetIntType(), 0);
    llvm::Constant *one = llvm::ConstantInt::get(irgen.GetIntType(), 1);

    llvm::Value *mth = llvm::ExtractElementInst::Create(rval, zero, "", bb);
    llvm::Value *nth = llvm::ExtractElementInst::Create(rval, one, "", bb);
      
    bAddr = llvm::InsertElementInst::Create(bAddr, mth, Lid1, "", bb);
    bAddr = llvm::InsertElementInst::Create(bAddr, nth, Lid2, "", bb);

  }
  else if(swizzle->NumElements() == 3){
    llvm::Constant *Lid1 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(0));
    llvm::Constant *Lid2 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(1));
    llvm::Constant *Lid3 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(2));
    llvm::Constant *zero = llvm::ConstantInt::get(irgen.GetIntType(), 0);
    llvm::Constant *one = llvm::ConstantInt::get(irgen.GetIntType(), 1);
    llvm::Constant *two = llvm::ConstantInt::get(irgen.GetIntType(), 2);
    llvm::Value *mth = llvm::ExtractElementInst::Create(rval, zero, "", bb);
    llvm::Value *nth = llvm::ExtractElementInst::Create(rval, one, "", bb);
    llvm::Value *oth = llvm::ExtractElementInst::Create(rval, two, "", bb);  

    bAddr = llvm::InsertElementInst::Create(bAddr, mth, Lid1, "", bb);
    bAddr = llvm::InsertElementInst::Create(bAddr, nth, Lid2, "", bb);
    bAddr = llvm::InsertElementInst::Create(bAddr, oth, Lid3, "", bb);
    
  }
  else{//indices
    llvm::Constant *Lid1 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(0));
    llvm::Constant *Lid2 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(1));
    llvm::Constant *Lid3 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(2));
    llvm::Constant *Lid4 = llvm::ConstantInt::get(irgen.GetIntType(), swizzle->Nth(3));
    llvm::Constant *zero = llvm::ConstantInt::get(irgen.GetIntType(), 0);
    llvm::Constant *one = llvm::ConstantInt::get(irgen.GetIntType(), 1);
    llvm::Constant *two = llvm::ConstantInt::get(irgen.GetIntType(), 2);
    llvm::Constant *three = llvm::ConstantInt::get(irgen.GetIntType(), 3);

    llvm::Value *mth = llvm::ExtractElementInst::Create(rval, zero, "", bb); //right hand values
    llvm::Value *nth = llvm::ExtractElementInst::Create(rval, one, "", bb);
    llvm::Value *oth = llvm::ExtractElementInst::Create(rval, two, "", bb); 
    llvm::Value *pth = llvm::ExtractElementInst::Create(rval, three, "", bb); 

    bAddr = llvm::InsertElementInst::Create(bAddr, mth, Lid1, "", bb); //putting right hand stuff to left
    bAddr = llvm::InsertElementInst::Create(bAddr, nth, Lid2, "", bb);
    bAddr = llvm::InsertElementInst::Create(bAddr, oth, Lid3, "", bb);
    bAddr = llvm::InsertElementInst::Create(bAddr, pth, Lid4, "", bb);
  }

  new llvm::StoreInst(bAddr, addr, bb);
  return bAddr;
}


