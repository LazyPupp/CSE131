/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include <iostream>
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "symtable.h"        
         


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
  
void VarDecl::PrintChildren(int indentLevel) { 
   if (typeq) typeq->Print(indentLevel+1);
   if (type) type->Print(indentLevel+1);
   if (id) id->Print(indentLevel+1);
   if (assignTo) assignTo->Print(indentLevel+1, "(initializer) ");
}


void VarDecl::Emit(){
   //Grabs the basic block
    llvm::BasicBlock *bb = irgen.GetBasicBlock();

   //Grabs type of vardecl
    llvm::Type *ty = this->llvmType(type);

    if(globalScope){  //Globalscope bool flag
        mem = new llvm::GlobalVariable(*mod, ty, false, 
            llvm::GlobalValue::ExternalLinkage,
            llvm::Constant::getNullValue(ty), this->getName());
    }
    else{
        mem = new llvm::AllocaInst(ty, this->getName(), bb);
    }
//inserts into symbol table
    std::map<string, Decl*> *currTable = tables->Nth(0);
//retrives identifier string
    std::string theName = this->getName();
    (*currTable)[theName] = this;

} 
/*
 * Helper method to grab the types
 *
 */
llvm::Type *Decl::llvmType(Type *type){
    llvm::Type *typ;
    if(type == Type::intType){ //Checks all the possible types
        typ = irgen.GetIntType();
    }
    else if(type == Type::floatType){
        typ = irgen.GetFloatType();
    }
    else if(type == Type::boolType){
        typ = irgen.GetBoolType();
    }
    else if(type == Type::vec4Type){
        typ = irgen.GetVec4Type();
    }
    else if(type == Type::mat2Type){
        typ = irgen.GetMat2Type();
    }
    else if(type == Type::voidType){
        typ = irgen.GetVoidType();
    }
    else if(type == Type::vec2Type){
        typ = irgen.GetVec2Type();
    }
    else if(type == Type::vec3Type){
        typ = irgen.GetVec3Type();
    }
    else{
        typ = irgen.GetIntType();
    }

    return typ;
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

void FnDecl::Emit(){

    //environment of the decl
    llvm::LLVMContext *context = irgen.GetContext();
    std::map<string, Decl*> *currTable = tables->Nth(0);

    //inside function means no globalscope
    globalScope = false;

    //new scope
    std::map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable,0);
    llvm::Type *retType = this->llvmType(returnType);

    //function params
    std::vector<llvm::Type *> argTypes;
    int z = 0;
    while(z < formals->NumElements() ){
        VarDecl *vdecl = formals->Nth(z);
        argTypes.push_back(this->llvmType(vdecl->GetType()));
        z++;
    }
    llvm::ArrayRef<llvm::Type *> argArray(argTypes);
//Creating new function signature
    llvm::FunctionType *funcTy = llvm::FunctionType::get(retType, argArray, false);
    llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction(this->getName(), funcTy));
    
    irgen.SetFunction(f);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*context, "entry", irgen.GetFunction());
//sets basic block
    irgen.SetBasicBlock(bb);

    //function params now into symbol table
    llvm::Function::arg_iterator arg = f->arg_begin();
    int p = 0;
    while( p < formals->NumElements()){
//setting the values for formals
        VarDecl *vee = formals->Nth(p);
        vee->Emit();

        //put current value into table and memory
        (*currTable)[vee->getName()] = (Decl*)vee;
        new llvm::StoreInst(arg, vee->GetMem(), bb);
        arg++; // increment args
        p++;
    }

    if(body){ // goes through the body
        for(int i = 0; i < ((StmtBlock*)body)->getStmts()->NumElements();i++){
            ((StmtBlock*)body)->getStmts()->Nth(i)->Emit();
        }
        if(!irgen.GetBasicBlock()->getTerminator() && retType == irgen.GetVoidType())
            llvm::ReturnInst::Create(*context, irgen.GetBasicBlock());
        else if(!irgen.GetBasicBlock()->getTerminator()) 
            new llvm::UnreachableInst(*context, irgen.GetBasicBlock());
    }

    std::string theName = this->getName();


    (*currTable)[theName] = this;
//leaves function and now in global scope again
    globalScope = true;

}

void FnDecl::PrintChildren(int indentLevel) {
    if (returnType) returnType->Print(indentLevel+1, "(return type) ");
    if (id) id->Print(indentLevel+1);
    if (formals) formals->PrintAll(indentLevel+1, "(formals) ");
    if (body) body->Print(indentLevel+1, "(body) ");
}

