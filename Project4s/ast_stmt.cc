/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "symtable.h"

#include "irgen.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/raw_ostream.h"                                                   

IRGenerator Node::irgen;
llvm::Module *Node::mod = Node::irgen.GetOrCreateModule("foo.bc");
llvm::Module::global_iterator Node::globs = mod->global_begin();

bool globalScope = true;
bool returnAFound = false;
bool returnBFound = false;

int switchrets = 0;

List <std::map<string, Decl*>*> *tables = new List<std::map<string, Decl*>*>();

List<llvm::BasicBlock*> *feet = new List<llvm::BasicBlock*>;
List<llvm::BasicBlock*> *heads = new List<llvm::BasicBlock*>;


Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    printf("\n");
}

void Program::Emit() {
    // TODO:
    // This is just a reference for you to get started
    //
    // You can use this as a template and create Emit() function
    // for individual node to fill in the module structure and instructions.
    //
    /*
    IRGenerator irgen;
    llvm::Module *mod = irgen.GetOrCreateModule("Name_the_Module.bc");

    // create a function signature
    std::vector<llvm::Type *> argTypes;
    llvm::Type *intTy = irgen.GetIntType();
    argTypes.push_back(intTy);
    llvm::ArrayRef<llvm::Type *> argArray(argTypes);
    llvm::FunctionType *funcTy = llvm::FunctionType::get(intTy, argArray, false);

    // llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("foo", intTy, intTy, (Type *)0));
    llvm::Function *f = llvm::cast<llvm::Function>(mod->getOrInsertFunction("Name_the_function", funcTy));
    llvm::Argument *arg = f->arg_begin();
    arg->setName("x");

    // insert a block into the runction
    llvm::LLVMContext *context = irgen.GetContext();
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*context, "entry", f);

    // create a return instruction
    llvm::Value *val = llvm::ConstantInt::get(intTy, 1);
    llvm::Value *sum = llvm::BinaryOperator::CreateAdd(arg, val, "", bb);
    llvm::ReturnInst::Create(*context, sum, bb);*/

    map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable, 0);

    for(int i = 0; i < decls->NumElements(); i++){
        decls->Nth(i)->Emit();
    }

    // write the BC into standard output
    llvm::WriteBitcodeToFile(mod, llvm::outs());
}

StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}

void StmtBlock::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    stmts->PrintAll(indentLevel+1);
}

void StmtBlock::Emit(){
    bool flag = true;
    map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable,0); 
    int b = 0;
    while( b < decls->NumElements()){
        decls->Nth(b)->Emit();
        b++;
    }
    int z = 0;
    while( z < stmts->NumElements()){
        if(!irgen.GetBasicBlock()->getTerminator()){ 
	   stmts->Nth(z)->Emit();
	}
	z++;
    }

    tables->RemoveAt(0);
}

DeclStmt::DeclStmt(Decl *d) {
    Assert(d != NULL);
    (decl=d)->SetParent(this);
}

void DeclStmt::PrintChildren(int indentLevel) {
    decl->Print(indentLevel+1);
}

void DeclStmt::Emit(){
    decl->Emit();
}

ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) { 
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this); 
    (body=b)->SetParent(this);
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) { 
    Assert(i != NULL && t != NULL && b != NULL);
    (init=i)->SetParent(this);
    step = s;
    if ( s )
      (step=s)->SetParent(this);
}

void ForStmt::PrintChildren(int indentLevel) {
    init->Print(indentLevel+1, "(init) ");
    test->Print(indentLevel+1, "(test) ");
    if ( step )
      step->Print(indentLevel+1, "(step) ");
    body->Print(indentLevel+1, "(body) ");
}

void ForStmt::Emit(){

    //grab the local environment
    llvm::LLVMContext *context = irgen.GetContext();
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::Function *f = irgen.GetFunction();

    //basic blocks for your header, body, etc
    llvm::BasicBlock *sBB = llvm::BasicBlock::Create(*context, "forstepBB", f);
    llvm::BasicBlock *fBB = llvm::BasicBlock::Create(*context, "forfootBB", f);
    llvm::BasicBlock *hBB = llvm::BasicBlock::Create(*context, "forheadBB", f);
    llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*context, "forbodyBB", f);
    
    heads->InsertAt(sBB,0);
    feet->InsertAt(fBB,0);

    init->Emit(); //bb for entry code
    llvm::BranchInst::Create(hBB, bb);

    irgen.SetBasicBlock(hBB); // bb for header
    test->Emit();
    llvm::BranchInst::Create(bBB, fBB, test->GetValue(), hBB);

    irgen.SetBasicBlock(bBB); //bb for body
    body->Emit();
    if(!irgen.GetBasicBlock()->getTerminator()) 
        llvm::BranchInst::Create(sBB, irgen.GetBasicBlock());

    sBB->moveAfter(irgen.GetBasicBlock()); //step bb
    irgen.SetBasicBlock(sBB);
    step->Emit();
    llvm::BranchInst::Create(hBB, sBB);

    fBB->moveAfter(irgen.GetBasicBlock()); //footer bb
    irgen.SetBasicBlock(fBB);

    heads->RemoveAt(0);
    feet->RemoveAt(0);

}

void WhileStmt::PrintChildren(int indentLevel) {
    test->Print(indentLevel+1, "(test) ");
    body->Print(indentLevel+1, "(body) ");
}

void WhileStmt::Emit(){
    bool flag = false;
    int z = 0;
    llvm::LLVMContext *context = irgen.GetContext(); //Grabs the local environment
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::Function *f = irgen.GetFunction();

    llvm::BasicBlock *hBB = llvm::BasicBlock::Create(*context, "whileheadBB", f); //bb header
    llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*context, "whilebodyBB", f); //bb body
    llvm::BasicBlock *fBB = llvm::BasicBlock::Create(*context, "whilefootBB", f); //bb footer
    
    heads->InsertAt(hBB,0);
    feet->InsertAt(fBB,0);

    llvm::BranchInst::Create(hBB, bb); //bb for starter code
    irgen.SetBasicBlock(hBB); //bb header stuff
    test->Emit();
    llvm::BranchInst::Create(bBB, fBB, test->GetValue(), hBB);

    irgen.SetBasicBlock(bBB); //bb body
    body->Emit();
    if(!irgen.GetBasicBlock()->getTerminator()) { 
        llvm::BranchInst::Create(hBB, irgen.GetBasicBlock());
    }
    fBB->moveAfter(irgen.GetBasicBlock()); //footer bb
    irgen.SetBasicBlock(fBB);

    heads->RemoveAt(0);
    feet->RemoveAt(0);
    
}

IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) { 
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}

void IfStmt::PrintChildren(int indentLevel) {
    if (test) test->Print(indentLevel+1, "(test) ");
    if (body) body->Print(indentLevel+1, "(then) ");
    if (elseBody) elseBody->Print(indentLevel+1, "(else) ");
}

void IfStmt::Emit(){
    //environment
   // bool flag = true;
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::LLVMContext *context = irgen.GetContext();
    llvm::Function *f = irgen.GetFunction();

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*context, "ifthenBB", f); //then bb
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*context, "ifelseBB", f); //else bb
    llvm::BasicBlock *footBB = llvm::BasicBlock::Create(*context, "iffootBB", f); //footer bb

    test->Emit();
    llvm::BranchInst::Create(thenBB, elseBody ? elseBB : footBB, test->GetValue(), bb); //establish bb

    irgen.SetBasicBlock(thenBB); //handles bb block
    body->Emit();
    if(!irgen.GetBasicBlock()->getTerminator()) 
        llvm::BranchInst::Create(footBB, irgen.GetBasicBlock());

    elseBB->moveAfter(irgen.GetBasicBlock()); //else bb
    irgen.SetBasicBlock(elseBB);
    if(elseBody)
	 elseBody->Emit();
    if(!irgen.GetBasicBlock()->getTerminator())
        llvm::BranchInst::Create(footBB, irgen.GetBasicBlock());

    footBB->moveAfter(irgen.GetBasicBlock()); //footer bb
    irgen.SetBasicBlock(footBB);

}

void BreakStmt::Emit(){
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::BranchInst::Create(feet->Nth(0), bb);   
}

void ContinueStmt::Emit(){
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::BranchInst::Create(heads->Nth(0), bb);

}

ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    expr = e;
    if (e != NULL) expr->SetParent(this);
}

void ReturnStmt::PrintChildren(int indentLevel) {
    if ( expr ) 
      expr->Print(indentLevel+1);
}

void ReturnStmt::Emit(){

    llvm::LLVMContext *cont = irgen.GetContext();
    llvm::BasicBlock *bb = irgen.GetBasicBlock();

    if(expr){
        expr->Emit();
        llvm::Value *ret = expr->GetValue();
        llvm::ReturnInst::Create(*cont, ret, bb);
    }
    else{
        llvm::ReturnInst::Create(*cont, bb); 
    }
    
}

SwitchLabel::SwitchLabel(Expr *l, Stmt *s) {
    Assert(l != NULL && s != NULL);
    (label=l)->SetParent(this);
    (stmt=s)->SetParent(this);
}

SwitchLabel::SwitchLabel(Stmt *s) {
    Assert(s != NULL);
    label = NULL;
    (stmt=s)->SetParent(this);
}

void SwitchLabel::PrintChildren(int indentLevel) {
    if (label) label->Print(indentLevel+1);
    if (stmt)  stmt->Print(indentLevel+1);
}

SwitchStmt::SwitchStmt(Expr *e, List<Stmt *> *c, Default *d) {
    Assert(e != NULL && c != NULL && c->NumElements() != 0 );
    (expr=e)->SetParent(this);
    (cases=c)->SetParentAll(this);
    def = d;
    if (def) def->SetParent(this);
}

void SwitchStmt::PrintChildren(int indentLevel) {
    if (expr) expr->Print(indentLevel+1);
    if (cases) cases->PrintAll(indentLevel+1);
    if (def) def->Print(indentLevel+1);
}

void Case::Emit(){

    std::string r("ReturnStmt");
    if(stmt->GetPrintNameForNode() == r) 
	switchrets++;
    label->Emit();
    stmt->Emit();

}

void Default::Emit(){

    std::string r("ReturnStmt");
    if(stmt->GetPrintNameForNode() == r) switchrets++;

    stmt->Emit();

}

void SwitchStmt::Emit(){

    switchrets = 0; //reset switchrets
    llvm::LLVMContext *context = irgen.GetContext(); //env
    llvm::BasicBlock *bb = irgen.GetBasicBlock();
    llvm::Function *f = irgen.GetFunction();

    llvm::BasicBlock *fBB = llvm::BasicBlock::Create(*context, "footBB", f); //footer bb
    feet->InsertAt(fBB,0);
    //bool flag;
    bool foundDefault = false;
    List<llvm::BasicBlock*> *caseBBs = new List<llvm::BasicBlock*>; //handles list
    List<int> *labels = new List<int>;
    int num = 0;
    while( num < cases->NumElements()){
        
        Stmt* stmt = cases->Nth(num);
        std::string cas("Case");
        std::string def("Default");
        std::string ret("ReturnStmt");
        std::string str(stmt->GetPrintNameForNode());

        if(str == cas || str == def){
            llvm::BasicBlock *cBB = llvm::BasicBlock::Create(*context,str == def ? "defaultBB" : "caseBB", f);
            caseBBs->Append(cBB);
            if(str == cas){
                Case *st = (Case*)stmt;
                IntConstant* i = (IntConstant*)(st->getLabel());
                labels->Append(i->getNum());
            } 
            if(str == def) 
		foundDefault = true;
            irgen.SetBasicBlock(cBB);
        }
        else{
            if(str == ret) switchrets++;
        }
        stmt->Emit();
	num++;
    }
//couldn't find default statement
    if(!foundDefault){
        caseBBs->Append(llvm::BasicBlock::Create(*context, "defaultBB", f));
    }

    irgen.SetBasicBlock(bb);
    expr->Emit();
    llvm::BasicBlock *defaultBB = caseBBs->Nth(caseBBs->NumElements()-1);
    fBB->moveAfter(defaultBB);

    llvm::SwitchInst *si = llvm::SwitchInst::Create(expr->GetValue(), defaultBB, 
        caseBBs->NumElements()-1, bb);
  //bool found = true;
    int k = 0;
    while(k < caseBBs->NumElements()){

        llvm::BasicBlock *currBB = caseBBs->Nth(k);
        llvm::BasicBlock *nextBB = fBB;

        irgen.SetBasicBlock(currBB);
        if(k < caseBBs->NumElements()-1){
            nextBB = caseBBs->Nth(k+1);
            int n = labels->Nth(k);
            si->addCase(llvm::ConstantInt::get(*context, 
                llvm::APInt(32, n, false)), currBB);
        }

        if(!currBB->getTerminator()) 
		llvm::BranchInst::Create(nextBB, currBB);

	k++;
    }

    irgen.SetBasicBlock(fBB);

    if(switchrets == caseBBs->NumElements()) 
        new llvm::UnreachableInst(*context, fBB);

    feet->RemoveAt(0);

}

