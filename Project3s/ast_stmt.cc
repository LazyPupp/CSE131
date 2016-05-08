/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "errors.h"

#include <map>
#include <string>
using std::map;
using std::string;

#include <typeinfo>

List< std::map<string, Decl*>*> *tables = new List< std::map<string, Decl*>*>();
bool escapeAllowed = false;
bool retFound = false;
Type *currRetType = NULL;

Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::PrintChildren(int indentLevel) {
    decls->PrintAll(indentLevel+1);
    printf("\n");
}

void Program::Check() {
    /* pp3: here is where the semantic analyzer is kicked off.
     *      The general idea is perform a tree traversal of the
     *      entire program, examining all constructs for compliance
     *      with the semantic rules.  Each node can have its own way of
     *      checking itself, which makes for a great use of inheritance
     *      and polymorphism in the node classes.
     */

    // sample test - not the actual working code
    // replace it with your own implementation
    // if ( decls->NumElements() >= 2 ) {
    //      Decl *newDecl  = decls->Nth(1);
    //      Decl *prevDecl = decls->Nth(0);
    //      ReportError::DeclConflict(newDecl, prevDecl);
    // }

    //List of symbol tables
    //List< map<string, Decl*>* > symbolTables;

    //Initial symbol table
    map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable, 0);

    //Check for declaration conflicts
    for(int i = 0; i < decls->NumElements(); i++)
    {
        decls->Nth(i)->Check();
    }


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

void StmtBlock::Check() {

    //Initial symbol table
    //map<string, Decl*> *symbolTable = tables->Nth(0);
    map<string, Decl*> *symbolTable = new map<string, Decl*>;
    tables->InsertAt(symbolTable,0);

    //Check for declaration conflicts
    if(decls){
        for(int i = 0; i < decls->NumElements(); i++){
            decls->Nth(i)->Check();
        }
    }

    //Check for statement conflicts in body
    if(stmts)
    {
        std::cout << stmts->NumElements() << std::endl;

        std::string brk ("BreakStmt");
        std::string cont ("ContinueStmt");
        std::string ret ("ReturnStmt");
        std::string declstmt ("DeclStmt");
        std::string stmtblock ("StmtBlock");

        for(int i = 0; i < stmts->NumElements(); i++){    

            // std::string s (stmts->Nth(i)->GetPrintNameForNode());
            // yyltype *l = stmts->Nth(i)->GetLocation();

            // //Check for incorrect use of break and continue
            // if(s.compare(brk) == 0 && !escapeAllowed){
            //     ReportError::BreakOutsideLoop(new BreakStmt(*l));
            // }
            // else if(s.compare(cont) == 0 && !escapeAllowed){
            //     ReportError::ContinueOutsideLoop(new ContinueStmt(*l));
            // }
            // else if(s.compare(ret) == 0){
            //     stmts->Nth(i)->Check();
            //     retFound = true;
            // }
            // else if(s.compare(declstmt) == 0){
            //     Decl* dec = stmts->Nth(i)->getDec();
            //     dec->Check();
            // }
            // else if(s.compare(stmtblock) == 0){
            //     tables->InsertAt(new map<string, Decl*>,0);
            //     stmts->Nth(i)->Check();    
            // }
            // else{
                stmts->Nth(i)->Check();
            //}   

        }
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

void DeclStmt::Check(){
    decl->Check();
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

void ForStmt::Check(){

    escapeAllowed = true;

    init->Check();
    test->Check();

    if(test->retType != Type::boolType 
        && test->retType != Type::errorType){
        ReportError::TestNotBoolean(test);
    }

    step->Check();

    //body->Check();
    if(body){
        //body->Check();
        for(int i = 0; i < ((StmtBlock*)body)->getStmts()->NumElements(); i++){
            ((StmtBlock*)body)->getStmts()->Nth(i)->Check();
        }
    }

    escapeAllowed = false;
}

void WhileStmt::PrintChildren(int indentLevel) {
    test->Print(indentLevel+1, "(test) ");
    body->Print(indentLevel+1, "(body) ");
}

void WhileStmt::Check(){

    escapeAllowed = true;

    if(test) test->Check();

    if(test->retType != Type::boolType){
        ReportError::TestNotBoolean(test);
    }

    //if(body) body->Check();
    if(body){
        //body->Check();
        for(int i = 0; i < ((StmtBlock*)body)->getStmts()->NumElements(); i++){
            ((StmtBlock*)body)->getStmts()->Nth(i)->Check();
        }
    }

    escapeAllowed = false;
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

void IfStmt::Check(){
    if(test){
        test->Check();

        if(test->retType != Type::boolType 
            && test->retType != Type::errorType){
             ReportError::TestNotBoolean(test);
        }
    }

    if(body){
        //body->Check();
        for(int i = 0; i < ((StmtBlock*)body)->getStmts()->NumElements(); i++){
            ((StmtBlock*)body)->getStmts()->Nth(i)->Check();
        }
    }

    if(elseBody){
        //elseBody->Check();
        for(int i = 0; i < ((StmtBlock*)elseBody)->getStmts()->NumElements(); i++){
            ((StmtBlock*)elseBody)->getStmts()->Nth(i)->Check();
        }
    } 

    
}

void BreakStmt::Check(){
    if(!escapeAllowed){
        ReportError::BreakOutsideLoop(this);
    }
}

void ContinueStmt::Check(){
    if(!escapeAllowed){
        ReportError::ContinueOutsideLoop(this);
    }
}


ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    expr = e;
    if (e != NULL) expr->SetParent(this);
}

void ReturnStmt::PrintChildren(int indentLevel) {
    if ( expr ) 
      expr->Print(indentLevel+1);
}

void ReturnStmt::Check(){

    if(expr){
        expr->Check();
        if((expr->retType != currRetType) && (expr->retType != Type::errorType))	{
            ReportError::ReturnMismatch(this, expr->retType, currRetType );
        }
    }
    else{
        if(currRetType != Type::voidType){
            ReportError::ReturnMismatch(this, Type::voidType, currRetType );
        }
    }
    retFound = true;
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

void Case::Check() {

    std::string stmtblock("StmtBlock");
    std::string s;

    if(label) label->Check();
    if(stmt){
        s = stmt->GetPrintNameForNode();
        if(s.compare(stmtblock) == 0){
            for(int i = 0; i < ((StmtBlock*)stmt)->getStmts()->NumElements(); i++){
            ((StmtBlock*)stmt)->getStmts()->Nth(i)->Check();
        }
        }
        else{
            stmt->Check();
        }
    }
}

void Default::Check() {
    std::string stmtblock("StmtBlock");
    std::string s;

    if(stmt){
        s = stmt->GetPrintNameForNode();
        if(s.compare(stmtblock) == 0){
            for(int i = 0; i < ((StmtBlock*)stmt)->getStmts()->NumElements(); i++){
            ((StmtBlock*)stmt)->getStmts()->Nth(i)->Check();
        }
        }
        else{
            stmt->Check();
        }
    }
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

void SwitchStmt::Check(){
    escapeAllowed = true;

    if(expr) expr->Check();

    for(int i = 0; i < cases->NumElements(); i++){
        cases->Nth(i)->Check();
    }

    if(def) def->Check();

    escapeAllowed = false;
}



