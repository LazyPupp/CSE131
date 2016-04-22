/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include <string.h>
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine
/*
struct types{
   TypeQualifer *typequals;
   Type		*typespec;
}
*/
%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *decllist;
    char *charConstant;
    double doubleConstant;
    
    List<Expr*> *exprlist;

    List<Stmt*> *stmtlist;
    List<SwitchStmt*> *switchstmtlist;

    ExprError *exprerror;
    EmptyExpr *emptyexpr;
    VarExpr *varexpr;
    IntConstant *intconstant;
    FloatConstant *floatconstant;

    Node *node;
    Identifier *ident;
    Error *error;
    VarDecl *vardecl;
    VarDeclError *vardeclerror;
    FnDecl *fndecl;
    FormalsError *formalserror;
    Expr *expr;
    BoolConstant *boolconstant;
    Operator *oper;
    CompoundExpr *compoundexpr;
    ArithmeticExpr *arithmeticexpr;
    RelationalExpr *relationalexpr;
    EqualityExpr *equalityexpr;
    LogicalExpr *logicalexpr;
    AssignExpr *assignexpr;
    PostfixExpr *postfixexpr;
    LValue *lvalue;
    ArrayAccess *arrayaccess;
    FieldAccess *fieldaccess;
    Call *call;
    ActualsError *actualserror;
    Stmt *stmt;
    StmtBlock *stmtblock;
    ConditionalStmt *conditionalstmt;
    LoopStmt *loopstmt;
    ForStmt *forstmt;
    WhileStmt *whilestmt;
    IfStmt *ifstmt;
    IfStmtExprError *ifstmtexprerror;
    BreakStmt *breakstmt;
    ReturnStmt *returnstmt;
    SwitchLabel *switchlabel;
    Case *cas;
    Default *defaul;
    SwitchStmt *switchstmt;
    SwitchStmtError *switchstmterror;
    Type *type;
    NamedType *namedtype;
    ArrayType *arraytype;
    TypeQualifier *typequal;

//    struct types typer;

}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant

/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
 /*
%type <declList>  DeclList
%type <decl>      Decl
*/

%type <expr>          variable_identifier
%type <expr>          primary_expression
%type <expr>          postfix_expression 
%type <expr>          unary_expression
%type <oper>             unary_operator
%type <expr>          multiplicative_expression
%type <expr>          additive_expression
%type <expr>          shift_expression
%type <expr>          relational_expression
%type <expr>          equality_expression
%type <expr>          and_expression
%type <expr>          exclusive_or_expression
%type <expr>          inclusive_or_expression
%type <expr>          logical_and_expression
%type <expr>          logical_xor_expression
%type <expr>          logical_or_expression
%type <expr>          conditional_expression
%type <expr>          assignment_expression
%type <oper>             assignment_operator
%type <expr>          expression
%type <decl>                declaration
%type <decl>                function_prototype
%type <decl>                function_declarator
%type <decl>                function_header_with_parameters
%type <decl>                function_header
%type <expr>          integer_expression
//%type <expr>          function_identifier
%type <decl>                parameter_declarator
%type <decl>                parameter_declaration
%type <type>                parameter_type_specifier
%type <decl>                init_declarator_list
%type <decl>                single_declaration
%type <type>                fully_specified_type
%type <type>                type_specifier
%type <type>                type_specifier_nonarray
%type <expr>          initializer
%type <stmt>                declaration_statement
%type <stmt>                statement
%type <stmt>                statement_no_new_scope
%type <stmt>                statement_with_scope
%type <stmt>                simple_statement
%type <stmt>            compound_statement_with_scope
%type <stmt>            compound_statement_no_new_scope
%type <stmtlist>            statement_list
%type <stmt>          expression_statement
%type <stmt>          selection_statement
%type <stmtlist>          selection_rest_statement
%type <stmt>                condition
%type <stmt>                switch_statement
%type <stmtlist>            switch_statement_list
%type <stmt>                case_label
%type <stmt>                iteration_statement
%type <stmt>                for_init_statement
%type <stmt>                conditionopt
%type <stmtlist>                for_rest_statement
%type <decllist>            translation_unit
%type <decl>                external_declaration
%type <decl>                function_definition
%type <expr>              constant_expression
%type <stmt>               jump_statement
%type <typequal>		   type_qualifier
%type <typequal>              storage_qualifier
// %type <vardecl>             array_specifier
%type <exprlist>             arg_params
//%type <typer>                  qual_spec
%type <ident>			type_ident	
%type <expr>		    function_call
%type <expr>		    function_call_header_no_parameters
%type <expr>		    function_call_header_with_parameters
%type <ident>		    function_identifier

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */




Program     
  :   translation_unit  { @1;
                          Program *program = new Program($1);
                          if (ReportError::NumErrors() == 0) 
                          program->Print(0);
                         }
  ;


variable_identifier  
  :   T_Identifier { $$ = new VarExpr(yylloc, new Identifier(yylloc, $1)); }
  ;

integer_expression
  :   expression { $$ = $1; }
  ;

primary_expression
  :   variable_identifier { $$ = $1; }
  |   T_IntConstant { $$ = new IntConstant(yylloc, $1); }
  |   T_FloatConstant { $$ = new FloatConstant(yylloc, $1); }
  |   T_BoolConstant { $$ = new BoolConstant(yylloc, $1); }
  |   T_LeftParen expression T_RightParen { $$ = $2; }
  ;

postfix_expression
  :   primary_expression { $$ = $1; }
  |   T_Identifier T_Dot T_Identifier { $$ = new FieldAccess(
        new VarExpr(yylloc, new Identifier(yylloc, $1)),
        new Identifier(yylloc, $3)); }
  |   postfix_expression T_Inc { $$ = new PostfixExpr($1, 
          new Operator(yylloc, "++")); }
  |   postfix_expression T_Dec { $$ = new PostfixExpr($1, 
          new Operator(yylloc, "--")); }
  |   postfix_expression T_LeftBracket integer_expression T_RightBracket { $$ = new ArrayAccess(@1,$1,$3); }
  |   function_call	{$$= $1;}
  ;   
/* for postfix_expr
  |   function_call   {$$ = $1;}

*/


function_call
  :   function_call_header_with_parameters T_RightParen { $$=$1; }
  |   function_call_header_no_parameters T_RightParen { $$=$1; }
  ;

function_call_header_no_parameters
  :   function_identifier T_LeftParen T_Void { //List<Expr*> *exprlist = new List<Expr*>;
	$$ = new Call(@1,NULL,$1 ,new List<Expr*>) ;  }
  |   function_identifier T_LeftParen { 
	$$ = new Call(@1, NULL , $1 ,new List<Expr*>);  }
  ;


function_call_header_with_parameters
  :   function_identifier T_LeftParen assignment_expression { 
						List<Expr*> *paramList = new List<Expr*>;
						paramList -> Append($3);
						$$= new Call(@1,NULL, $1 ,paramList); }
  |   function_identifier T_LeftParen arg_params {List<Expr*> *paramList = $3;
						 $$ = new Call(@1,NULL,$1,paramList);   }
  ;

function_identifier
  :   type_ident { $$ = $1; }
  |   T_Identifier { $$ = new Identifier(@1,$1); }
  ; 

arg_params
  :   arg_params T_Comma assignment_expression		{($$ = $1)-> Append($3);}
  |   assignment_expression				{($$ = new List<Expr*>)-> Append($1);}
  ;


unary_expression
  :   postfix_expression { $$ = $1; }
  |   T_Inc unary_expression { $$ = new ArithmeticExpr(
            new Operator(yylloc, "++"), $2); }
  |   T_Dec unary_expression { $$ = new ArithmeticExpr(
            new Operator(yylloc, "--"), $2);}
  |   unary_operator unary_expression { $$ = new ArithmeticExpr(
            $1, $2);}
  ;

unary_operator
  :   T_Plus { $$ = new Operator(yylloc, "+"); }
  |   T_Dash { $$ = new Operator(yylloc, "-"); }
  ;

multiplicative_expression
  :   unary_expression { $$ = $1; }
  |   multiplicative_expression T_Star unary_expression {
        $$ = new ArithmeticExpr($1, new Operator(yylloc, "*"), $3); }
  |   multiplicative_expression T_Slash unary_expression {
        $$ = new ArithmeticExpr($1, new Operator(yylloc, "/"), $3);
  }
  ;

additive_expression
  :   multiplicative_expression { $$ = $1; }
  |   additive_expression T_Plus multiplicative_expression { 
        $$ = new ArithmeticExpr($1, new Operator(yylloc, "+"), $3); }
  |   additive_expression T_Dash multiplicative_expression {
        $$ = new ArithmeticExpr($1, new Operator(yylloc, "-"), $3); }
  ;

shift_expression
  :   additive_expression { $$ = $1; }
  ;

relational_expression
  :   shift_expression { $$ = $1; }
  |   relational_expression T_LeftAngle shift_expression {
        $$ = new RelationalExpr($1, new Operator(yylloc, "<"), $3); }
  |   relational_expression T_RightAngle shift_expression {
        $$ = new RelationalExpr($1, new Operator(yylloc, ">"), $3); }
  |   relational_expression T_LessEqual shift_expression {
        $$ = new RelationalExpr($1, new Operator(yylloc, "<="), $3); }
  |   relational_expression T_GreaterEqual shift_expression {
        $$ = new RelationalExpr($1, new Operator(yylloc, ">="), $3); }
  ;

equality_expression
  :   relational_expression { $$ = $1; }
  |   equality_expression T_EQ relational_expression {
        $$ = new EqualityExpr( $1, new Operator(yylloc, "=="), $3); }
  |   equality_expression T_NE relational_expression {
        $$ = new EqualityExpr($1, new Operator(yylloc, "!="), $3); }
  ;

and_expression
  :   equality_expression { $$ = $1; }
  ;

exclusive_or_expression
  :   and_expression { $$ = $1; }
  ;

inclusive_or_expression
  :   exclusive_or_expression { $$ = $1; }
  ;

logical_and_expression
  :   inclusive_or_expression { $$ = $1; }
  |   logical_and_expression T_And inclusive_or_expression {
        $$ = new LogicalExpr($1, new Operator(yylloc, "&&"), $3); }
  ;

logical_xor_expression
  :   logical_and_expression { $$ = $1; }
  ;

logical_or_expression
  :   logical_xor_expression { $$ = $1; }
  |   logical_or_expression T_Or logical_xor_expression {
        $$ = new LogicalExpr($1, new Operator(yylloc, "||"), $3); }
  ;

conditional_expression
  :   logical_or_expression { $$ = $1; }
  ;

assignment_expression
  :   conditional_expression { $$ = $1; }
  |   unary_expression assignment_operator assignment_expression {
        $$ = new AssignExpr($1, $2, $3); }
  ;

assignment_operator
  :   T_Equal { $$ = new Operator(yylloc, "="); }
  |   T_MulAssign { $$ = new Operator(yylloc, "*="); }
  |   T_DivAssign { $$ = new Operator(yylloc, "/="); }
  |   T_AddAssign { $$ = new Operator(yylloc, "+="); }
  |   T_SubAssign { $$ = new Operator(yylloc, "-="); }
  ;

expression
  :   assignment_expression { $$ = $1; }
  ;

constant_expression
  :   conditional_expression { $$ = $1;}
  ;

declaration
  :   function_prototype T_Semicolon { $$ = $1; }
  |   init_declarator_list T_Semicolon { $$ = $1; }
  |   type_qualifier T_Identifier T_Semicolon {$$ = (Decl*)$1;  }
  ;


function_prototype
  :   function_declarator T_RightParen { $$ = $1; }
  ;

function_declarator
  :   function_header { $$ = $1; }
  |   function_header_with_parameters { $$ = $1; }
  ;
 

function_header_with_parameters
  :   function_header parameter_declaration { $$ = $1;
    //    ((FnDecl*)($$))->formals->Append((VarDecl*)$2);
 }
  |   function_header_with_parameters ',' parameter_declaration {
        $$ = $1;
     //   ((FnDecl*)($$))->formals->Append((VarDecl*)$3);
 }
  ;


function_header
  :   fully_specified_type T_Identifier T_LeftParen { $$ = new FnDecl(
        new Identifier(yylloc, $2), $1, new List<VarDecl*>); }
  ;

parameter_declarator
  :   type_specifier T_Identifier { $$ = new VarDecl( new Identifier(yylloc, $2), $1); }
  ;

parameter_declaration
  :   parameter_declarator { $$ = $1; } 
  |   parameter_type_specifier { $$ = (Decl*)$1; }
  ;

parameter_type_specifier
  :   type_specifier { $$ = $1; }
  ;

init_declarator_list
  :   single_declaration { $$ = $1; }
  ;

single_declaration
  :   fully_specified_type T_Identifier { $$ = new VarDecl(  new Identifier(yylloc, $2), $1); }
  |   fully_specified_type {$$ = (VarDecl*)$1;} 
  |   fully_specified_type T_Identifier T_Equal initializer { $$ = new VarDecl(  new Identifier(yylloc, $2), $1,$4); }
  |   fully_specified_type T_Identifier array_specifier { $$ = new VarDecl(new Identifier(yylloc, $2), new ArrayType(@1,$1)); }
  ;


fully_specified_type
  :   type_specifier { $$ = $1; }
  |   type_qualifier type_specifier { $$ = $2; } 
  ;


type_qualifier
  :   storage_qualifier  {$$=$1;  }
  |   type_qualifier storage_qualifier {$$= $1;  }
  ;


storage_qualifier
  : T_Const  {$$= TypeQualifier::constTypeQualifier; }
  | T_In  { $$= TypeQualifier::inTypeQualifier ;}
  | T_Out {$$= TypeQualifier::outTypeQualifier; }
  | T_Uniform {$$= TypeQualifier::uniformTypeQualifier;}
  ;

array_specifier
  :   T_LeftBracket constant_expression T_RightBracket { }
  ;



type_specifier
  :   type_specifier_nonarray { $$ = $1; }
  |   type_specifier_nonarray array_specifier { $$ = new ArrayType(@1,$1);  }
  ;

/* for type specifier

*/

type_specifier_nonarray
  :   T_Void { $$ = Type::voidType; }
  |   T_Float { $$ = Type::floatType; }
  |   T_Int  { $$ = Type::intType; }
  |   T_Bool { $$ = Type::boolType; }
  |   T_Vec2 { $$ = Type::vec2Type; }
  |   T_Vec3 { $$ = Type::vec3Type; }
  |   T_Vec4 { $$ = Type::vec4Type; }
  |   T_Mat2 { $$ = Type::mat2Type; }
  |   T_Mat3 { $$ = Type::mat3Type; }
  |   T_Mat4 { $$ = Type::mat4Type; }
  |   T_Ivec2 { $$ = Type::ivec2Type; }
  |   T_Ivec3 { $$ = Type::ivec3Type; }
  |   T_Ivec4 { $$ = Type::ivec4Type; }
  |   T_Uvec2 { $$ = Type::uvec2Type;}
  |   T_Uvec3  { $$ = Type::uvec3Type;}
  |   T_Uvec4  { $$ = Type::uvec4Type; }
  |   T_Bvec2 { $$ = Type::bvec2Type;}
  |   T_Bvec3  { $$ = Type::bvec3Type; }
  |   T_Bvec4  { $$ = Type::bvec4Type;}
  ; 
type_ident
  :   T_Void { $$ = new Identifier(@1, "void"); }
  |   T_Float { $$ = new Identifier(@1, "float");  }
  |   T_Int  { $$ = new Identifier(@1, "int"); }
  |   T_Bool { $$ = new Identifier(@1, "bool"); }
  |   T_Vec2 { $$ = new Identifier(@1, "vec2"); }
  |   T_Vec3 { $$ = new Identifier(@1, "vec3"); }
  |   T_Vec4 { $$ = new Identifier(@1, "vec4");  }
  |   T_Mat2 { $$ = new Identifier(@1, "mat2");  }
  |   T_Mat3 { $$ = new Identifier(@1, "mat3");  }
  |   T_Mat4 { $$ = new Identifier(@1, "mat4");  }
  |   T_Ivec2 { $$ = new Identifier(@1, "ivec2"); }
  |   T_Ivec3 { $$ = new Identifier(@1, "ivec3"); }
  |   T_Ivec4 { $$ = new Identifier(@1, "ivec4");  }
  |   T_Uvec2 { $$ = new Identifier(@1, "uvec2"); }
  |   T_Uvec3  { $$ = new Identifier(@1, "uvec3"); }
  |   T_Uvec4  { $$ = new Identifier(@1, "uvec4");  }
  |   T_Bvec2 { $$ = new Identifier(@1, "bvec2"); }
  |   T_Bvec3  { $$ = new Identifier(@1, "bvec3");  }
  |   T_Bvec4  { $$ = new Identifier(@1, "bvec4"); }

initializer
  :   assignment_expression { $$ = $1; }
  ;

declaration_statement
  :   declaration { $$ = (Stmt*)$1; }
  ;

statement
  :   compound_statement_with_scope { $$ = $1; }
  |   simple_statement { $$ = $1; }
  ;


statement_no_new_scope
  :   compound_statement_no_new_scope { $$ = $1; }
  |   simple_statement { $$ = $1; }
  ;

statement_with_scope
  :   compound_statement_no_new_scope { $$ = $1; }
  |   simple_statement { $$ = $1; }
  ;

simple_statement
  :   declaration_statement { $$ = $1; }
  |   expression_statement { $$ = $1; }
  |   selection_statement { $$ = $1; }
  |   switch_statement { $$ = $1; }
  |   case_label { $$ = $1; }
  |   iteration_statement { $$ = $1; }
  |   jump_statement { $$ = $1; } 
  ;

compound_statement_with_scope
  :   T_LeftBrace T_RightBrace { $$ = new StmtBlock(
        new List<VarDecl*>, new List<Stmt*>); }
  |   T_LeftBrace statement_list T_RightBrace { $$ = new StmtBlock(
        new List<VarDecl*>, $2); }
  ;

compound_statement_no_new_scope
  :   T_LeftBrace T_RightBrace { $$ = new StmtBlock( new List<VarDecl*>, new List<Stmt*>); }
  |   T_LeftBrace statement_list T_RightBrace { $$ = new StmtBlock(
        new List<VarDecl*>, $2); }
  ;

statement_list
  :   statement { $$ = new List<Stmt*>; $$->Append($1); }
  |   statement_list statement { $$ = $1; $$->Append($2); }
  ;

expression_statement
  :   T_Semicolon { $$ = new StmtBlock(
        new List<VarDecl*>, new List<Stmt*>); }
  |   expression T_Semicolon { $$ = $1; }
  ;

selection_statement
  :   T_If T_LeftParen expression T_RightParen selection_rest_statement {
        $$ = new IfStmt($3, $5->Nth(0),
        (($5->NumElements()) > 1) ? ($5->Nth(1)) : NULL);
      }
  ; 

selection_rest_statement
  :   statement_with_scope T_Else statement_with_scope {
        $$ = new List<Stmt*>;
        $$->Append($1); $$->Append($3); }
  |   statement_with_scope %prec NoElseToken { $$ = new List<Stmt*>;
        $$->Append($1); }
  ;

condition
  :   expression { $$ = $1; }
  |   fully_specified_type T_Identifier T_Equal initializer {
        $$ = new VarExpr(yylloc, new Identifier(yylloc, $2)); }
  ;

switch_statement
  :   T_Switch T_LeftParen expression T_RightParen T_LeftBrace switch_statement_list T_RightBrace {
        $$ = new SwitchStmt($3, (List<Case*>*)$6, NULL); }
  ;

switch_statement_list
  :   statement_list { $$ = $1; }
  ;

case_label
  :   T_Case expression T_Colon statement { List<Stmt*> *a = new List<Stmt*>;
        a->Append((Stmt*)$4); $$ = new Case((IntConstant*)$2, a); }
  |   T_Default T_Colon statement { List<Stmt*> *a = new List<Stmt*>;
        a->Append((Stmt*)$3); $$ = new Default(a); }
  ;

iteration_statement
  :   T_While T_LeftParen condition T_RightParen statement_no_new_scope { $$ = new WhileStmt((Expr*)$3, $5); }
  |   T_Do statement_with_scope T_While T_LeftParen expression T_RightParen T_Semicolon { $$ = new DoWhileStmt( $2, (Expr*)$5); }
  |   T_For T_LeftParen for_init_statement for_rest_statement T_RightParen statement_no_new_scope {
        Expr* a; Expr *b = NULL; a = (Expr*)($4->Nth(0));
        if($4->NumElements() == 2) b = (Expr*)($4->Nth(1));  
        $$ = new ForStmt((Expr*)$3, a, b, $6);}
  ;

for_init_statement
  :   expression_statement { $$ = $1; }
  |   declaration_statement { $$ = $1; }
  ;

conditionopt
  :   condition { $$ = $1; }
  ;

for_rest_statement
  :   conditionopt T_Semicolon { $$ = new List<Stmt*>; $$->Append($1); }
  |   conditionopt T_Semicolon expression { $$ = new List<Stmt*>;
        $$->Append($1); $$->Append($3); }
  ;

jump_statement
  :   T_Break    T_Semicolon {$$ = new BreakStmt(@1);  }
  |   T_Return T_Semicolon { $$ = new ReturnStmt(@1, new EmptyExpr()); }     
  |   T_Return expression T_Semicolon { $$ = new ReturnStmt(@2,$2); }
  ;

translation_unit
  :   external_declaration { $$ = new List<Decl*>();
        $$->Append($1); }
  |   translation_unit external_declaration { $$ = $1;
        $$->Append($2); }
  ;

external_declaration
  :   function_definition { $$ = $1; }
  |   declaration { $$ = $1; }
  ;

function_definition
  :   function_prototype compound_statement_no_new_scope {
        $$ = $1; ((FnDecl*)$$)->SetFunctionBody($2);
      }
  ;

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
