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
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

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
    List<Decl*> *declList;
    Type * type;
    TypeQualifier * typeQualifier;
    //experssions
    Expr * expr;
    Operator *op;
    Call *call;
    FnDecl *fnDecl;
    LogicalExpr *logicalExpr;
    RelationalExpr *relationalExpr;
    EqualityExpr *equalityExpr;
    SelectionExpr *selectionExpr;
    AssignExpr *assignExpr;
    ArithmeticExpr *arithmeticExpr;
    //stmts
    Stmt *stmt;
    List<Stmt*> *stmtList;
    ConditionalStmt *conditionalStmt;
    LoopStmt *loopStmt;
    WhileStmt *whileStmt;
    ForStmt *forStmt;
    DoWhileStmt *doWhileStmt;
    IfStmt *ifStmt;
    BreakStmt *breakStmt;
    ReturnStmt *returnStmt;
    SwitchLabel *switchLabel;
    Case *c;
    Default *d;
    SwitchStmt *switchStmt;

    

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
%type <declList>  DeclList
%type <decl>      Decl
%type <decl>      Declaration

%type <decl>      VarIdentifier 

%type <expr>      PrimaryExpr
%type <expr>      PostFixExpr
%type <expr>      IntegerExpr
%type <expr>      Expression UnaryExpr MultiExpr AddExpr ShiftExpr RelationalExpr EqualityExpr AndExpr 
%type <expr>      ExclusiveOrExpr InclusiveOrExpr ConditionalExpr AssignmentExpr 
%type <expr>      ConstExpr 

%type <logicalExpr> LogicalAndExpr LogicalXorExpr LogicalOrExpr
%type <expr>  Initializer

%type <call>      FunctionCall FunctionCallHeaderNoParam FunctionCallHeaderParam FunctionCallHeader

%type <fnDecl>    FunctionDef FunctionProto FunctionDeclarator FunctionHeaderParam FunctionHeader

%type <decl>      ParamDecl ParamDeclarator SingleDeclaration  
%type <type>      FunctionIdentifier ParamTypeSpecifier 
%type <declList>  InitDeclaratorList

%type <type>      FullySpecifiedType TypeSpecifier ArrSpecifier TypeSpecifierNonArr

%type <typeQualifier> TypeQualifier SingleTypeQualifier StorageQualifier 

%type <stmt> Statement StatementScope StatementNoScope SimpleStatement CompoundStmtScope CompoundStmtNoScope

%type <stmtList> StatementList

%type <expr> ExpressionStmt
%type <ifStmt> SelectionStmt SelectionRestStmt
%type <expr> Condition ConditionOpt
%type <switchStmt> SwitchStmt
%type <c> CaseLabel
%type <stmt> IterationStmt ForInitStmt ForRestStmt JumpStmt


%type<op>         UnaryOp AssignmentOp


%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */
Program   :    DeclList            {
                                      @1;
                                      /* pp2: The @1 is needed to convince
                                       * yacc to set up yylloc. You can remove
                                       * it once you have other uses of @n*/
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0)
                                          program->Print(0);
                                      }
          ;

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :   FunctionDef { $$ = $1; }
          |   Declaration {
                $$ = $1;
              }
          ;



VarIdentifier:  T_Identifier{ 
                    $$ = new Identifier(@1, $1);
                }
             ;


PrimaryExpr:  T_Identifier { 
                $$ = new VarExpr( @1, new Identifier( @1,$1));
          }
          |   T_IntConstant {
                $$ = new IntConstant( @1, $1);
          }
          |   T_FloatConstant {
                $$ = new FloatConstant( @1, $1);
          }
          |   T_BoolConstant {
                $$ = new BoolConstant( @1, $1);
          }
          |   T_LeftParen Expression T_RightParen { $$ = $2; }
          ;

IntegerExpr:  Expression { $$ = $1; };


PostFixExpr:  PrimaryExpr {
                $$ = $1;
              }
          |   PostFixExpr T_LeftBracket Expression T_RightBracket {$$ = new }
          |   PostFixExpr T_Dot T_Identifier {$$ = new FieldAccess($1, new Identifier(@3, $3)); }
          |   PostFixExpr T_Inc {$$ = new PostfixExpr($1, new Operator(@2, "++"));}
          |   PostFixExpr T_Dec {$$ = new PostfixExpr($1, new Operator(@2, "--"));}
          |   FunctionCall { $$ = $1; }
          ;


FunctionCall  :   FunctionCallHeaderParam T_RightParen {$$ = new Call(@1, NULL, $1.field, $1.actuals);}
              |   FunctionCallHeaderNoParam T_RightParen {
			List<Expr*> *emptyList;				
			$$ = new Call(@1, NULL , $1.field, emptyList);
		}
              ;

FunctionCallHeaderNoParam:  FunctionCallHeader T_Void { $$ = new Call(); }
                         |  FunctionCallHeader
                         ;

FunctionCallHeaderParam:  FunctionCallHeader AssignmentExpr {$$ = new Call(@1, $2, $1.field, $1.actuals); }
                       |  FunctionCallHeaderParam T_Comma AssignmentExpr
			 { $$ = new Call(@1, $3, $1.field, $1.actuals);}
                       ;

FunctionCallHeader: FunctionIdentifier T_LeftParen { $$ = new Call(@1, NULL, $1, NULL); }
                  ;

FunctionIdentifier:  T_Identifier { $$ = new Identifier(@1, $1); }
                  ;

UnaryExpr:    PostFixExpr { $$ = $1; }
          |   T_Inc UnaryExpr { $$ = new ArithmeticExpr(NULL,new Operator(@1,"++"),$2); }
          |   T_Dec UnaryExpr { $$ = new ArithmeticExpr(NULL,new Operator(@1,"--"),$2); }
          |   UnaryOp UnaryExpr  { $$ = new ArithmeticExpr(NULL, $1, $2); }
          ;

UnaryOp:      T_Plus  {$$ = new Operator(@1, "+");}
          |   T_Dash  {$$ = new Operator(@1, "-");}
          ;

MultiExpr:    UnaryExpr  {$$ = $1;}
          |   MultiExpr T_Star UnaryExpr { $$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3);}
          |   MultiExpr T_Slash UnaryExpr {$$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3);}
          ;

AddExpr:      MultiExpr {$$ = $1;}
          |   AddExpr T_Plus MultiExpr {$$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3);}
          |   AddExpr T_Dash MultiExpr {$$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3);}
          ;

ShiftExpr:    AddExpr {$$ = $1;};

RelationalExpr: ShiftExpr {$$ = $1;}
          |   RelationalExpr T_LeftAngle ShiftExpr {$$ = new RelationalExpr($1, new Operator(@2, "<"), $3);}
          |   RelationalExpr T_RightAngle ShiftExpr {$$ = new RelationalExpr($1, new Operator(@2, ">"), $3);}
          |   RelationalExpr T_LessEqual ShiftExpr {$$ = new RelationalExpr($1, new Operator(@2, "<="), $3);}
          |   RelationalExpr T_GreaterEqual ShiftExpr {$$ = new RelationalExpr($1, new Operator(@2, ">="), $3);}
          ;

EqualityExpr: RelationalExpr {$$ = $1;}
          |   EqualityExpr T_EQ RelationalExpr {$$ = new EqualityExpr($1, new Operator(@2, "=="), $3);}
          |   EqualityExpr T_NE RelationalExpr {$$ = new EqualityExpr($1, new Operator(@2, "!="), $3);}
          ;

AndExpr:      EqualityExpr {$$ = $1;};

ExclusiveOrExpr:  AndExpr {$$ = $1;};

InclusiveOrExpr:  ExclusiveOrExpr {$$ = $1;};

LogicalAndExpr:   InclusiveOrExpr {$$ = $1;}
          |   LogicalAndExpr T_And InclusiveOrExpr {$$ = new LogicalExpr($1, new Opeartor(@2, "&&"), $3);}
          ;

LogicalXorExpr:   LogicalAndExpr {$$ = $1;};

LogicalOrExpr:    LogicalXorExpr {$$ = $1;}
         |   LogicalOrExpr T_Or LogicalXorExpr {$$ = new LogicalExpr($1, new Operator(@2, "||"), $3);}
         ;

ConditionalExpr: LogicalOrExpr {$$ = $1;}
               | LogicalOrExpr T_Question Expression T_Colon AssignmentExpr {$$ = new SelectionExpr($1, $3, $5);}
	;

AssignmentExpr:  ConditionalExpr {$$ = $1;}
              |  UnaryExpr AssignmentOp AssignmentExpr {$$ = new AssignExpr($1, new Operator(@2, "="), $3);}
              ;

AssignmentOp:  T_Equal  {$$ = new Operator(@1, "=");}
            |  T_MulAssign  {$$ = new Operator(@1, "*="); }
            |  T_AddAssign  {$$ = new Operator(@1, "+="); }
            |  T_DivAssign  {$$ = new Operator(@1, "/="); }
            |  T_SubAssign  {$$ = new Operator(@1, "-="); }
            ;

Expression:  AssignmentExpr {$$ = $1;};

ConstExpr: ConditionalExpr {$$ = $1;};

Declaration:  FunctionProto T_Semicolon {$$=$1;}
           |  InitDeclaratorList T_Semicolon {$$=$1;}
           |  TypeQualifier T_Identifier T_Semicolon //STUFF
           ;

FunctionProto:  FunctionDeclarator T_RightParen {$$ = $1;}
             ;

FunctionDeclarator:  FunctionHeader {$$ = $1;}
                  |  FunctionHeaderParam {$$ = $1;}
                  ;

FunctionHeaderParam: FunctionHeader ParamDecl
                   | FunctionHeaderParam T_Comma ParamDecl
                   ;

FunctionHeader:  FullySpecifiedType T_Identifier T_LeftParen;

ParamDeclarator:  TypeSpecifier T_Identifier;

ParamDecl:  ParamDeclarator
         |  ParamTypeSpecifier  
         ;

ParamTypeSpecifier:  TypeSpecifier;

InitDeclaratorList:  SingleDeclaration;


SingleDeclaration:  FullySpecifiedType 
                 |  FullySpecifiedType T_Identifier
                 |  FullySpecifiedType T_Identifier ArrSpecifier
                 |  FullySpecifiedType T_Identifier T_Equal Initializer
                 ;

FullySpecifiedType:  TypeSpecifier
                  |  TypeQualifier TypeSpecifier
                  ;

TypeQualifier:  SingleTypeQualifier
             |  TypeQualifier SingleTypeQualifier
             ;

SingleTypeQualifier:  StorageQualifier;

StorageQualifier:  T_Const  { $$ = TypeQualifier::constTypeQualifier;}
                   |  T_In  { $$ = TypeQualifier::inTypeQualifier; }
                   |  T_Out  { $$ = TypeQualifier::outTypeQualifier; }
                   |  T_Uniform  {$$ = TypeQualifier::uniformTypeQualifier;}
                   ;

TypeSpecifier:  TypeSpecifierNonArr { $$=$1;}
             |  TypeSpecifierNonArr ArrSpecifier {$$ = new ArrayType(@1, $1);}
             ;

ArrSpecifier:  T_LeftBracket ConstExpr T_RightBracket {};

TypeSpecifierNonArr:  T_Void  {$$ = Type::voidType;}
                   |   T_Bool  {$$ = Type::boolType;}
                   |   T_Int   {$$ = Type::intType;}
                   |   T_Float {$$ = Type::floatType;}
                   |   T_Mat2   {$$ = Type::mat2Type;}
                   |   T_Mat3   {$$ = Type::mat3Type;}
                   |   T_Mat4   {$$ = Type::mat4Type;}
                   |   T_Vec2   {$$ = Type::vec2Type;}
                   |   T_Vec3   {$$ = Type::vec3Type;}
                   |   T_Vec4   {$$ = Type::vec4Type;}
                   |   T_Ivec2   {$$ = Type::ivec2Type;}
                   |   T_Ivec3   {$$ = Type::ivec3Type;}
                   |   T_Ivec4   {$$ = Type::ivec4Type;}
                   |   T_Bvec2   {$$ = Type::bvec2Type;}
                   |   T_Bvec3   {$$ = Type::bvec3Type;}
                   |   T_Bvec4   {$$ = Type::bvec4Type;}
                   |   T_Uint   {$$ = Type::uintType;}
                   |   T_Uvec2   {$$ = Type::uvec2Type;}
                   |   T_Uvec3   {$$ = Type::uvec3Type;}
                   |   T_Uvec4   {$$ = Type::uvec4Type;}
                   ;     

Initializer: AssignmentExpr {};

DeclarationStmt:  Declaration {};

Statement:  CompoundStmtScope {$$=$1;}
         |  SimpleStatement {$$=$1;}
         ;

StatementScope:  CompoundStmtNoScope
              |  SimpleStatement
              ;

StatementNoScope:  CompoundStmtNoScope
               |   SimpleStatement
               ;

SimpleStatement:  DeclarationStmt {$$ = $1;}
               |  ExpressionStmt {$$ = $1;}
               |  SelectionStmt {$$ = $1;}
               |  SwitchStmt {$$ = $1;}
               |  CaseLabel {$$ = $1;}
               |  IterationStmt {$$ = $1;}
               |  JumpStmt {$$ = $1;}
               ;

CompoundStmtScope:  T_LeftBrace T_RightBrace
                 |  T_LeftBrace StatementList T_RightBrace
                 ;

CompoundStmtNoScope: T_LeftBrace T_RightBrace
                   |  T_LeftBrace StatementList T_RightBrace
                   ;

StatementList:  Statement
             |  StatementList Statement
             ; 

ExpressionStmt:  T_Semicolon {$$ = new EmptyExpr();}
              |  Expression T_Semicolon {$$ =$1;}
              ;

SelectionStmt:  T_If T_LeftParen Expression T_RightParen SelectionRestStmt;

SelectionRestStmt:  StatementScope T_Else StatementScope
                 |  StatementScope
                 ;

Condition:  Expression {$$ = $1;}
         |  FullySpecifiedType T_Identifier T_Equal Initializer
         ;

SwitchStmt:  T_Switch T_LeftParen Expression T_RightParen T_LeftBrace SwitchStmtList T_RightBrace;

SwitchStmtList: StatementList;

CaseLabel:  T_Case Expression T_Colon
         |  T_Default T_Colon
         ;

IterationStmt:  T_While T_LeftParen Condition T_RightParen StatementNoScope
             |  T_Do StatementScope T_While T_LeftParen Expression T_RightParen T_Semicolon
             |  T_For T_LeftParen ForInitStmt ForRestStmt T_RightParen StatementNoScope
             ;

ForInitStmt:  ExpressionStmt
           ;

ConditionOpt: Condition;

ForRestStmt:  ConditionOpt T_Semicolon
           |  ConditionOpt T_Semicolon Expression
           ;

JumpStmt:  T_Break T_Semicolon {$$ = new BreakStmt(@1);}
        |  T_Return T_Semicolon {$$ = new ReturnStmt(@1, new EmptyExpr());}
        |  T_Return Expression T_Semicolon {$$ = new ReturnStmt(@1, $2);}
        ;


FunctionDef:  FunctionProto CompoundStmtNoScope;

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
