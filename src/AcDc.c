#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"

// XXX: use strncpy instead of strcpy Orz
// fine
int main (int argc, char *argv[]) {
    FILE *source, *target;
    Program program;
    SymbolTable symtab;

    if (argc == 3) {
        source = fopen(argv[1], "r");
        target = fopen(argv[2], "w");
        if (!source) {
            printf("can't open the source file\n");
            exit(2);
        }
        else if ( !target ) {
            printf("can't open the target file\n");
            exit(2);
        }
        else {
            program = parser(source);
            fclose(source);
            symtab = build(program);
            check(&program, &symtab);
            gencode(program, target);
            fclose(target);
        }
    }
    else {
        printf("Usage: %s source_file target_file\n", argv[0]);
    }


    return 0;
}


/*********************************************
  Scanning
 *********************************************/
Token getNumericToken (FILE *source, int c) {
// token: \d+\.\d+
    Token token;
    int i = 0;

    while ( isdigit(c) ) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    if ( c != '.' ) {
        ungetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = fgetc(source);
    if ( !isdigit(c) ) {
        ungetc(c, source);
        printf("Expect a digit : %c\n", c);
        exit(1);
    }

    while ( isdigit(c) ) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    ungetc(c, source);
    token.tok[i] = '\0';
    token.type = FloatValue;
    return token;
}

Token scanner ( FILE *source ) {
    int c;
    Token token;

    // scan till end-of-file
    while ( !feof(source) ) {
        c = fgetc(source);

        // token: ignore captiously spaces
        while( isspace(c) ) c = fgetc(source);

        // token: \d+\.\d+
        if( isdigit(c) )
            return getNumericToken(source, c);

        // token: variables or declarations
        token.tok[0] = c;
        token.tok[1] = '\0';
        if ( islower(c) ) {
            // pick up the next chak
            int cn = fgetc(source);

            // case: \w, c + cn = char + space
            if (isspace(cn)) {
                // f i p for special use
                if (c == 'f')
                    token.type = FloatDeclaration;
                else if (c == 'i')
                    token.type = IntegerDeclaration;
                else if (c == 'p')
                    token.type = PrintOp;
                else
                    token.type = Identifier;

                return token;
            }
            // case: \w{2,}, c + c* + space
            else {
                int i = 1;
                while (islower(cn)) {
                    token.tok[i++] = cn;
                    cn = fgetc(source);
                }
                token.tok[i] = '\0';
                token.type = Identifier;
                return token;
            }
        }

        // token: operators
        switch (c) {
            case '=':
                token.type = AssignmentOp;
                return token;
            case '+':
                token.type = PlusOp;
                return token;
            case '-':
                token.type = MinusOp;
                return token;
            case '*':
                token.type = MulOp;
                return token;
            case '/':
                token.type = DivOp;
                return token;
            case EOF:
                token.type = EOFsymbol;
                token.tok[0] = '\0';
                return token;
            default:
                printf("Invalid character : %c\n", c);
                exit(1);
        }
    }

    // token: EOF
    token.tok[0] = '\0';
    token.type = EOFsymbol;
    return token;
}


/********************************************************
  Parsing
 *********************************************************/
Declaration parseDeclaration ( FILE *source, Token token ) {
    Token token2;
    switch (token.type) {
        case FloatDeclaration:
        case IntegerDeclaration:
            // get next token
            token2 = scanner(source);
            // can't be f/i/p, they are reserved words
            if (token2.type == FloatDeclaration ||
                token2.type == IntegerDeclaration ||
                token2.type == PrintOp) {
                printf("Syntax Error: %s cannot be used as id\n", token2.tok);
                exit(1);
            }
            return makeDeclarationNode( token, token2 );

        default:
            // only support float & int
            printf("Syntax Error: Expect Declaration %s\n", token.tok);
            exit(1);
    }
}

// in AC, program = Declarations + Statements
// parse declarations here
Declarations *parseDeclarations ( FILE *source ) {
    Token token = scanner(source);
    Declaration decl;
    Declarations *decls;
    int i = 0;
    switch (token.type) {
        // only play with float & int declarations
        case FloatDeclaration:
        case IntegerDeclaration:
            decl = parseDeclaration(source, token);
            decls = parseDeclarations(source);
            return makeDeclarationTree( decl, decls );

        // shouldn't here //actually they should be here
        // I mean, print / id and eof shouldn't be in declaration list
        case PrintOp:
            ungetc(' ', source);
            ungetc(token.tok[0], source);
            return NULL;
        case Identifier:
            while (token.tok[i] != '\0')
                i++;
            ungetc(' ', source);
            while (i > 0)
                ungetc(token.tok[--i], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect declarations %s\n", token.tok);
            exit(1);
    }
}

// id, IntConst, FloatConst
Expression *parseValue ( FILE *source ) {
    Token token = scanner(source);
    Expression *value = (Expression *)malloc( sizeof(Expression) );
    value->leftOperand = value->rightOperand = NULL;


    switch (token.type) {
        case Identifier:
            (value->v).type = IdentifierV;
            (value->v).val.id = (char *)malloc(sizeof(char)*256);
            strncpy((value->v).val.id, token.tok, strlen(token.tok));
            break;
        case IntValue:
            (value->v).type = IntConst;
            (value->v).val.ivalue = atoi(token.tok);
            break;
        case FloatValue:
            (value->v).type = FloatConst;
            (value->v).val.fvalue = atof(token.tok);
            break;
        default:
            printf("Syntax Error: Expect Identifier or a Number %s\n", token.tok);
            exit(1);
    }

    return value;
}

Expression *parseExpressionTail ( FILE *source, Expression *lvalue ) {
    Token token = scanner(source);
    Token next_token;
    Expression *expr;
    int i = 0;

    switch (token.type) {
        case PlusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = PlusNode;
            (expr->v).val.op = Plus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail_t(source, expr);
        case MinusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MinusNode;
            (expr->v).val.op = Minus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail_t(source, expr);
        case Identifier:
            next_token = scanner(source);
            if (next_token.type != AssignmentOp) {
                printf("Syntax Error: Expect an numeric operator\n");
                exit(1);
            }
            ungetc('=', source);
            while (token.tok[i] != '\0')
                i++;
            ungetc(' ', source);
            while (i > 0)
                ungetc(token.tok[--i], source);
            return lvalue;
        case PrintOp:
            ungetc(' ', source);
            ungetc(token.tok[0], source);
            return lvalue;
        case EOFsymbol:
            return lvalue;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

Expression *parseExpressionTail_t ( FILE *source, Expression *lvalue ) {
    Token token = scanner(source);
    Token next_token;
    Expression *expr;
    int i = 0;

    switch (token.type) {
        case MulOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MulNode;
            (expr->v).val.op = Mul;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case DivOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = DivNode;
            (expr->v).val.op = Div;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case Identifier:
            next_token = scanner(source);
            if (next_token.type != AssignmentOp) {
                printf("Syntax Error: Expect an numeric operator\n");
                exit(1);
            }
            ungetc('=', source);
            while (token.tok[i] != '\0')
                i++;
            ungetc(' ', source);
            while (i > 0)
                ungetc(token.tok[--i], source);
            return lvalue;
        case PrintOp:
            ungetc(' ', source);
            ungetc(token.tok[0], source);
            return lvalue;
        case EOFsymbol:
            return lvalue;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

Expression *parseExpression_t ( FILE *source, Expression *lvalue ) {
    Token token = scanner(source);
    Token next_token;
    Expression *expr;
    int i = 0;

    switch (token.type) {
        case MulOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MulNode;
            (expr->v).val.op = Mul;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case DivOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = DivNode;
            (expr->v).val.op = Div;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail(source, expr);
        case Identifier:
            next_token = scanner(source);
            if (next_token.type != AssignmentOp) {
                printf("Syntax Error: Expect an numeric operator\n");
                exit(1);
            }
            ungetc('=', source);
            while (token.tok[i] != '\0')
                i++;
            ungetc(' ', source);
            while (i > 0)
                ungetc(token.tok[--i], source);
            return NULL;
        case PrintOp:
            ungetc(' ', source);
            ungetc(token.tok[0], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}


//
// Expr -> plus Val Expr
//      |  minus Val Expr
//      |  \
//
// XXX: we need a new grammar to handle ambiguity
// add a new type of Expression Expr_t
//
// Expr -> plus Val Expr_t
//      |  minus Val Expr_t
//      |  \
//
// Expr_t -> mul Val Expr
//        |  div Val Expr
//        |  Expr
//
// JJJ: I have an idea about grammars
// 
// Stmt -> id assign Expr 
//      |  print id
//
// Expr -> Expr_t plus Expr
//      |  Expr_t minus Expr
//      |  Expr_t
//
// Expr_t -> Val mul Expr_t
//        |  Val div Expr_t
//        |  Val
//
Expression *parseExpression ( FILE *source, Expression *lvalue ) {
    Token token = scanner(source);
    Token next_token;
    Expression *expr;
    int i = 0;

    switch (token.type) {
        case PlusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = PlusNode;
            (expr->v).val.op = Plus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail_t(source, expr);
        case MinusOp:
            expr = (Expression *)malloc( sizeof(Expression) );
            (expr->v).type = MinusNode;
            (expr->v).val.op = Minus;
            expr->leftOperand = lvalue;
            expr->rightOperand = parseValue(source);
            return parseExpressionTail_t(source, expr);
        case Identifier:
            next_token = scanner(source);
            if (next_token.type != AssignmentOp) {
                printf("Syntax Error: Expect an numeric operator\n");
                exit(1);
            }
            ungetc('=', source);
            while (token.tok[i] != '\0')
                i++;
            ungetc(' ', source);
            while (i > 0)
                ungetc(token.tok[--i], source);
            return NULL;
        case PrintOp:
            ungetc(' ', source);
            ungetc(token.tok[0], source);
            return NULL;
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
            exit(1);
    }
}

//
// Stmt -> id assign Val Expr
//      |  printn id
//
Statement parseStatement ( FILE *source, Token token ) {
    Token next_token;
    Expression *value, *expr;

    switch (token.type) {
        case Identifier:
            next_token = scanner(source);
            if (next_token.type == AssignmentOp) {
                value = parseValue(source);
                expr = parseExpression(source, value);
                return makeAssignmentNode(token.tok, value, expr);
            }
            else {
                printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);
                exit(1);
            }
        case PrintOp:
            next_token = scanner(source);
            if (next_token.type == Identifier)
                return makePrintNode(next_token.tok);
            else {
                printf("Syntax Error: Expect an identifier %s\n", next_token.tok);
                exit(1);
            }
            break;
        default:
            printf("Syntax Error: Expect a statement %s\n", token.tok);
            exit(1);
    }
}

//
// Stmts -> Stmt Stmts
//       |  \
//
Statements *parseStatements ( FILE * source ) {

    Token token = scanner(source);
    Statement stmt;
    Statements *stmts;

    switch (token.type) {
        case Identifier:
        case PrintOp:
            stmt = parseStatement(source, token);
            stmts = parseStatements(source);
            return makeStatementTree(stmt , stmts);
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect statements %s\n", token.tok);
            exit(1);
    }
}


/*********************************************************************
  Build AST
 **********************************************************************/
Declaration makeDeclarationNode ( Token declare_type, Token identifier ) {
    Declaration tree_node;

    switch (declare_type.type) {
        case FloatDeclaration:
            tree_node.type = Float;
            break;
        case IntegerDeclaration:
            tree_node.type = Int;
            break;
        default:
            break;
    }
    strncpy(tree_node.name, identifier.tok, strlen(identifier.tok));

    return tree_node;
}

Declarations *makeDeclarationTree ( Declaration decl, Declarations *decls ) {
    Declarations *new_tree = (Declarations *)malloc( sizeof(Declarations) );
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode ( char *id, Expression *v, Expression *expr_tail ) {
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    assign.id = (char *)malloc(sizeof(char) * 256);
    strcpy(assign.id, id);
    if (expr_tail == NULL)
        assign.expr = v;
    else
        assign.expr = expr_tail;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode ( char *id ) {
    Statement stmt;
    stmt.type = Print;
    stmt.stmt.variable = (char *)malloc(sizeof(char)*256);
    strcpy(stmt.stmt.variable, id);

    return stmt;
}

Statements *makeStatementTree ( Statement stmt, Statements *stmts ) {
    Statements *new_tree = (Statements *)malloc( sizeof(Statements) );
    new_tree->first = stmt;
    new_tree->rest = stmts;

    return new_tree;
}

/* parser */
Program parser ( FILE *source ) {
    Program program;

    // in AC, program = Declarations + Statements
    program.declarations = parseDeclarations(source);
    program.statements = parseStatements(source);

    return program;
}


/********************************************************
  Build symbol table
 *********************************************************/
void InitializeTable ( SymbolTable *table ) {
    int i;

    for (i = 0 ; i < 23; i++)
        table->table[i] = Notype;
}

void add_table ( SymbolTable *table, char *c, DataType t , int index) {
    // XXX: the table should fix to use a name as key

    if (table->table[index] != Notype) {
        printf("Error : id %s has been declared\n", c);//error
        exit(1);
    }
    strcpy(table->name[index], c);
    table->table[index] = t;
}

SymbolTable build ( Program program ) {
    SymbolTable table;
    Declarations *decls = program.declarations;
    Declaration current;
    int i = 0;

    InitializeTable(&table);

    while (decls != NULL) {
        current = decls->first;
        table.name[i] = (char *)malloc(sizeof(char)*256);
        add_table(&table, current.name, current.type, i++);
        decls = decls->rest;
    }

    // clear unused nodes
    while (i < 23) {
        table.name[i] = (char *)malloc(sizeof(char)*256);
        table.name[i++][0] = '\0';
    }

    return table;
}


/********************************************************************
  Type checking
 *********************************************************************/

void convertType ( Expression * old, DataType type ) {
    // float to int
    if (old->type == Float && type == Int) {
        printf("error : can't convert float to integer\n");
        return;
    }
    // int to float
    if (old->type == Int && type == Float) {
        Expression *tmp = (Expression *)malloc( sizeof(Expression) );
        if (old->v.type == IdentifierV)
            printf("convert to float %s \n",old->v.val.id);
        else
            printf("convert to float %d \n", old->v.val.ivalue);
        tmp->v = old->v;
        tmp->leftOperand = old->leftOperand;
        tmp->rightOperand = old->rightOperand;
        tmp->type = old->type;

        Value v;
        v.type = IntToFloatConvertNode;
        v.val.op = IntToFloatConvert;
        old->v = v;
        old->type = Int;
        old->leftOperand = tmp;
        old->rightOperand = NULL;
    }
}

DataType generalize ( Expression *left, Expression *right ) {
    if (left->type == Float || right->type == Float) {
        printf("generalize : float\n");
        return Float;
    }
    printf("generalize : int\n");
    return Int;
}

DataType lookup_table ( SymbolTable *table, char *c ) {
    int i;

    for(i = 0 ; i < 23 ; i++)
        if(strcmp(table->name[i], c) == 0)
            return table->table[i];

    printf("Error : identifier %s is not declared\n", c);//error
    exit(1);
}

void checkexpression ( Expression *expr, SymbolTable *table ) {
    char *c = (char *)malloc(sizeof(char)*256);
    if (expr->leftOperand == NULL && expr->rightOperand == NULL) {
        switch (expr->v.type) {
            case IdentifierV:
                strcpy(c, expr->v.val.id);
                printf("identifier : %s\n",c);
                expr->type = lookup_table(table, c);
                break;
            case IntConst:
                printf("constant : int\n");
                expr->type = Int;
                break;
            case FloatConst:
                printf("constant : float\n");
                expr->type = Float;
                break;
                //case PlusNode: case MinusNode: case MulNode: case DivNode:
            default:
                break;
        }
    }
    else {
        Expression *left = expr->leftOperand;
        Expression *right = expr->rightOperand;

        checkexpression(left, table);
        checkexpression(right, table);

        DataType type = generalize(left, right);
        convertType(left, type);//left->type = type;//converto
        convertType(right, type);//right->type = type;//converto
        expr->type = type;
    }
}

void checkstmt ( Statement *stmt, SymbolTable *table ) {
    if (stmt->type == Assignment) {
        AssignmentStatement assign = stmt->stmt.assign;
        printf("assignment : %s \n",assign.id);
        checkexpression(assign.expr, table);
        stmt->stmt.assign.type = lookup_table(table, assign.id);
        if (assign.expr->type == Float && stmt->stmt.assign.type == Int) {
            printf("error : can't convert float to integer\n");
        }
        else {
            convertType(assign.expr, stmt->stmt.assign.type);
        }
    }
    else if (stmt->type == Print) {
        printf("print : %s \n",stmt->stmt.variable);
        lookup_table(table, stmt->stmt.variable);
    }
    else
        printf("error : statement error\n");//error
}

void check(  Program *program, SymbolTable *table ) {
    Statements *stmts = program->statements;
    while (stmts != NULL) {
        checkstmt(&stmts->first,table);
        stmts = stmts->rest;
    }
}


/***********************************************************************
  Code generation
 ************************************************************************/
void fprint_op ( FILE *target, ValueType op ) {
    switch (op) {
        case MinusNode:
            fprintf(target,"-\n");
            break;
        case PlusNode:
            fprintf(target,"+\n");
            break;
        case MulNode:
            fprintf(target,"*\n");
            break;
        case DivNode:
            fprintf(target,"\\\n");
            break;
        default:
            fprintf(target,"Error in fprintf_op ValueType = %d\n",op);
            break;
    }
}

void fprint_expr ( FILE *target, Expression *expr) {

    if (expr->leftOperand == NULL) {
        switch ( (expr->v).type ) {
            case IdentifierV:
                fprintf(target,"l%s\n",(expr->v).val.id);
                break;
            case IntConst:
                fprintf(target,"%d\n",(expr->v).val.ivalue);
                break;
            case FloatConst:
                fprintf(target,"%f\n", (expr->v).val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d\n",(expr->v).type);
                break;
        }
    }
    else {
        fprint_expr(target, expr->leftOperand);
        if (expr->rightOperand == NULL) {
            fprintf(target,"5k\n");
        }
        else {
            //fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand);
            fprint_op(target, (expr->v).type);
        }
    }
}

void gencode (Program prog, FILE *target) {
    Statements *stmts = prog.statements;
    Statement stmt;

    while (stmts != NULL) {
        stmt = stmts->first;
        switch (stmt.type) {
            case Print:
                fprintf(target,"l%s\n",stmt.stmt.variable);
                fprintf(target,"p\n");
                break;
            case Assignment:
                fprint_expr(target, stmt.stmt.assign.expr);
                /*
                   if(stmt.stmt.assign.type == Int){
                   fprintf(target,"0 k\n");
                   }
                   else if(stmt.stmt.assign.type == Float){
                   fprintf(target,"5 k\n");
                   }*/
                fprintf(target,"s%s\n",stmt.stmt.assign.id);
                fprintf(target,"0 k\n");
                break;
        }
        stmts = stmts->rest;
    }

}


/***************************************
  For our debug,
  you can omit them.
 ****************************************/
void print_expr (Expression *expr) {
    if (expr == NULL)
        return;
    else {
        print_expr(expr->leftOperand);
        switch ((expr->v).type) {
            case IdentifierV:
                printf("%s ", (expr->v).val.id);
                break;
            case IntConst:
                printf("%d ", (expr->v).val.ivalue);
                break;
            case FloatConst:
                printf("%f ", (expr->v).val.fvalue);
                break;
            case PlusNode:
                printf("+ ");
                break;
            case MinusNode:
                printf("- ");
                break;
            case MulNode:
                printf("* ");
                break;
            case DivNode:
                printf("/ ");
                break;
            case IntToFloatConvertNode:
                printf("(float) ");
                break;
            default:
                printf("error ");
                break;
        }
        print_expr(expr->rightOperand);
    }
}

void test_parser ( FILE *source ) {
    Declarations *decls;
    Statements *stmts;
    Declaration decl;
    Statement stmt;
    Program program = parser(source);

    decls = program.declarations;

    while (decls != NULL) {
        decl = decls->first;
        if (decl.type == Int)
            printf("i ");
        if (decl.type == Float)
            printf("f ");
        printf("%s ",decl.name);
        decls = decls->rest;
    }

    stmts = program.statements;

    while (stmts != NULL) {
        stmt = stmts->first;
        if (stmt.type == Print) {
            printf("p %s ", stmt.stmt.variable);
        }

        if (stmt.type == Assignment) {
            printf("%s = ", stmt.stmt.assign.id);
            print_expr(stmt.stmt.assign.expr);
        }
        stmts = stmts->rest;
    }

}
