module astdump;

// TODO: remove unnec. imports
import std.stdio, std.path, std.array, std.string, std.algorithm, std.conv;
import file=std.file;
import util;
import ast.lexer, ast.parser, ast.expression, ast.declaration, ast.error, help;
import astopt;
import options, ast.scope_, ast.semantic_, ast.summarize;

struct ASTDumper{
    FunctionDef[string] functions;
    this(FunctionDef[string] functions){
        this.functions = functions;
    }

    void dumpAST(){
        foreach (name, ops; this.functions){
            writeln(name);
        }
    }
}