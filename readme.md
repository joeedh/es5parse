#Python/PLY JS parser

A fully-functional JS parser, written in Python and PLY (a LALR compiler-compiler for Python).  
It even parses difficult regexpr cases (something that is much more difficult than the ECMAScript
spec implies, as to properly implement it in a regexpr lexical scanner requires manual unrolling
of a conditional grammar loop).

This is based off of an in-house ES6->ES5 compiler, which you can also find on my github repository.

##Requirements
* Python3.x
* PyPLY: http://www.dabeaz.com/ply/

##Basic Usage
You can validate JS source files with:

    js_cc.py [path-to-file]

Since this is a reference implementation,
the parser doesn't actually do anything other than parse JS files into AST trees (see js_ast.py
for the AST node types).

