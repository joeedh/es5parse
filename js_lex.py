import sys, traceback, re

from js_global import glob

import ply.yacc as yacc
import ply.lex as lex
from ply.lex import TOKEN
# List of token names.   This is always required

from types import BuiltinMethodType as PyMethodType, BuiltinFunctionType as PyFunctionType

class StringLit (str):
  pass

res = [
'if', 'then', 'else', 'while', 'do', 'function', 
'var', 'in', 'for', 'new', "return", "continue", "break",
'throw', 'try', 'catch', 'delete', 'typeof', 'instanceof',
'with', 'switch', 'case', 'default', 'yield', 'struct',
'float', 'int', 'const', 'short', 'double', 'char',
'unsigned', 'signed', 'variable', 'byte',
'global', 'inferred', 'native', 'class', 'extends',
'static', 'typed'
]

reserved = {}
for k in res:
  reserved[k] = k.upper()

reserved_lst = []
for k in res:
  reserved_lst.append(k.upper())

"""
to use token states:

states = [
  ("main", "inclusive"|"exclusive") 

add state name as prefix to toke, e.g. t_main_token

stack management:

t.lexer.push_state("main")
t.lexer.pop_state()

get lex tokens between two stored positions (exclude comments and like by not returning them):
  t.lexer.lexdata[t.lexer.code_start:t.lexer.lexpos+1]
]
"""

states = [
  ("incomment", "exclusive"),
  ("instr", "exclusive"),
  ("mlstr", "exclusive")
]

tokens = (
   "MLSTRLIT",
   'COMMENT',
   'INC',
   'DEC',
   'GTHAN',
   'LTHAN',
   'EQUAL',
   'MOD',
   'GTHANEQ',
   'LTHANEQ',
   'NUMBER',
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'LPAREN',
   'RPAREN',
   'SEMI',
   'LBRACKET',
   'RBRACKET',
   'BNEGATE',
   'BAND',
   'BOR',
   'BXOR',
   'LAND',
   'LOR',
   'NOT',
   'ID',
   "NOTEQUAL",
   "STRINGLIT",
   "REGEXPR",
   "ASSIGN",
   "DOT",
   "BACKSLASH",
   "EMPTYLINE",
#   "NL",
   "COMMA",
   "LSBRACKET",
   "RSBRACKET",
   "COLON",
   "QEST",
   "SLASHR",
   "OPENCOM",
   "CLOSECOM",
   "ALL", #only used in states
   "newline",
   "LSHIFT",
   "RSHIFT",
   "LLSHIFT",
   "RRSHIFT",
   "ASSIGNPLUS",
   "ASSIGNMINUS", 
   "ASSIGNDIVIDE", 
   "ASSIGNTIMES",
   "ASSIGNBOR",
   "ASSIGNBAND",
   "ASSIGNBXOR",
   
   "ASSIGNLSHIFT",
   "ASSIGNRSHIFT",
   "ASSIGNRRSHIFT",
   "ASSIGNLLSHIFT",
   "BITINV",   
   "NOTEQUAL_STRICT",
   "EQUAL_STRICT",
   "TLTHAN",
   "TGTHAN",
) + tuple(reserved_lst)

# Regular expression rules for simple tokens
t_ASSIGNPLUS = r'\+='
t_ASSIGNMINUS = r'-='
t_ASSIGNDIVIDE = r'/='
t_ASSIGNTIMES = r'\*='
t_ASSIGNBOR = r'\|='
t_ASSIGNBAND = r'\&='
t_ASSIGNBXOR = r'\^='
t_ASSIGNLSHIFT = r'ENOTHINGNODTHINGNOGTHINGNOHTHING'
t_ASSIGNRSHIFT = r'ENOTHINGNODTHINGNOGTHINGNOHTHINGs'
t_ASSIGNLLSHIFT = r'ENOTHINGNODTHINGNOGTHINGNOHTHING'
t_ASSIGNRRSHIFT = r'ENOTHINGNODTHINGNOGTHINGNOHTHING'

t_BITINV = r'\~'
t_LSHIFT = r'\^\^'
t_RSHIFT = r'\^\^'
t_LLSHIFT = r'\^\^\^'
t_RRSHIFT = r'\>\>\>'

t_BAND = r'&'
t_BOR = r'\|'
t_BXOR = r'\^'
t_LAND = r'&&'
t_LOR = r'\|\|'
t_NOT = r'\!'
t_NOTEQUAL_STRICT = r'\!=='
t_EQUAL_STRICT = r'==='
t_EQUAL = r'=='
t_NOTEQUAL = r'\!='
t_INC = r'\+\+'
t_DEC = r'--'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MOD     = r'%'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\{'
t_RBRACKET = r'\}'
t_ASSIGN = r'='
t_DOT = r'\.'
t_BACKSLASH = r'\\'
t_COMMA = r','
t_LSBRACKET = r'\['
t_RSBRACKET = r'\]'
t_COLON = r'\:'
t_SEMI = r';'
t_QEST = r'\?'
t_ALL = r'ENOTHINGNODTHINGNOGTHINGNOHTHING'
t_TGTHAN = r'sdfwetreENOTHINGNODTHINGNOGTHINGNOHTHINGytery' #ensure we never match anything

def t_GTHANEQ(t):
  r'\>='
  return t
  
def t_LTHANEQ(t):
  r'\<='
  return t
  
def t_GTHAN(t):
  r'\>'
  global in_lthan_test, gthan_ignores
  
  if in_lthan_test: 
    t.type = "TGTHAN"
    return t
  
  if t.lexpos in gthan_ignores:
    return
    
  if t.lexpos in tgthan_lexposes:
    t.type = "TGTHAN"
    return t
  
  if t.lexpos < len(t.lexer.lexdata)-2 and \
     t.lexer.lexdata[t.lexpos+1] == ">" and \
     t.lexer.lexdata[t.lexpos+2] == "=":
    t.type = "ASSIGNRSHIFT"
    t.value = ">>="
    gthan_ignores.add(t.lexpos+1)
    gthan_ignores.add(t.lexpos+2)
    t.lexer.lexpos += 2
  elif t.lexpos < len(t.lexer.lexdata)-3 and \
     t.lexer.lexdata[t.lexpos+1] == ">" and \
     t.lexer.lexdata[t.lexpos+2] == ">" and \
     t.lexer.lexdata[t.lexpos+3] == "=":
    t.type = "ASSIGNRRSHIFT"
    t.value = ">>>="
    gthan_ignores.add(t.lexpos+1)
    gthan_ignores.add(t.lexpos+2)
    t.lexer.lexpos += 3
  elif t.lexpos < len(t.lexer.lexdata)-2 and \
     t.lexer.lexdata[t.lexpos+1] == ">" and \
     t.lexer.lexdata[t.lexpos+2] == ">":
    t.type = "RRSHIFT"
    t.value = ">>>"
    gthan_ignores.add(t.lexpos+1)
    gthan_ignores.add(t.lexpos+2)
  elif t.lexpos < len(t.lexer.lexdata)-1 and t.lexer.lexdata[t.lexpos+1] == ">":
    t.type = "RSHIFT"
    t.value = ">>"
    gthan_ignores.add(t.lexpos+1)
    
  return t

in_lthan_test = False
tgthan_lexposes = set()
gthan_ignores = set()
lthan_ignores = set()

def t_LTHAN(t):
  r'\<'
  global in_lthan_test, lthan_ignores
  
  if in_lthan_test:
    t.type = "TLTHAN"
    return t
  
  if t.lexpos in lthan_ignores:
    return
    
  in_lthan_test = True
  
  import js_parse as jsp
  
  s = ""
  lvl = 0
  i = t.lexpos
  lexdata = t.lexer.lexdata
  ret = None
  while i < len(lexdata):
    if lexdata[i] == ">":
      lvl += 1
    elif lexdata[i] == "<":
      lvl -= 1
    if lexdata[i] in ["\n", "\r", ";"]:
      ret = False
      break
      
    s += lexdata[i]
    if lvl == 0: break
    i += 1
  
  if t.lexpos < len(t.lexer.lexdata)-2 and \
   t.lexer.lexdata[t.lexpos+1] == "<" and \
   t.lexer.lexdata[t.lexpos+2] == "=":
    t.type = "ASSIGNLSHIFT"
    t.value = "<<="
    lthan_ignores.add(t.lexpos+1)
    lthan_ignores.add(t.lexpos+2)
    t.lexer.lexpos += 2
  elif t.lexpos < len(t.lexer.lexdata)-3 and \
     t.lexer.lexdata[t.lexpos+1] == "<" and \
     t.lexer.lexdata[t.lexpos+2] == "<" and \
     t.lexer.lexdata[t.lexpos+3] == "=":
    t.type = "ASSIGNLSHIFT"
    t.value = "<<<="
    lthan_ignores.add(t.lexpos+1)
    lthan_ignores.add(t.lexpos+2)
    lthan_ignores.add(t.lexpos+3)
    t.lexer.lexpos += 3
  elif t.lexpos < len(t.lexer.lexdata)-2 and t.lexer.lexdata[t.lexpos+1] == "<" and t.lexer.lexdata[t.lexpos+2] == "<":
    t.type = "LLSHIFT"
    t.value = "<<<"
    lthan_ignores.add(t.lexpos+1)
    lthan_ignores.add(t.lexpos+2)
  elif t.lexpos < len(t.lexer.lexdata)-1 and t.lexer.lexdata[t.lexpos+1] == "<":
    t.type = "LSHIFT"
    t.value = "<<"
    lthan_ignores.add(t.lexpos+1)

  in_lthan_test = False

  return t

class _Rd:
  i = 0
  st = 0

_rd = _Rd()
_rd.i = 0
_rd.st = ""

#this function generates the regular expression used to parse
#JavaScript regular expression literals.
#
#it works by converting stupid ECMAScript's 
#"production grammar" for lexical scanners to regexpr.  
#the standard implies that its grammar is DFA-compatible, 
#and it seems to work.
#
#how I hate self-righteous engineer innuendo such as this. . .

#"""
def gen_re():
  def expect(s):
    if s in [".", "+", "(", "[", "]", ")", "*", "^", "\\"]:
        s = "\\" + s
    
    s = r'(((?<!\\)|(?<=\\\\))' + s + ")"
    return s
      
  def consume_ifnot(s):
    
    s2 = "[^"
    if type(s) == list:
      for s3 in s:
        if s3 in ["+", "\\", "(", "[", "]", ")", "*", "^"]:
          s3 = '\\' + s3
        
        s2 += s3
    else:
      if s in ["+", "\\", "(", "[", "]", ")", "*", "^"]:
        s = '\\' + s
      s2 += s
      
    s2 += "]"
    return "%s" % s2
    
  def NonTerm(extra=[]):
    return consume_ifnot(["\\n", "\\r"] + extra)
  
  def Char():
    return "(%s|%s|%s)" % (NonTerm(["\\", "/", "["]), BackSeq(), Class())
    
  def FirstChar():
    return "(%s|%s|%s)" % (NonTerm(["*", "\\", "/", "["]), BackSeq(), Class())
  
  def empty():
    return "(\b|\B)"
    
  def Chars():
    return "(%s)*" % Char()
  
  def BackSeq():
    return expect('\\') + NonTerm()
  
  def ClassChar():
    return "(%s|%s)" % (NonTerm(["]", "\\"]), BackSeq())
  
  def ClassChars():
    return ClassChar() + "+"
    
  def Class():
    return "(" + expect("[") + ClassChars() + expect("]") + ")"
  
  def Flags():
    return "[a-zA-Z]*"
    
  def Body():
    return "(" + FirstChar() + Chars() + ")"
    
  def Lit():
    #what is with this stupid grammar?
    #it's supposed to hook into the lexical
    #scanner, but it isn't working without
    #these idiot hacks, like the unrolling of
    #this variable-width lookbehind I'm doing here.
    
    def g(c, n):
      s = "(?<=[([\=,]"
      
      if n != 0:
        s += "[%s]{%d}" % (c, n)
      
      s += ")"
      return s
    
    pats = (
      g(" ", 0),
      g(" ", 1),
      g(" ", 2),
      g(" ", 3),
      g(" ", 4),
      g(" ", 5),
      g(" ", 6),
      g(r"\t", 1),
      g(r"\t", 2),
      g(r"\t", 3),
      g(r"\t", 4),
      g(r"\t", 5),
    )
    
    pat = ""
    for i, p in enumerate(pats):
      if (i != 0):
        pat += "|"
      pat += p
      
    pat = "((" + pat + ")" + expect("/") + ")"
    pat += Body() + expect("/") 
    pat = pat + "(?!/)" + Flags()
    return pat
    
  return Lit()

#print(gen_re())
re1 = gen_re()
 
str1 = r"/ 3) * 3;              //"
#print("\n" + str(str1))

m = re.match(re1, str1)
if m != None:
  s = m.span()
#  print(str1[s[0]: s[1]])
else:
  pass

#sys.exit()
#"""

t_REGEXPR = gen_re() #r'(((?<!\\)|(?<=\\\\))/)(([^\n\r\*\\/\[]|(((?<!\\)|(?<=\\\\))\\)[^\n\r]|((((?<!\\)|(?<=\\\\))\[)([^\n\r\]\\]|(((?<!\\)|(?<=\\\\))\\)[^\n\r])+(((?<!\\)|(?<=\\\\))\])))(([^\n\r\\/\[]|(((?<!\\)|(?<=\\\\))\\)[^\n\r]|((((?<!\\)|(?<=\\\\))\[)([^\n\r\]\\]|(((?<!\\)|(?<=\\\\))\\)[^\n\r])+(((?<!\\)|(?<=\\\\))\]))))*)(((?<!\\)|(?<=\\\\))/)(?!/)[a-zA-Z]*'

#t_STRINGLIT = r'".*"'
strlit_val = StringLit("")
start_q = 0

t_mlstr_ignore = ''

def t_MLSTRLIT(t):
  r'"""';
  
  global strlit_val;
  t.lexer.push_state("mlstr");
  strlit_val = StringLit("")
  
def ml_escape(s):
  i = 0
  lastc = 0
  
  nexts = False
  excl = ['"', "'"]
  
  s2 = ""
  while i < len(s):
    c = s[i]
    
    if nexts:
      nexts = False
      s2 += c
      i += 1
      continue
    
    if c == "\\":
      nexts = True
      s2 += c
      i += 1
      continue
    
    if c in ["'", '"']:
      s2 += "\\"
    
    if c == "\n": c = "\\n"
    if c == "\r": c = "\\r"
    
    s2 += c
    i += 1
  return s2
      
def t_mlstr_MLSTRLIT(t):
  r'"""' #(""")|(\\""")';
  
  global strlit_val;
  
  if ("\\" in t.value):
    strlit_val = StringLit(strlit_val + t.value);
    return
    
  str = StringLit(ml_escape(strlit_val))
  #str = StringLit(str)
  
  t.strval = t.value;
  t.value = StringLit('"' + str + '"');
  t.type = "STRINGLIT"

  t.lexer.pop_state();
  return t;

def t_mlstr_ALL(t):
  r'(.|[\n\r\v])'
  
  global strlit_val
  if 1: #t.lexer.lexdata[t.lexpos:t.lexpos+3] != '"""':
    strlit_val = StringLit(strlit_val + t.value)
  
  t.lexer.lineno += t.value.count('\n')  
  
def t_STRINGLIT(t):
  r'\"|\''
  global strlit_val, start_q
  
  start_q = t.value
  strlit_val = StringLit("")
  t.lexer.push_state("instr")
  
def t_instr_STRINGLIT(t):
  r'\"|\'|\\"|\\\''
  global strlit_val, start_q
  
  if "\\" in t.value or start_q not in t.value:
    strlit_val = StringLit(strlit_val + t.value)
    return
  
  t.lexer.pop_state()
  t.strval = t.value
  t.value = StringLit(start_q + strlit_val + start_q)
  return t

def t_instr_ALL(t):
  r'([^"\']|(\\\'\\"))+';

  global strlit_val
  strlit_val = StringLit(strlit_val + t.value)
  
  t.lexer.lineno += '\n' in t.value
  
def t_SLASHR(t):
  r'\r+'

comment_str = ""
comment_startline = -1
def t_OPENCOM(t):
  r'/\*'
  
  global comment_str, comment_startline
  t.lexer.push_state("incomment")
  comment_str = t.value
  comment_startline = t.lexer.lineno if t.lexer.lineno != -1 else 0
  
def t_incomment_CLOSECOM(t):
  r'\*/'
  
  global comment_str
  comment_str += t.value
  t.lexer.pop_state()
  
  i = t.lexer.lexpos
  ld = t.lexer.lexdata
  while i < len(ld):
    if ld[i] not in [" ", "\t", "\n", "\r"]: break
    if ld[i] == "\n":
      comment_str += "\n"
      break
    i += 1
  t.lexer.comment = comment_str
  t.lexer.comments[t.lexer.comment_id] = [comment_str, comment_startline]
  t.lexer.comment_id += 1
  
def t_incomment_ALL(t):
  r'[^/\*]+';
  
  global comment_str
  comment_str += t.value
  
  t.lexer.lineno += t.value.count("\n")

# Error handling rule
def t_incomment_error(t):
    #print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

#def t_incomment_newline(t):
#    r'\n+'
#    t.lexer.lineno += len(t.value)
    
# Error handling rule
def t_instr_error(t):
    print("Illegal character in string '%s'" % t.value[0])
    t.lexer.skip(1)

def t_mlstr_error(t):
    print("Illegal character in multiline string '%s'" % t.value[0])
    t.lexer.skip(1)
  
def t_COMMENT(t):
  r'//.*\n'
  global comment_startline, comment_str
  #r'(/\*(.|\n|\r)*\*/)|'

  t.lexer.comment = t.value
  t.lexer.comments[t.lexer.comment_id] = [t.lexer.comment, t.lexer.lineno]
  t.lexer.comment_id += 1
  
  t.lexer.lineno += t.value.count("\n")

@TOKEN(r'[\$a-zA-Z_][\$a-zA-Z_0-9]*')
def t_ID(t):
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    
    return t

class HexInt(int):
  pass
        
# A regular expression rule with some action code
def t_NUMBER(t):
    r'(0x[0-9a-fA-F]+)|((\d|(\d\.\d+))+(e|e\-|e\+)\d+)|(\d*\.\d+)|(\d+)'
    
    t.strval = t.value
    
    if "." not in t.value and "e" not in t.value and "x" not in t.value:
      t.value = int(t.value)
    elif "x" in t.value:
      t.value = HexInt(t.value, 16)
    else:
      t.value = float(t.value)
    
    return t

def t_EMPTYLINE(t):
  r'\n[ \t]\n'
  t.lexer.lineno += t.value.count("\n")
  
# this rule finds newlines not preceded by backslashes, to handle
#multi-line statements
"""
def t_NL(t):
  r'(?<!\\)\n'
  
  #t.lexer.lineno += 1
  
  #if "\\" not in t.value:
  #  return t
"""

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
      
# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'
t_instr_ignore  = ''
t_incomment_ignore = ''

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
class LexWithPrev():
  def __init__(self, lexer):
    self.lexer = lexer
    self.prev = None
    self.cur = None
    self.lineno = 0
    self.lexpos = 0
    self.peeks = []
    self.rawlines = []
    self.prev_lexpos = 0

    self.comment = None
    self.comment_id = 0
    self.comments = {}
    
    lexer.comment = None
    lexer.comment_id = 0
    lexer.comments = {}
    
  def next(self):
    t = self.token()
    if t == None: raise StopIteration
    
    return t
  
  def peek(self):
    p = self.lexer.token()
    
    if p == None: return p
    
    self.comments = self.lexer.comments
    self.comment = self.lexer.comment
    self.commend_id = self.lexer.comment_id
    
    p.lineno = self.lexer.lineno;
    p.lexer = self;
    
    self.peeks.append([p, self.lexer.lexpos])
    return p
  
  def token_len(self, t):
    if t.type in ["NUMBER", "STRINGLIT"]:
      return len(t.strval)
    else:
      return len(t.value)
    
  def token(self):
    self.prev = self.cur;
    if len(self.peeks) > 0:
      self.prev_lexpos = self.lexpos
      
      self.cur, self.lexpos = self.peeks.pop(0)
      
      self.cur.lexpos = self.lexpos
      self.cur.prev_lexpos = self.prev_lexpos
      return self.cur
    
    self.cur = self.lexer.token()
    
    if self.cur != None:
      self.cur.lexer = self
      self.cur.lineno = self.lexer.lineno
      self.cur.prev_lexpos = self.prev_lexpos
      self.comments = self.lexer.comments
      self.comment = self.lexer.comment
      self.commend_id = self.lexer.comment_id
    else:
      #reset state
      """
      global in_lthan_test, tgthan_lexposes, gthan_ignores, lthan_ignores
      tgthan_lexposes = set()
      gthan_ignores = set()
      lthan_ignores = set()
      in_lthan_test = False;
      """
      pass
      
    self.lineno = self.lexer.lineno
    self.prev_lexpos = self.lexpos;
    self.lexpos = self.lexer.lexpos
    
    return self.cur
    
  def input(self, data):
    self.comment_id = 0
    if not in_lthan_test:
      global tgthan_lexposes, gthan_ignores, lthan_ignores
      tgthan_lexposes = set()
      gthan_ignores = set()
      lthan_ignores = set()

    self.lineno = self.lexer.lineno = 0
    
    """
    #ensure all return statements end with ";"
    d2 = ""
    rlen = len("return")
    
    def has_word(data, word):
      return i >= len(word) and data[i-len(word):i] == word
      
    i = 0
    while i < len(data):
      if has_word(data, "return") or has_word(data, "continue"):
        #find newline
        adds = True
        
        b1 = 0
        b2 = 0
        i2 = i
        while i < len(data):
          if data[i] == "\n": break
          if data[i] == "[": b1 += 1
          elif data[i] == "]": b1 -= 1
          if data[i] == "{": b2 += 1
          elif data[i] == "}": b2 -= 1
          d2 += data[i]
          i += 1
        
        adds = d2.strip().endswith("return") or d2.strip().endswith("continue"); #b1 == 0 and b2 == 0
        
        if adds and not d2.strip().endswith(";"):
          d2 += ";"
        d2 += "\n"
      else:
        d2 += data[i]
      
      i += 1
      
    data = d2
    #"""
    
    self.lexer.lineno = self.lineno
    self.lexer.input(data)
    self.rawlines = data.replace("\r\n", "\n").split("\n")
  
  def set_lexpos(self, lexpos):
    self.lexpos = lexpos
    self.lexer.lexpos = lexpos
  
  def push(self, tok):
    self.peeks.insert(0, [tok, tok.lexpos])
  
  def push_state(self, state):
    self.lexer.push_state(state)
  
  def pop_state(self):
    self.lexer.pop_state()
    
plexer = LexWithPrev(lex.lex())
tmp_lexer = LexWithPrev(lex.lex())
plexer.lexer.comments = {}
plexer.lexer.comment = None
plexer.lexer.comment_id = 0
