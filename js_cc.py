import os, sys, os.path, math, random, time, shutil, shlex
from js_parse import *
from js_lex import plexer
from js_ast import *
from js_global import glob

def parse_intern(buf, path):
  glob.g_file = path
  glob.g_lexpos = 0
  glob.g_line = 0
  glob.g_error_pre = None
  glob.g_error = False
  
  ret = parser.parse(buf, lexer=plexer)
  if ret == None:
    return "", None
  
  return ret.gen_js(0), ret

def parse(buf, path):
  try:
    return parse_intern(buf, path)
  except JSError:
    if glob.g_print_stack:
      traceback.print_stack()
      traceback.print_exc()
    
    glob.g_error = True
    return "", None
  
if __name__ == "__main__":
  if len(sys.argv) < 2:
    sys.stderr.write("js_cc.py: no input files\n");
    sys.exit(-1)
    
  path = sys.argv[1]
  file = open(path, "r")
  buf = file.read()
  file.close()
  
  ret = parse(buf, path)
  if glob.g_error:
      sys.exit(-1)
  