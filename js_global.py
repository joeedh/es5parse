import argparse
import os, sys, struct, io, random, os.path, types, re

glob_cmd_help_override = {
}
glob_cmd_short_override = {}

glob_cmd_parse_exclude = set(["infile", "outfile", "nfile"])
glob_cmd_advanced = set(["g_error", "g_line", "g_file", "g_tried_semi", "g_error_pre", "g_lexpos", "g_clear_slashr", "g_lexer"])
glob_cmd_exclude = set(["g_comment_line", "g_comment", "g_comment_id", "g_lexer", "g_error_pre", "g_outfile", "g_lines", "g_lexdata"])
glob_long_word_shorten = {"generators": "gens", "error": "err", "warnings": "warn", "production": "prod"}

gcs = glob_cmd_short_override

def argv_to_argline():
  s = ""
  for i in range(len(sys.argv)-1):
    s += sys.argv[i] + " "
  return s

glob_defaults = {}
dont_set = set(["expand", "destroy", "add", "force", 
                "print", "process", "pre", "do", "exit"])

class AbstractGlob:
    __arg_map = {}
    
    def reset(self):
      self.load(Glob(), _debug=False)
      for attr in glob_defaults:
        setattr(self, attr, glob_defaults[attr])
      
    def copy(self):
      g = Glob()
      for attr in dir(self):
        if attr.startswith("__"): continue
        p = getattr(self, attr)
        if type(p) == type(self.copy): continue
        
        setattr(g, attr, p)

      return g
      
    def load(self, g, _debug=True):
      for attr in dir(self):
        if attr.startswith("__"): continue
        
        p = getattr(g, attr)
        if type(p) == type(self.load): continue
        
        setattr(self, attr, p)
      return g
    
    def add_args(self, cparse, js_cc_mode=True):
      global glob_cmd_exclude, glob_cmd_advanced, glob_cmd_help_override
      
      def gen_cmd(attr):
        s = attr[2:].replace("_", "-")
        
        for k in glob_long_word_shorten.keys():
          v = glob_long_word_shorten[k]
          s = re.sub(k, v, s)
          
        if attr in glob_cmd_help_override:
          arg_help = glob_cmd_help_override[attr]
        else: 
          arg_help = s.replace("_", " ").replace("-", " ")
        
        val = getattr(self, attr)
        atype = None
        metavar = None
        act = "store"
        if type(val) == bool:
          if getattr(self, attr) == True:
            act = "store_false"
            found = False
            for d in dont_set:
              if s.startswith(d):
                arg_help = "don't " + arg_help
                found = True
                break
                
            if not found:
              arg_help = "no " + arg_help
              
            s = "no-"+s 
          else:
            act = "store_true"
        else:
          if type(val) == int:
            act = "store"
            atype = int
            metavar = 'i'
          elif type(val) == str:
            act = "store"
            atype = str
            
        arg_help = arg_help[0].upper() + arg_help[1:]
        
        if attr in glob_cmd_short_override:
          short = glob_cmd_short_override[attr]
        else:
          short = gen_short(s)
        
        self.__arg_map[s] = attr
        self.__arg_map[short] = attr
        return short, s, act, atype, metavar, arg_help
        
      def gen_short(string):
        cmps = string.split("-")
        s2 = ""
        i = 0
        while i < len(cmps) and (s2 == "" or s2 in shortset):
          if len(cmps[i]) == 0: 
            i += 1
            continue
            
          s2 += cmps[i][0]
          i += 1
        
        s3 = s2
        i = 1
        while s2 in shortset:
          s2 = "%s%d" % (s3, i)
          i += 1
          
        shortset.add(s2)
        return s2
      
      if js_cc_mode:
        cparse.add_argument("infile", help="input files")
        if len(sys.argv) > 2 and not sys.argv[2].startswith("-"):
          cparse.add_argument("outfile", default="", help="input files")
        else:
          cparse.add_argument("outfile", default="", nargs="?", help="input files")
      
      shortset = set('a')
      adv_attrs = []
      for attr in dir(self):
        if not attr.startswith("g_"): continue
        if attr.startswith("__"): continue
        if attr in glob_cmd_exclude: continue
        if attr in glob_cmd_advanced:
          adv_attrs.append(attr)
          continue
          
        short, long, action, ctype, metavar, help = gen_cmd(attr)
        if ctype != None:
          cparse.add_argument("-"+short, "--"+long, action=action, type=ctype, metavar=metavar, help=help)
        else:
          cparse.add_argument("-"+short, "--"+long, action=action, help=help)
          
      if "adv" in argv_to_argline():
        subparsers = cparse.add_subparsers(title="Advanced Commands")
        adv_parse = subparsers.add_parser('adv', add_help = False)
      
        for attr in adv_attrs:
          short, long, action, ctype, metavar, help = gen_cmd(attr)
          
          if ctype != None:
            adv_parse.add_argument("-"+short, "--"+long, action=action, type=ctype, metavar=metavar, help=help)
          else:
            adv_parse.add_argument("-"+short, "--"+long, action=action, help=help)

        adv_parse.add_argument("--help", action="help", help="Print this message")
          
    def parse_args(self, cparse, args):
      for k in args.__dict__:
        if k in glob_cmd_parse_exclude: continue
        
        attr = self.__arg_map[k.replace("_", "-")]
        val = getattr(args, k)
        if val != None and val != getattr(self, attr):
          setattr(self, attr, val)
          glob_defaults[attr] = val

class Glob(AbstractGlob):
  g_file = ""
  g_outfile = ""
  g_error_pre = None
  g_lexpos = 0
  g_line = 0
  g_error = False
    
glob = Glob()
