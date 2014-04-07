from random import random, seed
import time

def traverse(n, ntype, func, use_depth=False, 
             exclude=[], copy_children=False, 
             use_scope=False, scope=None, depth=0):
  if scope == None: scope = {}
  scope = handle_scope(n, scope)
  
  if type(exclude) != list and type(exclude) != tuple and issubclass(exclude, Node):
    exclude = [exclude]
  
  if type(n) in exclude and depth != 0:
    return

  if copy_children:
    cs = n[:]
  
  if type(n) == ntype: 
    if use_depth and use_scope:
      func(n, scope, depth)
    elif use_scope:
      func(n, scope)
    elif use_depth:
      func(n, depth)
    else:
      func(n)
  
  if not copy_children:
    cs = n.children
    
  for c in cs:
    traverse(c, ntype, func, use_depth, exclude, copy_children, use_scope, scope, depth+1)
    
class NodeVisit:
  """
  visitor pattern.  example usage:
  
  class SomeVisit(NodeVisit):
    #the tlevel param isn't really used anymore,
    #but could in the future be used for 
    #things like stack pushing/popping in 
    #bytecode generators.
    
    def BinOpNode(self, node, scope, traverse, tlevel):
      traverse(node[0], scope, tlevel)
      traverse(node[1], scope, tlevel)
    
    def FunctionNode(self, node, scope, traverse, tlevel):
      #copy scope
      scope = dict(scope)
      
      for c in node[0]:
        scope[c.val] = c
      for c in node[1:]:
        traverse(c, scope, tlevel)
  """
  def __init__(self, required_nodes=[]):
    self.required_nodes = list(required_nodes)
  
  def traverse(self, node, scope=None, tlevel=0):
    if scope == None and tlevel != 0:
      raise RuntimeError("NodeVisit.traverse called without scope")
      
    if scope == None:
      scope = {}
      
    if scope == None: scope = NodeScope()
    
    typestr = type(node).__name__
    if not hasattr(self, typestr) and typestr in self.required_nodes:
      raise RuntimeError("Unimplemented node visit for node type %s", typestr)
    
    if not hasattr(self, typestr):
      for c in node.children:
        self.traverse(c, scope, tlevel)
    else:
      getattr(self, typestr)(node, scope, self.traverse, tlevel)
