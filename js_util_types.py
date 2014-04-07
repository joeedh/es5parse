class SortedDict (dict):
  def __iter__(self):
    return self.keys()
  
  def keys(self):
    keys = []
    for k in dict.keys(self):
      keys.append(k)
    keys.sort()
    return keys

class odict:
  def __init__(self):
    self.items = []
    self.dict = {}
    self.keypos = {}
    
  def __setitem__(self, key, value):
    if key not in self.dict:
      self.items.append(key)
      self.keypos[key] = len(self.items)-1
    
    self.dict[key] = value
    
  def __getitem__(self, key):
    return self.dict[key]
  
  def __delitem__(self, key):
    i = self.keypos[key]
    self.items.pop(i)
    del self.keypos[key]
    del self.dict[key]
  
  def __contains__(self, key):
    return key in self.dict
  
  def __len__(self):
    return len(self.dict)
  
  def __iter__(self):
    return iter(self.items)
  
  def keys(self):
    return list(self.items)
  
  def values(self):
    vs = []
    for k in self.items:
      vs.append(self.dict[k])
    return vs
  
  def __str__(self):
    s = "odict{"
    for i, k in enumerate(self.items):
      if i > 0: s += ", "
      s += "%s: %s" % (str(k), str(self[k]))
    s += "}"
    
    return s
    
  def __repr__(self):
    return str(self)
    