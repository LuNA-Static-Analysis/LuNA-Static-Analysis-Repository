#!/usr/bin/env python3

class FatalError(Exception):
	def __init__(self, message, *args, **kwargs):
		Exception.__init__(self, *args, **kwargs)
		self.message=message

def parse_args(args, valued_short_keys=[], multikeys=[]):
	'''Parse command-line arguments

	Recognizes the following types of arguments:

	-f
	--some-flag
		Key without value
	
	-k value
	--some-key=value
		Key with value
	
	arg
		Standalone argument (does not start with '-')

	By default all '-k' keys are assumed to have no arguments and
	can be combined into a set:
		-klm
	is equivalent to:
		-k -l -m
	
	If the user wants to denote some "short" (single '-') arguments
	to have parameters, they must be enumerated in valued_short_keys
	parameter.

	"Multikeys" are keys which can have more than one occurance, like
	-I and -L keys in gcc. To denote a key to be a multikey add it
	to 'multikeys'.

	If duplicate keys present, KeyError is raised.

	If short key without parameter is present(i.e. is the last argument)
	ValueError is raised.

	Returns (argv, argf, argd) tuple, where argv is a list of
	standalone arguments, argf is a list of keys without arguments
	and argd is a dictionary of keys with arguments. Lists preserve
	order of arguments.
		
	'''
	kvs={}
	flags=[]
	seq=[]
	args=list(args)
	while args:
		arg=args.pop(0)
		arg=arg.strip()
		if arg.startswith('--'):
			if '=' in arg:
				key, val=arg.split('=', 1)
				key=key.strip()
				val=val.strip()
				if key in kvs and key not in multikeys:
					raise KeyError('Key redefinition', key)
				
				if key in multikeys:
					if key not in kvs:
						kvs[key]=[]
					kvs[key].append(val)
				else:
					kvs[key]=val
			else:
				flags.append(arg)
		elif arg.startswith('-'):
			for i in range(len(arg[1:])):
				k='-'+arg[i+1]
				if k in valued_short_keys:
					if i!=len(arg[1:])-1:
						raise KeyError('Valued key not in the end of keys sequence', k)
					if k in kvs and k not in multikeys:
						raise KeyError('Key redefinition', k)
					
					if not args:
						raise ValueError('Key parameter expected', k)
					if k in multikeys:
						if k not in kvs:
							kvs[k]=[]
						kvs[k].append(args.pop(0))
					else:
						kvs[k]=args.pop(0)
				else:
					flags.append(k)
		else:
			seq.append(arg)
	return seq, flags, kvs


def to_String(SRC, path2id, id2path, s):
	res=[]

	id=get_src_id(s, path2id, id2path)
	SRC[id]=s
	for i in range(len(s)):
		res.append((id, None, i, s[i]))
	
	return res

def get_src_id(path, path2id, id2path):
	if path in path2id:
		return path2id[path]
	else:
		assert len(path2id)==len(id2path)
		id=len(path2id)
		path2id[path]=id
		id2path[id]=path
		return id

def to_string(S, start=0):
	return ''.join([C[3] for C in S[start:]])
	
def mark(s, pos):
	ws=''
	for i in range(pos):
		ws+='\t' if s[i]=='\t' else ' '

	if pos<=7 or '\t' in ws:
		return '%s\n%s^--here\n' % (s, ws)
	else:
		return '%s\n%shere--^\n' % (s, ws[:-6])
		
