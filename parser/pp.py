#!python3

import sys, os, re, json
import common
from common import *

_prog=os.path.split(sys.argv[0])[1]
HELP_MESSAGE=sys.argv[0]+'''

NAME
	''' + _prog + ''' - LuNA preprocessor

SYNOPSIS
	''' + _prog + ''' FILE [OPTION]...
	''' + _prog + r''' --help

DESCRIPTION
	Preprocesses LuNA program. Preprocessing comprises the following
	stages:
	    *) Extraction of foreign blocks, comments and strings
	    *) Definitions substitutions

OPTIONS
	Options can also be placed before FILE argument, but be careful
	with valued keys (-o, etc.).

	--help
	    Show this info and terminate.

	-o <path>
	--out-path=<path>
		Path for compilation result

	-b <path>
	--blocks-path=<path>
		Path for saving blocks (in JSON format).
	
	--text-info=<path>
	    Save text positioning information into <path>.
	
	--path=<list:of:paths>
	    Colon-separated list of paths to seek for luna programs. By
	    default current dir is used. Backslash can be used to define
	    literal colon.

	-D <key>
	-D <key>=<value
	    Define preprocessor variable (similar to gcc).


SYNTAX
	First line if input file may optionally be an interpreter
	specification. If present it is cut off by preprocessor. The rest
	of input file is a sequence of characters. There are special
	character sequences, that denote special sections: string
	constants, comments, directives and foreign blocks.
	The sections are never overlapping (e.g. no comments can start
	inside a sting constant, etc.). 
	Each section has a starting rule, which is only significant outside
	other sections, and a termination rule, which is only significant
	inside the section. No section must reach end of file.

	String constant
	    Starts with '"' symbol. Ends with '"' symbol. Inside a string
	    constant backslash ('\') symbol removes special meaning from
	    the following next symbols: backslash ('\'), quote('"'),
	    newline ('\n'). Here are valid string constants examples:

	        "simple string"
	        "string \"with\" quotes"
	        "multi-
line string"
			"single-li\
ne string"
	        "backslash \\ disables backslash, thus string ends: \\"

	One-word-comment
	    Starts with '`'. Ends with a whitespace or a newline.

	End-of-line comment
		Starts with doube slash ('//'). Ends with newline ('\n').
	    Backslash disables special meaning of the following symbols:
	    newline ('\n') and backslash ('\'). Here are valid end-of-
	    line comments examples:

	        // simple comment
	        // trailing backslash is disabled: \\
	        // this comment passes \
	               to the next line

		End-of-line comments are ignored as if there were none.

	Multi-line comment
		Starts with '/*' and ends with '*/'. Multiline comments are
	    ignored as if there were none.

	Directives
	    Preprocessor directives are similar to C preprocessor, but with
	    some peculiarities.
		
	        #define <name>[(<args>)]<body>
	            Macro definition. Body definition ends with newline
	            (unles backslashed). Arguments are optional, they are
	            substituted with $<name> expression. Examples:
	                #define greet(arg1, arg2) Hello, $arg1 \
	                    and $arg2.
	                #define N 10
	                #define FLAG

	        #undef name (EXPERIMENTAL!)
	            Undefine previously defined macro. Warning: macros
	            are undefined before macros apply, so it is only
	            useful for #ifdef/ifndef conditional blocks

	        #include "path/to/file"
	            Substitute external file text into current file. The
	            file substituted is also preprocessed. Unless path is
	            absolute, the file is looked up in current directory or
	            directories specified with --path option (left-to-right).

	        #ifdef <name> ... [#else ...] #endif
	        #ifndef <name> ... [#else ...] #endif
	            Conditional substitution of block. May be multi-line.

	    Definitions substitutions are controlled by $<name> expressions
	    with arguments (if required). Arguments are separated by comma
	    (',') unless backslashed, and the list is terminated by right
	    bracket (')') unless backslashed. Examples:

	        $N // gives 10
	        $greet(John, Marry) // gives: Hello, John
	            and  Marry

	    Definitions are substituted in an unspecified order iteratively,
	    until no more definitions are appliable.

	Foreign block
	    Foreign block is a sequence of characters, which are interpreted
	    by some external instance, so it is cut-off as is. A foreign
	    block starts with '${<TERM}' sequence, where TERM is a custom
	    sequence of symbols, which can be different for different blocks.
	    Block ends with TERM sequence, defined at block start.
		
		There is a special type of block, started with '${{' and
		terminated with '$}}'. This block implies insertion '{' and '}'
		at the begginning and the ending of the block contents
		correspondently. It is useful for C++ blocks,

		Here are valid blocks examples:

	        ${<END}some textEND // 'some text'
			${{some text$}} // '{some text}'
	        ${<!}Block may
	        cover multiple lines and backslash
	        has no special power: \!

	    All foreign blocks are numbered in the order of appearance
	    (starting with zero).

	    Foreign blocks are saved in JSON format as a list of dictionaries:
	        [{
	            "path": "path/to/file/with/block",
	            "line": <line_number>,
	            "pos": <position_in_line>,
	            "body": "single string with block body"
	        }, ...]


	As the result of preprocessing all foreign blocks, comments and
	definitions are removed from the source, and after all definitions
	substitution is done the result is saved as output. The output is
	supplied with info on original text slices positions as a separate
	file in JSON format as a list of records:
	    [{
	        "line": <line_num>,
	        "post": <pos in line>,
	        "length": <line_num>,
	        "src_path": "path/to/source",
	        "src_line": <line_num>,
	        "src_pos": <line_num>,
	        "macros": [{
	            <TODO>: some macro substitution info
	        }, ...]
	    }, ...]


	Other preprocessor directives
	    #include "path/to/file"
	        Put contents of the specified file instead of this directive
'''

ID=r'([_a-zA-Z][_a-zA-Z0-9]*)'
PARAMS=r'\(([^)]*)\)'

class FatalError(Exception):
	def __init__(self, *args, **kwargs):
		Exception.__init__(self, *args, **kwargs)
		self.message=str(self)

class PreprocessorError(Exception):
	def __init__(self, message, path, line_num, line_pos, *extras):
		self.Message=message
		self.Path=path
		self.LineNum=line_num
		self.LinePos=line_pos
		self.Extras=extras
		if line_num is not None:
			Exception.__init__(self,
				'preprocessor error: %s at %s:%d:%d' % (message, path,
				(line_num+1), (line_pos+1)))
		else:
			Exception.__init__(self,
				'preprocessor error: %s at %s:%d' % (message, path,
				(line_pos+1)))

class CondBlockEnd:
	def __init__(self, body, cur):
		self.Body=body
		self.cur=cur

def startswith(s, S, start=0):
	assert type(s) in [str, str]
	assert type(S) == list
	for i in range(len(s)):
		if start+i>=len(S):
			return False
		if S[start+i][3]!=s[i]:
			return False
	return True

path=None
gsrc=None


def markerror(msg, C):
	res=''
	if C[1] is not None:
		res+='%s at %s:%d:%d:\n' % (msg, id2path[C[0]], C[1]+1, C[2]+1)
	else:
		res+='%s (not in file):\n' % msg

	gsrc=SRC[C[0]]
	if C[1]==len(gsrc.split('\n')):
		assert C[2]==0
	else:
		if C[1] is None:
			res+=common.mark(SRC[C[0]], C[1])
		else:
			res+=common.mark(gsrc.split('\n')[C[1]], C[2])
	return res

def pperror(msg, C, *comment):
	return PreprocessorError(msg, C[0], C[1], C[2], *comment)

def get_interpreter(src, cur):
	assert startswith('#!', src, cur)
	
	interpreter=[]
	start=cur+2
	while cur<len(src) and src[cur][3]!='\n':
		interpreter.append(src[cur])
		cur+=1
	cur+=1 # skip '\n'

	return interpreter, cur

def get_slcomment(src, cur):
	assert startswith('//', src, cur)
	
	comment=[]
	start=cur+2
	while cur<len(src) and src[cur][3]!='\n':
		if src[cur][3]=='\\':
			comment.append(src[cur])
			cur+=1
			if cur>=len(src):
				raise pperror("'\\' at end of file", src[cur-1])
		comment.append(src[cur])
		cur+=1
	return comment, cur

def get_mlcomment(src, cur):
	assert startswith('/*', src, cur)

	start=cur
	cur+=2
	comment=[]
	while cur<len(src) and not startswith('*/', src, cur):
		comment.append(src[cur])
		cur+=1
	
	assert cur<=len(src)
	if cur==len(src):
		raise pperror('multiline comment reaches end of file', src[start])
	cur+=2
	return comment, cur

def get_owcomment(src, cur):
	assert startswith('`', src, cur)
	
	comment=[]
	start=cur+1
	cur+=1
	while cur<len(src) and (re.match('[_0-9a-zA-Z]', src[cur][3]) \
			is not None or src[cur][3]=='\\'):
		if src[cur][3]=='\\':
			comment.append(src[cur])
			cur+=1
			if cur>=len(src):
				raise pperror("'\\' at end of file", src[cur-1])
		comment.append(src[cur])
		cur+=1
	return comment, cur

def get_string(src, cur):
	assert src[cur][3]=='"'

	start=cur
	string=[src[cur]]
	cur+=1
	while cur<len(src) and src[cur][3]!='"':
		if src[cur][3]=='\\':
			string.append(src[cur])
			cur+=1
			if cur>=len(src):
				raise pperror("'\\' at end of file", src[cur-1])
		string.append(src[cur])
		cur+=1
	assert cur<=len(src)
	if cur==len(src):
		raise pperror('unmatched quote (\'"\')', src[start])
	string.append(src[cur])
	cur+=1
	return string, cur

def get_def_body(src, cur):
	start=cur
	Body=[]
	while cur<len(src) and src[cur][3]!='\n':
		if src[cur][3]=='\\':
			Body.append(src[cur])
			cur+=1
			if cur==len(src):
				raise pperror("'\\' at end of file", src[cur-1])
		Body.append(src[cur])
		cur+=1
	assert cur<=len(src)
	assert src[cur][3]=='\n'
	return Body, cur

def parse_params(raw):
	if re.match('\s*'+ID+'\s*' + '(\s*,\s*'+ID+'\s*)*', raw) is None:
		return None
	
	params=[]
	for p in re.findall(ID, raw):
		if p in params:
			return None
		else:
			params.append(p)
	return params

def get_definition(src, cur):
	assert startswith('#define', src, cur)
	
	cur+=7

	tail=to_string(src, cur)
	m=re.match(r'\s+'+ID+r'\s*'+PARAMS, tail)
	if m is not None:
		name=m.groups()[0]
		Name=[]
		for i in range(len(name)):
			Name.append(src[cur+m.start(1)+i])
			assert Name[-1][3]==name[i]
		params=parse_params(m.groups()[1])
		Body, end_cur=get_def_body(src, cur+m.start(2)+len(m.groups()[1])+1)
		if params is not None:
			return {
				'name': name,
				'params': params,
				'Body': Body,
				'Name': Name
			}, end_cur

	m=re.match(r'\s+'+ID, tail)
	if m is not None:
		name=m.groups()[0]
		Name=[]
		for i in range(len(name)):
			Name.append(src[cur+m.start(1)+i])
			assert Name[-1][3]==name[i]
		Body, cur=get_def_body(src, cur+m.start(1)+len(name))
		if Body and Body[0][3] in ' \t':
			Body=Body[1:]
		return {
			'name': name,
			'params': None,
			'Body': Body,
			'Name': Name
		}, cur
	
	raise pperror('definitition syntax error', src[cur-7])

def get_block(src, cur):
	assert startswith('${<', src, cur) or startswith('${{', src, cur)

	start=cur

	if startswith('${<', src, cur):
		custom_block=True
		tail=to_string(src, cur)

		m=re.match(r'\${<([^}]+)}', tail)

		if m is None:
			raise pperror('block start syntax error', cur)
	
		term=m.groups()[0]

		cur+=4+len(term)

	elif startswith('${{', src, cur):
		custom_block=False
		term='$}}'
		cur+=2
	else:
		assert False, 'should not reach here'


	Block=[]

	while cur<len(src) and not startswith(term, src, cur):
		Block.append(src[cur])
		cur+=1
	
	assert cur<=len(src)
	if cur==len(src):
		raise pperror('block reaches end of file', src[start])
	
	cur+=len(term)

	if not custom_block:
		Block.append(src[cur-1])

	return Block, cur

def get_target(obj):
	global SRC

	path=None
	if os.path.isabs(obj):
		path=obj
		try:
			src=open(path).read()
		except IOError as x:
			return None
	else:
		for p in PATH:
			# Read input
			try:
				src=open(os.path.join(p, obj)).read()
				path=os.path.join(p, obj)
				break
			except IOError as x:
				continue
		if path is None:
			return None
	
	src_id=get_src_id(path, path2id, id2path)
	SRC[src_id]=src
	text0=[]
	line=0
	pos=0
	for c in src:
		text0.append((src_id, line, pos, c))
		if c=='\n':
			line+=1
			pos=0
		else:
			pos+=1
	return text0
	

def include(src, cur):
	assert startswith('#include', src, cur)

	cur+=8

	while cur<len(src) and src[cur][3] in ' \t\n':
		cur+=1
	
	if cur==len(src):
		raise pperror('unexpected end of file while parsing #include',
			src[cur-1])
	
	if src[cur][3]!='"':
		raise pperror("quote ('\"') expected", src[cur])
	
	target_pos=src[cur+1]
	string, cur=get_string(src, cur)
	target=get_target(to_string(string[1:-1]))
	if target is None:
		raise pperror('include target not found', target_pos)

	return target, cur

def get_name(src, cur, start_with_ws=True):
	pattern=ID
	if start_with_ws:
		pattern=r'\s+'+pattern
	
	tail=to_string(src[cur:])
	m=re.match(pattern, tail)
	
	if m is None:
		raise pperror('syntax error', src[cur])
	
	name=m.groups(0)

	Name=[]
	for i in range(len(name)):
		Name.append(src[cur+m.start(1)+i])
	
	return Name, cur+m.start(1)+len(name)

def match_comment(Text, cur):
	return startswith('//', Text, cur) \
		or startswith('/*', Text, cur) \
		or startswith('`', Text, cur)
	
def skip_comment(Text, cur):
	if startswith('//', Text, cur):
		comment, cur=get_slcomment(Text, cur)
	elif startswith('/*', Text, cur):
		comment, cur=get_mlcomment(Text, cur)
	elif startswith('`', Text, cur):
		comment, cur=get_owcomment(Text, cur)
	else:
		raise Exception("Mustn't reach here")
	return cur
	
def match_string(Text, cur):
	return Text[cur][3]=='"'

def match_define(Text, cur):
	return startswith("#define", Text, cur)

def match_include(Text, cur):
	return startswith('#include', Text, cur)

def match_ifdef(Text, cur):
	return startswith('#ifdef', Text, cur)

def match_ifndef(Text, cur):
	return startswith('#ifndef', Text, cur)

def match_else(Text, cur):
	return startswith('#else', Text, cur)

def match_endif(Text, cur):
	return startswith('#endif', Text, cur)

def match_undef(Text, cur):
	return startswith('#undef', Text, cur)

def match_directive(Text, cur):
	if Text[cur][3]!='#' or cur+1==len(Text):
		return False
	return re.match('[_a-zA-Z]', Text[cur+1][3]) is not None

def match_fblock(Text, cur):
	return startswith('${<', Text, cur) or startswith('${{', Text, cur)

def match_macro(Text, cur):
	if Text[cur][3]!='$' or cur+1==len(Text):
		return False
	return re.match('[_a-zA-Z]', Text[cur+1][3]) is not None

def add_define(defs, dfn):
	if dfn['name'] in defs:
		C=defs[dfn['name']]['Name'][0]
		raise pperror('duplicate definition: %s' % dfn['name'],
			dfn['Name'][0],
			markerror('previous definition:', C))
	defs[dfn['name']]=dfn


def get_macro(Text, defs, cur):
	tail=to_string(Text, cur)
	m=re.match(r'\$'+ID, tail)
	if m is None:
		raise pperror("invalid $-expr", Text[cur])
	name=m.groups()[0]

	if name not in defs:
		raise pperror("undefined identifier: %s" % name,
			Text[cur+1])
	else:
		if defs[name]['params'] is None:
			return name, None, cur+m.start(1)+len(name)
		else:
			args, cur=get_args(Text, cur+1+len(name))
			if len(args)<len(defs[name]['params']):
				raise pperror(('more arguments expected ' \
					+'(provided: %d, need: %d)') % (
					len(args), len(defs[name]['params'])),
					Text[cur-1])
			elif len(args)>len(defs[name]['params']):
				larg=args[len(defs[name]['params'])-1]
				raise pperror(('unexpected argument ' \
					+ '(%d expected, got %d)') \
					% (len(defs[name]['params']), len(args)),
					larg['end'])
			return name, args, cur

def get_XXdef_name(Text, cur):
	base_cur=cur
	if startswith('#ifdef', Text, cur) \
			or startswith('#undef', Text, cur):
		cur+=6
	elif startswith('#ifndef', Text, cur):
		cur+=7
	else:
		raise Exception("Mustn't reach here")
	
	tail=to_string(Text, cur)
	m=re.match(r'\s+' + ID + r'([ \t]*)\n', tail)

	if m is None:
		raise pperror('directive syntax error "%s"%s"' % (
			r'\s+' + ID + r'([ \t]*)$', repr(tail)), Text[cur])
	
	name=m.groups()[0]
	name_pos=m.start(1)+cur
	cur+=m.start(2)+len(m.groups()[1])
	return name, cur, name_pos


def macro(dfn, args):
	argd={}
	if args is not None:
		for i in range(len(dfn['params'])):
			key=dfn['params'][i]
			val=args[i]
			assert key not in argd
			argd[key]=val
	body=list(dfn['Body'])
	cur=0
	while cur<len(body):
		subst_flag=False
		for par in dfn['params'] if dfn['params'] is not None else []:
			if startswith('$%s' % par, body, cur):
				body=body[:cur]+argd[par]+body[cur+1+len(par):]
				subst_flag=True
		if not subst_flag:
			cur+=1

	return body

def insert(l, pos, mid):
	assert 0<=pos<=len(l)

	for i in range(len(mid)):
		l.insert(pos+i, mid[i])

_parse_stack=[]
def parse2(Text, cur, defs, blocks, skip=False, from_where=None):
	assert type(cur)==int
	assert type(Text)==list
	_parse_stack.append(from_where)

	if len(_parse_stack)>64:
		assert _parse_stack[0] is None
		for s in _parse_stack[1:]:
			assert s is not None
			gsrc=SRC[s[0]]
			sys.stderr.write('at %s:%d:%d:\n%s' % ( id2path[s[0]],
				s[1], s[2],
				common.mark(gsrc.split('\n')[s[1]], s[2])))
		sys.stderr.write('%s: preprocessing error: ' % _prog \
			+ 'maximum parse recursion depth reached\n')
		sys.exit(1)

	Result=[]

	while cur<len(Text):
		if match_comment(Text, cur):
			cur=skip_comment(Text, cur)
		elif match_string(Text, cur):
			body, cur=get_string(Text, cur)
			if not skip:
				Result+=body
		elif match_define(Text, cur):
			dfn, cur=get_definition(Text, cur)
			if not skip:
				add_define(defs, dfn)
		elif match_include(Text, cur):
			if not skip:
				body, cur=include(Text, cur)
				body, cur1=parse2(body, 0, defs, blocks, False, Text[cur])
				insert(Text, cur, body)
		elif match_ifdef(Text, cur) or match_ifndef(Text, cur):
			base_cur=cur
			is_ifndef=match_ifndef(Text, cur)
			name, cur, name_pos=get_XXdef_name(Text, cur)
			if skip:
				body, cur=parse2(Text, cur, defs, blocks, True,
					Text[cur])
				if match_else(Text, cur):
					cur+=len('#else')
					body, cur=parse2(Text, cur, defs, blocks, True,
						Text[cur])
				if not match_endif(Text, cur):
					if cur==len(Text):
						raise pperror("unmatched %s" % (
							"#ifndef" if is_ifndef else "#ifdef"),
							Text[base_cur])
					raise pperror("#endif expected", Text[cur])
				cur+=len('#endif')
			else:
				cond=name in defs
				if is_ifndef:
					cond=not cond
				body, cur=parse2(Text, cur, defs, blocks, cond==False,
					Text[cur])
				if cond:
					Result+=body
				if match_else(Text, cur):
					cur+=len('#else')
					body, cur=parse2(Text, cur, defs, blocks, cond==True,
						Text[cur])
					if not cond:
						Result+=body

				if not match_endif(Text, cur):
					if cur==len(Text):
						raise pperror("unmatched %s" % (
							"#ifndef" if is_ifndef else "#ifdef"),
							Text[base_cur])
					raise pperror("#endif expected", Text[cur])
				cur+=len('#endif')
		elif match_else(Text, cur):
			if len(_parse_stack)==1:
				raise pperror("unmatched #else", Text[cur])
			else:
				_parse_stack.pop()
				# do not skip else, it will be skipped outside
				return Result, cur
		elif match_endif(Text, cur):
			if len(_parse_stack)==1:
				raise pperror("unmatched #endif", Text[cur])
			else:
				_parse_stack.pop()
				return Result, cur
		elif match_undef(Text, cur):
			base_cur=cur
			name, cur, name_pos=get_XXdef_name(Text, cur)
			if not skip:
				if name not in defs:
					raise pperror('undefined identifier: %s' % name,
						Text[name_pos])
				del defs[name]
		elif match_directive(Text, cur):
			raise pperror("illegal directive", src[cur+1])
		elif match_fblock(Text, cur):
			Block, cur=get_block(Text, cur)
			if not skip:
				Result+=to_String(SRC, path2id, id2path,
						' __block(%d) ' % len(blocks))
				blocks.append(Block)
		elif match_macro(Text, cur):
			base_cur=cur
			name, args, cur=get_macro(Text, defs, cur)

			if not skip:
				if name not in defs:
					raise pperror("undefined macro: %s" % name,
						Text[base_cur])
				if args is None:
					body=macro(defs[name], None)
					insert(Text, cur, body)
				else:
					args2=[]
					for arg in args:
						body, cur1=parse2(list(arg['body']), 0, defs,
							blocks, False, Text[cur-1])
						args2.append(body)
					body=macro(defs[name], args2)
					insert(Text, cur, body)


		else:
			if not skip:
				Result.append(Text[cur])
			cur+=1

	_parse_stack.pop()
	return Result, cur
			
				

'''
def parse(blocks_, defs_, src, from_where=None, mode='normal'):
	blocks=list()
	defs=dict()
	global gsrc, _parse_stack
	_parse_stack.append(from_where)
	if len(_parse_stack)>64:
		assert _parse_stack[0] is None
		for s in _parse_stack[1:]:
			assert s is not None
			gsrc=SRC[s[0]]
			sys.stderr.write(common.mark(gsrc.split('\n')[s[1]],
				s[2]))
		sys.stderr.write('%s: preprocessing error: ' % _prog \
			+ 'maximum #include depth reached\n')
		sys.exit(1)
		
	cur=0

	text=[]

	if mode=='normal' and startswith('#!', src, cur):
		interpreter, cur=get_interpreter(src, cur)

	while cur<len(src):
		if src[cur][3]=='"':
			string, cur=get_string(src, cur)
			for C in string:
				text.append(C)
		elif startswith('//', src, cur):
			comment, cur=get_slcomment(src, cur)
		elif startswith('/*', src, cur):
			comment, cur=get_mlcomment(src, cur)
		elif startswith('`', src, cur):
			comment, cur=get_owcomment(src, cur)
		elif src[cur][3]=='#':
			if startswith('#define', src, cur):
				dfn, cur=get_definition(src, cur)
				if dfn['name'] in defs:
					C=defs[dfn['name']]['Name'][0]
					raise pperror('duplicate definition: %s' % dfn['name'],
						dfn['Name'][0],
						markerror('previous definition:', C))
				defs[dfn['name']]=dfn
			elif startswith('#include', src, cur):
				base_cur=cur
				body, cur=include(src, cur)
				body=parse(blocks, defs, body, src[base_cur])
				for C in body:
					text.append(C)
			elif startswith('#ifdef', src, cur):
				base_cur=cur
				cur+=6
				Id, cur=get_name(src, cur)
				id=to_string(Id)
				try:
					if id in defs:
						parse(blocks, defs, src[cur:], src[base_cur],
							'cond_then_true')
						raise pperror('unfinished conditional block',
							src[base_cur])
					else:
						#parse(blocks, defs, src[cur:], src[base_cur],
						parse([], {}, src[cur:], src[base_cur],
							'cond_then_false')
						raise pperror('unfinished conditional block',
							src[base_cur])
				except CondBlockEnd as x:
					for C in x.Body:
						text.append(C)
					cur+=x.cur
			elif startswith('#ifndef', src, cur):
				base_cur=cur
				cur+=7
				Id, cur=get_name(src, cur)
				id=to_string(Id)
				try:
					if id in defs:
						#parse(blocks, defs, src[cur:], src[base_cur],
						parse([], {}, src[cur:], src[base_cur],
							'cond_then_false')
						raise pperror('unfinished conditional block',
							src[base_cur])
					else:
						parse(blocks, defs, src[cur:], src[base_cur],
							'cond_then_true')
						raise pperror('unfinished conditional block',
							src[base_cur])
				except CondBlockEnd as x:
					for C in x.Body:
						text.append(C)
					cur+=x.cur
			elif startswith('#endif', src, cur):
				if mode=='normal':
					raise pperror('#endif outside conditional block',
						src[cur])
				elif mode in ['cond_then_true', 'cond_else_true']:
					# ifdef-endif included
					blocks_+=blocks
					for k, v in defs.iteritems():
						if k in defs_:
							raise pperror('duplicate definition: %s' \
								% k, v['Name'][0],
								markerror('previous definition:',
								defs_[k]['Name'][0]))
						else:
							defs_[k]=v
					raise CondBlockEnd(text, cur+6)
				elif mode in ['cond_then_false', 'cond_else_false']:
					# ifdef-endif ignored
					raise CondBlockEnd([], cur+6)
				else:
					raise NotImplementedError(mode)
			elif startswith('#else', src, cur):
				base_cur=cur
				if mode=='normal':
					raise pperror('#else outside conditional block',
						src[cur])
				elif mode=='cond_then_true':
					cur+=5
					# ifdef-else include 'then'
					blocks_+=blocks
					for k, v in defs.iteritems():
						if k in defs_:
							raise pperror('duplicate definition: %s' \
								% k, v['Name'][0],
								markerror('previous definition:',
								defs_[k]['Name'][0]))
						else:
							defs_[k]=v
					try:
						parse(blocks, defs, src[cur:], src[base_cur],
							'cond_else_false')
					except CondBlockEnd as x:
						assert len(x.Body)==0
						cur+=x.cur
						raise CondBlockEnd(text, cur)
				elif mode=='cond_then_false':
					cur+=5
					# ifdef-else include 'else'
					try:
						parse(blocks, defs, src[cur:], src[base_cur],
							'cond_else_true')
					except CondBlockEnd as x:
						cur+=x.cur
						raise CondBlockEnd(x.Body, cur)
				elif mode in ['cond_else_true', 'cond_else_false']:
					raise pperror('duplicate #else in conditional block',
						src[base_cur])
				else:
					raise NotImplementedError(mode)

			elif startswith('#undef', src, cur):
				base_cur=cur
				cur+=6
				Id, cur=get_name(src, cur)
				id=to_string(Id)
				if id not in defs:
					raise pperror('undefined identifier: %s', Id[0])
				del defs[id]
			else:
				raise pperror('illegal directive', src[cur+1])
		elif startswith('${<', src, cur):
			Block, cur=get_block(src, cur)
			for C in to_String(SRC, path2id, id2path,
					' __block(%d) ' % len(blocks)):
				text.append(C)
			blocks.append(Block)
		else:
			text.append(src[cur])
			cur+=1

	_parse_stack.pop()
	blocks_+=blocks
	for k, v in defs.iteritems():
		if k in defs_:
			print >>sys.stderr, defs, defs_, _parse_stack
			raise pperror('duplicate definition: %s' \
				% k, v['Name'][0],
				markerror('previous definition:',
				defs_[k]['Name'][0]))
		else:
			defs_[k]=v
	return text
'''
def get_args(src, cur):
	if not startswith('(', src, cur):
		raise pperror('argument(s) expected', src[cur])
	start=cur
	cur+=1
	Args=[]
	Arg={'body': []}
	ended=False
	while cur<len(src):
		if startswith('\\', src, cur):
			# arg.append(src[cur]) -- do not include backslash
			cur+=1
			if cur==len(src):
				raise pperror("'\\' at end of file", src[cur-1])
		elif src[cur][3] in ',)':
			Arg['end']=src[cur]
			Args.append(Arg)
			cur+=1
			Arg={'body':[]}
			if src[cur-1][3]==')':
				ended=True
				break
		Arg['body'].append(src[cur])
		cur+=1
	if not ended:
		assert cur==len(src)
		raise pperror("Macro arguments reached end of file", src[start])
	return Args, cur

def subst_args(body, args):
	prev_text=None
	text=list(body)
	while text!=prev_text:
		prev_text=list(text)
		tail=to_string(text)
		for k, v in args.items():
			idx=tail.find('$%s' % k)
			if idx!=-1:
				for i in range(len(k)+1):
					del text[idx]
				for i in range(len(v)):
					text.insert(idx+i, v[i])
				break
	return text

def apply_macros(src, defs):
	prev_text=None
	text=src

	while text!=prev_text:
		prev_text=list(text)
		text=apply_macros_once(text, defs)
	
	return text

def apply_macros_once(src, defs):
	cur=0
	text=[]
	while cur<len(src):
		if startswith('"', src, cur):
			start=cur
			string, cur=get_string(src, cur)
			assert len(string)>1
			assert startswith('"', string) and string[-1][3]=='"'
			for C in string:
				text.append(C)
		elif startswith('$', src, cur):
			tail=to_string(src, cur)
			m=re.match(r'\$'+ID, tail)
			if m is None:
				raise pperror("invalid $-expr", src[cur])
			name=m.groups()[0]

			if name not in defs:
				raise pperror("undefined identifier: %s" % name,
					src[cur+1])
			else:
				if defs[name]['params'] is None:
					for C in defs[name]['Body']:
						text.append(C)
					cur+=1+len(name)
				else:
					args, cur=get_args(src, cur+1+len(name))
					if len(args)<len(defs[name]['params']):
						raise pperror(('more arguments expected ' \
							+'(provided: %d, need: %d)') % (
							len(args), len(defs[name]['params'])),
							src[cur-1])
					elif len(args)>len(defs[name]['params']):
						larg=args[len(defs[name]['params'])-1]
						raise pperror(('unexpected argument ' \
							+ '(%d expected, got %d)') \
							% (len(defs[name]['params']), len(args)),
							larg['end'])
					argd={}
					for i in range(len(defs[name]['params'])):
						key=defs[name]['params'][i]
						val=args[i]['body']
						assert key not in argd
						argd[key]=val
					body=subst_args(defs[name]['Body'], argd)
					for C in body:
						text.append(C)
		else:
			text.append(src[cur])
			cur+=1
	return text

path2id={}
id2path={}

def split_colons(s):
	tokens=[]
	cur=0
	token=''
	while s:
		if s[0]=='\\':
			s=s[1:]
			if not s:
				raise FatalError("'\\' at end of line")
			token+=s[0]
			s=s[1:]
		elif s[0]==':':
			if token:
				tokens.append(token)
			token=''
			s=s[1:]
		else:
			token+=s[0]
			s=s[1:]
	if token:
		tokens.append(token)
	return tokens

def main():
	global path, SRC
	SRC={}
	if '--help' in sys.argv[1:]:
		print(HELP_MESSAGE)
		sys.exit(0)
	
	# Handle arguments
	try:
		args, argf, argd=common.parse_args(sys.argv[1:], ['-o', '-D', '-b'], ['-D'])
	except ValueError as x:
		raise FatalError(
			'no value provided for key: %s' % x.args[1])
	except KeyError as x:
		raise FatalError('argument error: %s' % ' '.join(x.args))
	
	for key in argf:
		if key not in ['--help']:
			raise FatalError(
				'flag argument not recognized: %s' % key)
	
	for k, v in argd.items():
		if k not in ['-o', '--out-path', '--text-info', '--path', '-D',
				'-b', '--blocks-path']:
			raise FatalError(
				'argument not recognized: %s' % k)

	if len(args)==0:
		raise FatalError('no program specified')
	elif len(args)>1:
		raise FatalError(
			'multiple programs specified: %s' % ', '.join(args))
	
	in_path=args[0]

	if '--out-path' in argd and '-o' in argd:
		raise FatalError(
			'cannot use -o and --out-path options together')
	out_path=argd.get('--out-path', argd.get('-o', 'a.out'))

	global PATH
	if '--path' not in argd:
		PATH=['.']
	else:
		PATH=split_colons(argd['--path'])

	text0=get_target(in_path)
	if text0 is None:
		raise FatalError('failed to read \'%s\'' \
			% in_path)

	blocks=[]
	defs={}

	for k, v in argd.items():
		if k!='-D':
			continue
		for w in v:
			if '=' in w:
				key, val=w.split('=', 1)
			else:
				key, val=w, ''

			if key in defs:
				raise FatalError('identifier redefinition: %s' % key)
			defs[key]={
				'name': key,
				'params': None,
				'Body': to_String(SRC, path2id, id2path, val),
				'Name': to_String(SRC, path2id, id2path, key)
			}

	#text1=parse(blocks, defs, text0)

	#text2=apply_macros(text1, defs)

	if startswith('#!/', text0, 0):
		interpreter, cur=get_interpreter(text0, 0)
		text0=text0[cur:]

	text_final, cur=parse2(text0, 0, defs, blocks)#text2

	out=''.join([x[3] for x in text_final])

	open(out_path, 'w').write(out)

	if '--text-info' in argd:
		open(argd['--text-info'], 'w').write(json.dumps({
			'paths': id2path,
			'text': text_final
		}, sort_keys=True, ensure_ascii=False))

	blocks_path=argd.get('--blocks-path', argd.get('-b'))

	if blocks_path is not None:
		open(blocks_path, 'w').write(json.dumps({
			'paths': id2path,
			'text': blocks
		}, sort_keys=True))
		

if __name__=='__main__':
	try:
		main()
	except FatalError as x:
		sys.stderr.write('%s: fatal error: %s\n' % (_prog, x.message))
		sys.exit(1)
	except PreprocessorError as x:
		gsrc=SRC[x.Path]
		if x.LineNum is not None:
			sys.stderr.write('%s: preprocessing error: %s at %s:%d:%d:\n' \
				% (_prog, x.Message, id2path[x.Path], x.LineNum+1,
				x.LinePos+1))
		else:
			sys.stderr.write('%s: preprocessing error: %s ' % (_prog,
				x.Message) + '(not in file):\n')

		if x.LineNum==len(gsrc.split('\n')):
			assert x.LinePos==0
		else:
			if x.LineNum is None:
				sys.stderr.write(common.mark(SRC[x.Path], x.LinePos))
			else:
				sys.stderr.write(common.mark(gsrc.split('\n')[x.LineNum],
					x.LinePos))

		for msg in x.Extras:
			sys.stderr.write('%s\n' % msg)
		sys.exit(1)
