# NOTE this is an edited copy of the original luna script

# TODO add ability to use some other temporary directory for building
# tests, not .luna subdirectory, and allow setup via environment var.


import sys, os, subprocess, json, re, datetime, signal, shutil
sys.path.append(os.path.join(os.environ['LUNA_HOME'], 'scripts'))
from common import *

_argv0=os.path.split(sys.argv[0])[1]
HELP_MESSAGE=sys.argv[0]+'''

NAME
	''' + _argv0 + ''' - execute LuNA program

SYNOPSIS
	''' + _argv0 + ''' [OPTION]... FILE [ARGUMENT]...

DESCRIPTION
	Executes LuNA program, described in FILE.

	FILE is a path of LuNA-program (*.fa) to execute. In the LuNA-
	program subroutine called 'main' is executed. If the subroutine
	has arguments then corresponding number of arguments must be
	passed.

OPTIONS
	--help
	    Show this help and exit. Other options are ignored.
	
	--build-dir=<path>
	    Specify path for intermediate files' dir. Warning: may
	    be deleted with all the contents. Default is './.luna'.
	
	--luna-home=<path>
	    Specify luna system installation dir (overrides LUNA_HOME env var).
	
	--disable-warning=<NUM>
	    Do not show warnings with given number.

	-u
	    Display information flags about cpu usage by all thread pools
	
	-q
	--quiet
	    Do not show progress info.

	-g
	    Include debug information (may reduce performance)
	
	-b
	    Enable dynamic balance.
		

	-O0
	-O
	    Specify optimization level. -O0 optimizes compilation time at
	    the cost of performance reduction. -O optimizes performance
	    (may increase compilation time).
	
	--no-cleanup
	    Do not delete intermediate files after program termination.

	--verbose
	    Display most intermediate info
	
	-t
	    Show program execution time (compilation time is not included).

	--compile-only
	    Do not run program after compilation. Also sets no-cleanup 
	    flag. Output is a command for program execution.

	--cpp-dir=<path>
		Specify source dirictories for *.cpp files. Can recieve 
		multiple paths. Default is FILE

	--cpp-source=<path>
		Specify additional *.cpp files.

	-c
	    Only compile the program, deleting all intermediate files.
	    The result is stored as ./libucodes.so by default (unlike
	    the --compile-only flag, which stores the result into
	    the build directory.
		
	--use-mpi
		Use MPI compiler instead of gcc.

	--allow-growing-thread-pool
		Allow thread pool to grow when using MPI-functions.

	--max-additional-threads=<count>
		Caps number of threads created when using MPI-functions.

	-D key
	-D key=val
	    Add preprocessor variable (#define...)

	--worker-threads-count=<count>
	    Set number of worker threads.
	
ENVIRONMENT
	Environment variables affect LuNA settings, but the OPTIONs take
	precedence over them (if appropirate).

	LUNA_HOME
	    Path to LuNA home directory. If unset, LUNA_HOME is set to
		'<script_dir>/..'.
	
	PYTHON
	    Python interpreter to use.

	CXX_FLAGS
	    Additional flags used to compile program sources (*.cpp).

	CXX
	    C++ compiler to use to compile program sources (*.cpp).

	LDFLAGS
	    C++ linker flags to link program sources.
	
	LUNA_NO_CLEANUP
	    If this variable is present, then temporary build directory
	    will not be removed in the end (same as --no-cleanup option).

REPORTING BUGS
	Report bugs to perepelkin@ssd.sscc.ru.
'''

class CompileError(Exception):
	def __init__(self, message, ti, pos):
		self.message=message
		self.path=ti['paths'][str(ti["text"][pos][0])]
		self.line_num=ti["text"][pos][1]
		self.line_pos=ti["text"][pos][2]
		self.line=open(self.path).read().split('\n')[self.line_num]

DISABLED_WARNINGS=set()

def warn(num, msg):
	if num in DISABLED_WARNINGS:
		return
	sys.stderr.write('%s: WARNING#%d: %s\n' % (_argv0, num, msg))

def parse_args(args):
	'''
	Parse and verify command-line arguments (see --help for info)
	'''
	global QUIET_FLAG, VERBOSE_FLAG, COMPILE_ONLY_FLAG
	conf={}
	cur=0

	# Default config parameters
	QUIET_FLAG=False
	conf['DEBUG']=False
	conf['CLEANUP']=True
	conf['TIME']=False
	conf['BALANCE']=False
	conf['PP_KEYS']=[]
	conf['RTS_FLAGS']=[]
	VERBOSE_FLAG=False
	COMPILE_ONLY_FLAG=False


	while cur<len(args):
		arg=args[cur]

		if arg.startswith('--build-dir='):
			conf['BUILD_DIR']=arg.split('=', 1)[1]
		elif arg.startswith('--luna-home='):
			conf['LUNA_HOME']=arg.split('=', 1)[1]
		elif arg.startswith('--worker-threads-count='):
			conf['RTS_FLAGS'].append(arg)
		elif arg.startswith('max-additional-threads='):
			conf['RTS_FLAGS'].append(arg)
		elif arg=='--allow-growing-thread-pool':
			conf['RTS_FLAGS'].append(arg)
		elif arg.startswith('--disable-warning='):
			try:
				num=int(arg.split('=', 1)[1])
				DISABLED_WARNINGS.add(num)
			except ValueError:
				raise FatalError("'--disable-warning' requires "\
					+ "numerical argument, got: %s" % arg.split('=', 1)[1])
		elif arg.startswith('--cpp-dir='):
			conf['CPP_DIR']=arg.split('=', 1)[1]
		elif arg.startswith('--cpp-source='):
			conf['CPP_SOURCES']=arg.split('=', 1)[1]
		elif arg in ['-q', '--quiet']:
			QUIET_FLAG=True
		elif arg=='--verbose':
			VERBOSE_FLAG=True
		elif arg=='--compile-only':
			COMPILE_ONLY_FLAG=True
			conf['CLEANUP']=False
		elif arg in ['-O', '-O0']:
			if 'OPTIMIZATION' in conf and conf['OPTIMIZATION']!=arg:
				raise FatalError("conflicting optimization options: " \
					+ "'%s' and '%s'" % (conf['OPTIMIZATION'], arg))
			conf['OPTIMIZATION']=arg
		elif arg=='-g':
			conf['DEBUG']=True
		elif arg=='--no-cleanup':
			conf['CLEANUP']=False
		elif arg=='-t':
			conf['TIME']=True
		elif arg=='-b':
			conf['BALANCE']=True
		elif arg=='-D':
			conf['PP_KEYS'].append('-D')
			cur+=1
			if cur>=len(args):
				raise FatalError("no value provided for -D key");
			arg=args[cur]
			conf['PP_KEYS'].append(arg)
		elif arg=='-c':
			COMPILE_ONLY_FLAG=True
			conf['SO_OUTPUT_FILE_NAME']='libucodes.so'
		elif arg.startswith('--comm-config='):
			conf['COMM_CONFIG'] = arg.split('=')[1]
		elif arg=='--use-mpi':
			conf['CXX']='mpicxx'
		elif arg.startswith('--output-file='):
			if 'SO_OUTPUT_FILE_NAME' in conf:
				conf['SO_OUTPUT_FILE_NAME']=arg.split('=', 1)[1]
			else: raise FatalError('--output-file supported only with -c before')
		else:
			if arg[cur].startswith('-'):
				warn(1, "suspicious program name: '%s' (mistyped a key?)" \
					% arg)
			conf['PROGRAM']=args[cur]
			if 'OPTIMIZATION' not in conf:
				conf['OPTIMIZATION']='-O'
			conf['ARGV']=args[cur+1:]
			return conf

		cur+=1

	raise FatalError('no program specified')

def parse_env(conf):
	'''
	Check environment variables for unset config parameters
	'''
	if 'LUNA_HOME' not in conf:
		conf['LUNA_HOME']=os.environ.get('LUNA_HOME',
			os.path.join(os.path.dirname(os.path.realpath(__file__)),
			os.pardir))

	if 'PYTHON' not in conf:
		conf['PYTHON']=os.environ.get('PYTHON', 'python3')

	if 'CXX_FLAGS' not in conf and 'CXX_FLAGS' in os.environ:
		conf['CXX_FLAGS']=os.environ['CXX_FLAGS']

	if 'CXX' not in conf and 'CXX' in os.environ:
		conf['CXX']=os.environ['CXX']

	if 'LDFLAGS' not in conf and 'LDFLAGS' in os.environ:
		conf['LDFLAGS']=os.environ['LDFLAGS']

	if 'LUNA_NO_CLEANUP' in os.environ:
		conf['CLEANUP']=False

	if 'CXX' not in conf:
		conf['CXX']='g++'

C_TYPES={
	'int': 'int',
	'real': 'double',
	'string': 'const char *',
	'value': 'const InputDF &',
	'name': 'OutputDF &'
}

def get_path_id(path, id2path, path2id, SRC):
	if path in path2id:
		return path2id[path]
	else:
		id=len(path2id)
		path2id[path]=id
		id2path[id]=path
		return id

def generate_cpp_blocks(ja_in_path, headers_in_path, ja_ti_path, b_in_path,
		ja_out_path, b_out_path, b_ti_path):
	ja=json.loads(open(ja_in_path).read())
	try:
		hja=json.loads(open(headers_in_path).read())
	except IOError:
		hja=[]
	blocks=json.loads(open(b_in_path).read())

	path2id={}
	id2path={}
	SRC={}
	Cpp=[]

	for C in to_String(SRC, path2id, id2path, '#include "ucenv.h"\n\n'):
		Cpp.append(C)

	for header in hja:
		if header[0]=='head':
			block_id=header[1]
			for C in blocks['text'][block_id]:
				Cpp.append(C)
		else:
			raise NotImplementedError(header[0], header)

	for name, sub in ja.items():
		if sub['type']=='foreign_cpp':
			block_id=sub['block_id']

			# Generate signature
			sig='extern "C" void __foreign_block_%d(' % block_id

			sig+=', '.join(['%s %s' % (C_TYPES[arg['type']], arg['id']) \
				for arg in sub['args']])

			sig+=')\n'

			for C in to_String(SRC, path2id, id2path, sig):
				Cpp.append(C)

			# Append block

			for C in blocks['text'][block_id]:
				path=blocks['paths'][str(C[0])]
				C[0]=get_path_id(path, id2path, path2id, SRC)

				Cpp.append(C)

			sub['type']='extern'
			sub["mpi"]='false'
			sub['foreign_type']='C++'
			sub['code']='__foreign_block_%d' % block_id
			del sub['block_id']

	open(b_out_path, 'w').write(to_string(Cpp))
	open(b_ti_path, 'w').write(json.dumps({
		"paths": id2path,
		"text": Cpp
	}))
	open(ja_out_path, 'w').write(json.dumps(ja))


def info(msg):
	if QUIET_FLAG or COMPILE_ONLY_FLAG:
		return
	if VERBOSE_FLAG:
		if msg is None:
			return
		else:
			print(msg)
	else:
		if msg is None:
			sys.stdout.write('\033[2K\r')
		else:
			assert '\n' not in msg
			sys.stdout.write('\033[2K\rluna: %s' % msg)
		sys.stdout.flush()

def error_block_message(block_id, blocks_info, pp_ti):
	bi=blocks_info[str(block_id)]
	comment=bi.get('comment', '')
	C=pp_ti['text'][bi['pos']]
	path=pp_ti['paths'][str(C[0])]
	#TODO handle IOError
	line=open(path).read().split('\n')[C[1]]
	pos=C[2]
	return 'error in block %d in %s:%d:%d:\n%s\n%s' % (block_id,
		path, C[1]+1, C[2]+1, comment,
		mark(line, pos))

def so_build_message(eln, epos, emsg):
	#emsg=emsg.decode('utf-8')
	ln=1
	cur=1
	for i in range(len(conf['fb.ti']['text'])):
		assert ln<=eln and (cur<=epos or ln<eln)

		if ln==eln and cur==epos:
			C=conf['fb.ti']['text'][i]
			mrk=mark(conf['fb.cpp'][ln-1], cur-1)
			return 'compile: at %s:%d:%d: %s\n%s' % (
				conf['fb.ti']['paths'][str(C[0])], C[1], C[2],
				emsg, mrk)
		if conf['fb.ti']['text'][i][3]=='\n':
			ln+=1
			cur=1
		else:
			cur+=1
	raise Exception('reached eof while seeking for error pos')

def get_src(path):
	return open(path).read().split('\n')

def translate_so_build_err(err):
	err=err.split('\n')
	cur=0

	res=[]

	while cur<len(err):
		ln=err[cur]
		if ln.startswith(conf['BUILD_DIR']):
			m=re.match('%s:([0-9]+):([0-9]+): (.*)$' % os.path.join(
				conf['BUILD_DIR'], 'foreign_blocks.cpp'), ln)
			m2=re.match('%s:.*void __foreign_block_([0-9]+)\(.*' \
				% os.path.join(
				conf['BUILD_DIR'], 'foreign_blocks.cpp'), ln)
			if m is not None:
				res.append(so_build_message(int(m.groups()[0]),
					int(m.groups()[1]), m.groups()[2]))
			elif m2 is not None:
				bid=int(m2.groups()[0])
				C=conf['blocks.ti']['text'][bid][0]
				res.append('in foreign block at %s:%d:%d:\n%s' % (
					conf['blocks.ti']['paths'][str(C[0])],C[1], C[2],
					mark(get_src(conf['blocks.ti']['paths'][str(C[0])])[C[1]], C[2])))
			else:
				res.append(ln)#.decode('utf-8'))
		else:
			res.append(ln)#.decode('utf-8'))

		cur+=1

	return '\n'.join(res)


def translate_fcmp2_err(err):
	res=[]

	for ln in err.split('\n'):
		m=re.match('#ERROR at ([0-9]+): (.*)$', ln)
		if m is None:
			res.append('err> %s' % ln)
		else:
			pos=int(m.groups()[0])
			C=ti['text'][pos]
			path=ti['paths'][str(C[0])]
			ln=C[1]
			ln_pos=C[2]
			line=get_src(ti['paths'][str(C[0])])[ln]
			mmsg=mark(line, ln_pos)

			res.append('%s at %s:%d:%d:\n%s' % (
				m.groups()[1], path, ln+1, ln_pos+1, mmsg))
	return '\n'.join(res)

def main():
	global conf, ti
	if '--help' in sys.argv[1:]:
		print(HELP_MESSAGE)
		sys.exit(0)

	conf=parse_args(sys.argv[1:])
	parse_env(conf)

	# Set unset parameters to default values
	if 'BUILD_DIR' not in conf:
		path=os.path.abspath(conf['PROGRAM'])
		dirs=[]
		while os.path.split(path)[1]:
			path, tail=os.path.split(path)
			dirs=[tail]+dirs
		conf['BUILD_DIR']=os.path.join(conf['LUNA_HOME'], 'build', 'programs', *dirs)

	if 'PYTHON' not in conf:
		conf['PYTHON']='python3'

	conf['CXX_FLAGS']=conf.get('CXX_FLAGS', '') \
		+ ' -I %s' % os.path.join(conf['LUNA_HOME'], 'include') \
		+ ' -std=c++11 -fPIC -Wall -Wpedantic -Wno-vla' \
		+ ' -Wno-sign-compare -Wno-unused-but-set-variable' \
		+ ' -Wno-unused-variable'

	if conf['DEBUG']:
		conf['CXX_FLAGS']+=' -g -DADD_DEBUG_INFO'

	if conf['OPTIMIZATION']=='-O':
		conf['CXX_FLAGS']+=' -O3'
	elif conf['OPTIMIZATION']=='-O0':
		conf['CXX_FLAGS']+=' -O0'
	else:
		raise NotImplementedError(conf['OPTIMIZATION'])


	conf['LDFLAGS']=conf.get('LDFLAGS', '') \
		+ ' -ldl -L %s' % os.path.join(conf['LUNA_HOME'], 'lib')

	if conf['DEBUG']:
		conf['LDFLAGS']+=' -g -lrts_dbg'
	else:
		conf['LDFLAGS']+=' -lrts'
	if 'CPP_DIR' not in conf:
		conf['CPP_DIR']=os.path.dirname(os.path.abspath(
		conf['PROGRAM']))
	if 'CPP_SOURCES' not in conf:
		conf['CPP_SOURCES']=0

	# ensure build dir exists
	info('creating build dir')

	if not os.path.exists(conf['BUILD_DIR']):
		os.makedirs(conf['BUILD_DIR'])

	if not os.path.isdir(conf['BUILD_DIR']):
		raise FatalError('not a directory at build dir: %s' \
			% conf['BUILD_DIR'])
	try:

		# ==========
		# preprocess
		# ==========

		info('preprocessing')
		cmd=[conf['PYTHON']]
		cmd+=[os.path.join(conf['LUNA_HOME'], 'scripts', 'pp.py')]
		cmd+=conf['PP_KEYS']
		cmd+=[conf['PROGRAM']]
		cmd+=['-o', os.path.join(conf['BUILD_DIR'], 'preprocessed.fa')]
		cmd+=['--text-info=%s' % os.path.join(conf['BUILD_DIR'],
			'preprocessed.fa.ti')]
		cmd+=['--blocks-path=%s' % os.path.join(conf['BUILD_DIR'],
			'blocks.ti')]

		if VERBOSE_FLAG:
			print(' '.join(cmd))
		p=subprocess.Popen(cmd, stderr=subprocess.PIPE,
			stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		out, err=p.communicate()
		out=out.decode('utf-8')
		err=err.decode('utf-8')

		if VERBOSE_FLAG:
			print(out)

		if p.returncode!=0:
			raise FatalError('preprocessing failed (see error messages ' \
				+ 'below):\n%s' % err)
		ti=json.loads(open(os.path.join(conf['BUILD_DIR'],
			'preprocessed.fa.ti')).read())


		# =====
		# parse
		# =====

		info('parsing')
		cmd=[os.path.join(conf['LUNA_HOME'], 'bin', 'parser')]
		cmd+=[os.path.join(conf['BUILD_DIR'], 'preprocessed.fa')]
		cmd+=['-o', os.path.join(conf['BUILD_DIR'], 'program.ja')]
		cmd+=['-h', os.path.join(conf['BUILD_DIR'], 'headers.ja')]
		if VERBOSE_FLAG:
			print(' '.join(cmd))
		p=subprocess.Popen(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
		out, err=p.communicate()
		out=out.decode('utf-8')
		err=err.decode('utf-8')

		if p.returncode!=0:
			for ln in err.split('\n'):
				m=re.match('^#ERROR: ([0-9]+): (.*)$', ln)

				if m is not None:
					pos=int(m.groups()[0])
					raise CompileError(m.groups()[1], ti, pos)
			raise FatalError('compilation failed: ' \
				+ 'see error messages below:\n%s' % err)

		if VERBOSE_FLAG:
			print(out)

		# ================
		# let substitution
		# ================

		info('let substitution')
		cmd=[conf['PYTHON']]
		cmd+=[os.path.join(conf['LUNA_HOME'], 'scripts',
			'substitution_let.py')]
		cmd+=[os.path.join(conf['BUILD_DIR'], 'program.ja')]
		if VERBOSE_FLAG:
			print(' '.join(cmd))
		p=subprocess.Popen(cmd, stderr=subprocess.PIPE,
			stdout=subprocess.PIPE)
		out, err=p.communicate()
		out=out.decode('utf-8')
		err=err.decode('utf-8')

		if p.returncode!=0:
			raise FatalError('compilation failed: ' \
				+ 'see error messages below:\n%s' % err)

		if VERBOSE_FLAG:
			print(out)

		# ===================
		# generate cpp blocks
		# ===================

		info('generating cpp blocks')

		generate_cpp_blocks(
			os.path.join(conf['BUILD_DIR'], 'program.ja'),
			os.path.join(conf['BUILD_DIR'], 'headers.ja'),
			os.path.join(conf['BUILD_DIR'], 'preprocessed.fa.ti'),
			os.path.join(conf['BUILD_DIR'], 'blocks.ti'),
			os.path.join(conf['BUILD_DIR'], 'program_foreign.ja'),
			os.path.join(conf['BUILD_DIR'], 'foreign_blocks.cpp'),
			os.path.join(conf['BUILD_DIR'], 'foreign_blocks.cpp.ti')
		)

		conf['blocks.ti']=json.loads(open(
			os.path.join(conf['BUILD_DIR'], 'blocks.ti')
			).read())
		conf['fb.ti']=json.loads(open(
			os.path.join(conf['BUILD_DIR'], 'foreign_blocks.cpp.ti')
			).read())
		conf['fb.cpp']=open(
			os.path.join(conf['BUILD_DIR'], 'foreign_blocks.cpp')).read() \
				.split('\n')


		# ===============
		# generate recoms
		# ===============

		info('generating recommendations')

		cmd=[conf['PYTHON'],
			os.path.join(conf['LUNA_HOME'], 'scripts', 'fcmp'),
			os.path.join(conf['BUILD_DIR'], 'program_foreign.ja'),
			os.path.join(conf['BUILD_DIR'], 'program_recom.ja'),
			'--only-requests']
		if VERBOSE_FLAG:
			print(' '.join(cmd))
		p=subprocess.Popen(cmd, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
		out, err=p.communicate()
		out=out.decode('utf-8')
		err=err.decode('utf-8')

		if p.returncode!=0:
			for ln in err.split('\n'):
				m=re.match('^#ERROR at ([0-9]+): (.*)$', ln)
				if m is not None:
					pos=int(m.groups()[0])
					raise CompileError(m.groups()[1], ti, pos)
			raise FatalError('recom-generation failed (see below):\n%s' \
				% err)

		if VERBOSE_FLAG:
			print(out)

	finally:
		if conf['CLEANUP']:
			info('cleaning up')
			try:
				cmd=['rm', '-r', conf['BUILD_DIR']]
				if VERBOSE_FLAG:
					print(' '.join(cmd))
				subprocess.check_call(cmd)
			except subprocess.CalledProcessError as x:
				raise FatalError('cleanup failed (errcode=%d)' % x.returncode)
		info(None)


if __name__=='__main__':
	try:
		main()
	except FatalError as x:
		sys.stderr.write(('%s: fatal error: %s\n' % (_argv0, x.message)))
		sys.exit(1)
	except CompileError as x:
		sys.stderr.write('%s: compile error: %s at %s:%d\n' % (
			_argv0, x.message, x.path, x.line_num+1))
		sys.stderr.write(mark(x.line, x.line_pos))
		sys.exit(1)
