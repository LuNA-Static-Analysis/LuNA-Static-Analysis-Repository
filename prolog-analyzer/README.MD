# prolog-analyzer

A static analysis tool for [LuNA](https://gitlab.ssd.sscc.ru/luna/luna) 
programs.

## Usage 

Set `PROLOG_ANALYZER_HOME` to project path, add 
`$PROLOG-ANALYZER_HOME/bin` to `PATH`.

Make sure `$PROLOG-ANALYZER_HOME/bin/prolog-analyzer` has
execution permission.

Help:
```
$ prolog-analyzer --help
Usage: prolog-analyzer.py [OPTIONS] [LUNA_SRC]...

Options:
  --project-dir DIRECTORY  LuNA project directory.  [required]
  --build-dir DIRECTORY    LuNA project build directory.  [default: build]
  --output-dir DIRECTORY   Directory for program output.  [default: .prolog-
                           analyzer]
  --errors-file FILE       Append errors list to existing file or create a new
                           errors file.
  --no-cleanup             Do not delete generated files.
  --help                   Show this message and exit.
```

## Requirements
 * [LuNA](https://gitlab.ssd.sscc.ru/luna/luna)
 * [Python3.10+](https://www.python.org/downloads/)
   * [requirements.txt](requirements.txt)
 * [SWI-Prolog](https://www.swi-prolog.org/download/stable)
