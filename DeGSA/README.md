In order for DeGSA to work, PATH envvar should have a path to:

1. python3
2. g++
3. flex
4. bison

so the launch script could run(run: bash launch.sh).

Project structure:

- luna subfolder: contains LuNA source codes to analyze; script automatically pickups "source.fa"; ucodes.cpp contains C++ modules in order for LuNA code to run, but it does basically nothing
- classes.hpp: forward class declaration needed for some modules
- compile.sh: script for compilation of analyzer without running it
- launch.sh: same as compile.sh, but also launches analyzer
- ddg.cpp: module with main class consisting of all graph construction and initialization algorithms
- enums.hpp: all enums of the project
- expr.hpp/cpp: classes that are used to represent all expressions inside the program
- ids.hpp/cpp: classes that are used to represent all identifiers inside the program; strongly connected with expr.cpp
- vertices.hpp/cpp: classes that are used to represent graph itself (not only vertices, but also edges)
- main.cpp: starting point of a program

Other files:
- output.txt: resulting information about vertices of a graph, declared identifiers, found errors and working time
- temporary leftover files from launches of an analyzer (such as grammar.tab.hpp, which is required for Intellisense to work properly)

External dependencies:
- parser folder: used for parsing source LuNA code and create an AST out of it for using it in creation of a graph
- g++ -- required for compilation
- bison, flex -- required for building AST, therefore are mandatory
- python -- required for preprocessiong of LuNA source code, so needed to analyze the code
- gdb -- required for debugging (at least in VSCode)
