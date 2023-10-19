Ветка DeGSA (Dependency Graph Static Analyzer). Разработчик: Царев Василий.

На всякий случай оставлю здесь модификации ast:

//MODIFICATIONS
#define uint unsigned int // VSCode on Windows could not see uint before for some reason
//typedef int ssize_t; // while using MSYS2 offered compilator, I found that this is a conflicting definition;
// everything works on my laptop with this commented, later will check if this works on the desktop

//also this must be added to the class ast: friend class DDG;

//END
