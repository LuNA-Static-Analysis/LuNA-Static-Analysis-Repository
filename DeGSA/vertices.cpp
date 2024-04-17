#include "enums.hpp"
#include "vertices.hpp"

Vertex::Binding::Binding(Vertex* pointerTo, Identifier* id){
    this->id = id;
    this->pointerTo = pointerTo;
}

Identifier* Vertex::Binding::getId(){
    return this->id;
}

Vertex* Vertex::Binding::getPointerTo(){
    return this->pointerTo;
}

bool Vertex::Binding::operator<(const Binding b) const {//TODO
    if ((long)(this->pointerTo) < (long)b.pointerTo){
        return true;
    //} else if (this->id.compare(b.id) > 0){
    } else if (this->id > b.id){
        return true;
    } else {
        return false;
    }
}

Vertex::Vertex(){};

Vertex::~Vertex(){};

VertexType Vertex::getVertexType(){
    return vertexType;
}

std::string Vertex::getName(){
    return name;
}

std::set<Identifier*> Vertex::getUseSet(){
    return use;
}

std::set<Identifier*> Vertex::getDefSet(){
    return def;
}

std::set<Vertex*> Vertex::getInsideSet(){
    return inside;
}

std::set<Vertex::Binding> Vertex::getInSet(){
    return in;
}

std::set<Vertex::Binding> Vertex::getOutSet(){
    return out;
}

int Vertex::getDepth(){
    return depth;
}

int Vertex::getNumber(){
    return number;
}

int Vertex::getLine(){
    return line;
}

void Vertex::addIn(Vertex* vertex, Identifier* id){
    this->in.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

void Vertex::addOut(Vertex* vertex, Identifier* id){
    this->out.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

void Vertex::addInside(Vertex* vertex){
    auto temp = this->inside.find(vertex);
    if (temp == this->inside.end()){
        this->inside.insert(vertex);
    }
}

void Vertex::addUse(Identifier* id){
    auto temp = this->use.find(id);
    if (temp == this->use.end()){
        this->use.insert(id);
    }
}

void Vertex::addDef(Identifier* id){
    auto temp = this->def.find(id);
    if (temp == this->def.end()){
        this->def.insert(id);
    }
}

std::map<std::string, Identifier*> Vertex::getDeclaredInsideIdsMap(){
    return declaredInsideIdsMap;
}

std::map<std::string, Identifier*> Vertex::getDeclaredOutsideIdsMap(){
    return declaredOutsideIdsMap;
}

std::map<std::string, Identifier*> Vertex::getDeclaredBothIdsMap(){
    return declaredBothIdsMap;
}

Vertex* Vertex::getParent(){
    return this->parent;
}

std::string Vertex::getFileName(){
    return this->fileName;
}

void Vertex::setDeclaredInsideIdsMap(std::map<std::string, Identifier*> declaredInsideIdsMap){
    this->declaredInsideIdsMap = declaredInsideIdsMap;
}

void Vertex::setDeclaredOutsideIdsMap(std::map<std::string, Identifier*> declaredOutsideIdsMap){
    this->declaredOutsideIdsMap = declaredOutsideIdsMap;
}

void Vertex::setDeclaredBothIdsMap(std::map<std::string, Identifier*> declaredBothIdsMap){
    this->declaredBothIdsMap = declaredBothIdsMap;
}

void Vertex::printCallStack(std::ostream* outputTarget){
    *outputTarget << "Address: " << this;
    *outputTarget << "; type: " << this->getVertexType();
    *outputTarget << "; line: " << this->getLine() << std::endl;
    if (this->getParent() != nullptr){
        this->getParent()->printCallStack(outputTarget);
    }
}

CFVertex::CFVertex(int depth, int number, int line,
    std::string name, VertexType vertexType, Vertex* parent, std::vector<Identifier*> argNames,
    std::string fileName){

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;
    this->fileName = fileName;
    this->argNames = argNames;

    this->name = name;
    this->vertexType = vertexType; // import or sub

    this->in = {};
    this->out = {};
}

void CFVertex::printInfo(std::ostream* outputTarget) {

    *outputTarget << "Vertex number: " << this->getNumber() << std::endl;
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex type: ";
    *outputTarget << this->getVertexType() << " ";
    switch (this->getVertexType()){
        case importVF:
            *outputTarget << "(atomic CF); name: " << this->getName() << std::endl;
            break;
        case subVF:
            *outputTarget << "(structured CF);  name: " << this->getName() << std::endl;
            break;
        default:
            *outputTarget << "(unknown)" << std::endl;
            break;
    }
    *outputTarget << "Vertex line: " << this->getLine() << std::endl;
    *outputTarget << "Vertex depth: " << this->getDepth() << std::endl;

    *outputTarget << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (Vertex* v: this->getInsideSet()){
        *outputTarget << " " << v;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

}

ForVertex::ForVertex(int depth, int number, int line,
    ForIteratorName* iterator, Expression* leftBorder, Expression* rightBorder,
    Vertex* parent, std::string fileName){

    this->vertexType = forVF;
    this->name = "for";

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;
    this->fileName = fileName;

    this->iterator = iterator;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;

    this->in = {};
    this->out = {};
}

ForIteratorName* ForVertex::getIterator(){
    return iterator;
}

Expression* ForVertex::getLeftBorder(){
    return leftBorder;
}

Expression* ForVertex::getRightBorder(){
    return rightBorder;
}

void ForVertex::printInfo(std::ostream* outputTarget){

    *outputTarget << "Vertex number: " << this->getNumber() << std::endl;
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex type: ";
    *outputTarget << this->getVertexType() << " " << "(for)" << std::endl;
    *outputTarget << "Vertex line: " << this->getLine() << std::endl;
    *outputTarget << "Vertex depth: " << this->getDepth() << std::endl;

    *outputTarget << "Iterator: " + this->getIterator()->getName() << std::endl;
    *outputTarget << "Left border: " + this->getLeftBorder()->getExpr()->to_string() << std::endl;
    *outputTarget << "Right border: " + this->getRightBorder()->getExpr()->to_string() << std::endl;

    *outputTarget << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

}

WhileVertex::WhileVertex(int depth, int number, int line,
    WhileIteratorName* iterator, Identifier* outName, Expression* conditionExpr, Expression* startExpr,
    Vertex* parent, std::string fileName){

    this->vertexType = whileVF;
    this->name = "while";

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;
    this->fileName = fileName;

    this->iterator = iterator;
    this->outName = outName;
    this->conditionExpr = conditionExpr;
    this->startExpr = startExpr;

    this->in = {};
    this->out = {};
}

WhileIteratorName* WhileVertex::getIterator(){
    return this->iterator;
}

Identifier* WhileVertex::getOutName(){
    return this->outName;
}

Expression* WhileVertex::getConditionExpr(){
    return this->conditionExpr;
}

Expression* WhileVertex::getStartExpr(){
    return this->startExpr;
}

void WhileVertex::printInfo(std::ostream* outputTarget){
    *outputTarget << "Vertex number: " << this->getNumber() << std::endl;
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex type: ";
    *outputTarget << this->getVertexType() << " " << "(while)" << std::endl;
    *outputTarget << "Vertex line: " << this->getLine() << std::endl;
    *outputTarget << "Vertex depth: " << this->getDepth() << std::endl;

    *outputTarget << "Iterator: " + this->getIterator()->getName() << std::endl;
    *outputTarget << "Out name: " + this->getOutName()->getName() << std::endl;
    *outputTarget << "Condition expression: " + this->getConditionExpr()->getExpr()->to_string() << std::endl;
    *outputTarget << "Start expression: " + this->getStartExpr()->getExpr()->to_string() << std::endl;

    *outputTarget << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;
}

IfVertex::IfVertex(int depth, int number, int line,
    Expression* conditionExpr,
    Vertex* parent, std::string fileName){

    this->vertexType = ifVF;
    this->name = "if";

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;
    this->fileName = fileName;

    this->conditionExpr = conditionExpr;

    this->in = {};
    this->out = {};
}

Expression* IfVertex::getConditionExpr(){
    return this->conditionExpr;
}

void IfVertex::printInfo(std::ostream* outputTarget){
    *outputTarget << "Vertex number: " << this->getNumber() << std::endl;
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex type: ";
    *outputTarget << this->getVertexType() << " " << "(if)" << std::endl;
    *outputTarget << "Vertex line: " << this->getLine() << std::endl;
    *outputTarget << "Vertex depth: " << this->getDepth() << std::endl;

    *outputTarget << "Condition expression: " + this->getConditionExpr()->getExpr()->to_string() << std::endl;

    *outputTarget << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;
}

LetVertex::LetVertex(int depth, int number, int line,
    std::vector<LetName*>* letNamesVector,
    Vertex* parent, std::string fileName){

    this->vertexType = letVF;
    this->name = "let";

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;
    this->fileName = fileName;

    this->letNamesVector = letNamesVector;

    this->in = {};
    this->out = {};
}

std::vector<LetName*>* LetVertex::getLetNamesVector(){
    return this->letNamesVector;
}

void LetVertex::printInfo(std::ostream* outputTarget){
    *outputTarget << "Vertex number: " << this->getNumber() << std::endl;
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex type: ";
    *outputTarget << this->getVertexType() << " " << "(let)" << std::endl;
    *outputTarget << "Vertex line: " << this->getLine() << std::endl;
    *outputTarget << "Vertex depth: " << this->getDepth() << std::endl;

    *outputTarget << "Assigned expressions and their names: " << std::endl;
    for (auto assignment: *(this->getLetNamesVector())){
        *outputTarget << assignment->getName() << " = " << assignment->getReference()->getExpr()->to_string() << std::endl;
    }

    *outputTarget << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        *outputTarget << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    *outputTarget << std::endl;
}
