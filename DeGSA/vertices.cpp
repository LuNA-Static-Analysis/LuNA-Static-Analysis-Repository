#include "enums.hpp"
#include "vertices.hpp"

Vertex::Binding::Binding(Vertex* pointerTo, BaseDFName* id){
    this->id = id;
    this->pointerTo = pointerTo;
}

BaseDFName* Vertex::Binding::getId(){
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

std::set<BaseDFName*> Vertex::getUseSet(){
    return use;
}

std::set<BaseDFName*> Vertex::getDefSet(){
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

//todo DFR redo this
void Vertex::addIn(Vertex* vertex, BaseDFName* id){
    this->in.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

//todo DFR redo this
void Vertex::addOut(Vertex* vertex, BaseDFName* id){
    this->out.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

void Vertex::addInside(Vertex* vertex){
    auto temp = this->inside.find(vertex);
    if (temp == this->inside.end()){
        this->inside.insert(vertex);
    }
}

void Vertex::addUse(BaseDFName* id){
    auto temp = this->use.find(id);
    if (temp == this->use.end()){
        this->use.insert(id);
    }
}

void Vertex::addDef(BaseDFName* id){
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

void Vertex::setDeclaredInsideIdsMap(std::map<std::string, Identifier*> declaredInsideIdsMap){
    this->declaredInsideIdsMap = declaredInsideIdsMap;
}

void Vertex::setDeclaredOutsideIdsMap(std::map<std::string, Identifier*> declaredOutsideIdsMap){
    this->declaredOutsideIdsMap = declaredOutsideIdsMap;
}

void Vertex::setDeclaredBothIdsMap(std::map<std::string, Identifier*> declaredBothIdsMap){
    this->declaredBothIdsMap = declaredBothIdsMap;
}

void Vertex::printInfo(){ // virtual?
    std::cout << "printInfo() placeholder" << std::endl;
}

void Vertex::printCallStack(){
    std::cout << "Address: " << this;
    std::cout << "; type: " << this->getVertexType();
    std::cout << "; line: " << this->getLine() << std::endl;
    if (this->getParent() != nullptr){
        this->getParent()->printCallStack();
    }
}


CFVertex::CFVertex(int depth, int number, int line,
    std::string name, VertexType vertexType, Vertex* parent){

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;

    this->name = name;
    this->vertexType = vertexType; // import or sub
}

std::string CFVertex::getName(){
    return name;
}

void CFVertex::printInfo() {

    std::cout << "Vertex number: " << this->getNumber() << std::endl;
    std::cout << "Vertex address: " << this << std::endl;
    std::cout << "Vertex type: ";
    std::cout << this->getVertexType() << " ";
    switch (this->getVertexType()){
        case importVF:
            std::cout << "(atomic CF); name: " << this->getName() << std::endl;
            break;
        case subVF:
            std::cout << "(structured CF);  name: " << this->getName() << std::endl;
            break;
        default:
            std::cout << "(unknown)" << std::endl;
            break;
    }
    std::cout << "Vertex line: " << this->getLine() << std::endl;
    std::cout << "Vertex depth: " << this->getDepth() << std::endl;

    std::cout << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){//TODO
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){//TODO
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Identifier* i: this->getUseSet()){//TODO
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Identifier* i: this->getDefSet()){//TODO
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices inside:";
    for (Vertex* v: this->getInsideSet()){
        std::cout << " " << v;
    }
    std::cout << std::endl;

    std::cout << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

    std::cout << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

}

ForVertex::ForVertex(int depth, int number, int line,
    ForIteratorName* iterator, expr* leftBorder, expr* rightBorder,
    Vertex* parent){

    this->vertexType = forVF;

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;

    this->iterator = iterator;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;
}

ForIteratorName* ForVertex::getIterator(){
    return iterator;
}

expr* ForVertex::getLeftBorder(){
    return leftBorder;
}

expr* ForVertex::getRightBorder(){
    return rightBorder;
}

void ForVertex::printInfo(){

    std::cout << "Vertex number: " << this->getNumber() << std::endl;
    std::cout << "Vertex address: " << this << std::endl;
    std::cout << "Vertex type: ";
    std::cout << this->getVertexType() << " " << "(for)" << std::endl;
    std::cout << "Vertex line: " << this->getLine() << std::endl;
    std::cout << "Vertex depth: " << this->getDepth() << std::endl;

    std::cout << "Iterator: " + this->getIterator()->getName() << std::endl;
    std::cout << "Left border: " + this->getLeftBorder()->to_string() << std::endl;
    std::cout << "Right border: " + this->getRightBorder()->to_string() << std::endl;

    std::cout << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

    std::cout << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

}

WhileVertex::WhileVertex(int depth, int number, int line,
    WhileIteratorName* iterator, WhileOutName* outName, expr* conditionExpr, expr* startExpr,
    Vertex* parent){

    this->vertexType = whileVF;

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;

    this->iterator = iterator;
    this->outName = outName;
    this->conditionExpr = conditionExpr;
    this->startExpr = startExpr;
}

WhileIteratorName* WhileVertex::getIterator(){
    return this->iterator;
}

WhileOutName* WhileVertex::getOutName(){
    return this->outName;
}

expr* WhileVertex::getConditionExpr(){
    return this->conditionExpr;
}

expr* WhileVertex::getStartExpr(){
    return this->startExpr;
}

void WhileVertex::printInfo(){
    std::cout << "Vertex number: " << this->getNumber() << std::endl;
    std::cout << "Vertex address: " << this << std::endl;
    std::cout << "Vertex type: ";
    std::cout << this->getVertexType() << " " << "(while)" << std::endl;
    std::cout << "Vertex line: " << this->getLine() << std::endl;
    std::cout << "Vertex depth: " << this->getDepth() << std::endl;

    std::cout << "Iterator: " + this->getIterator()->getName() << std::endl;
    std::cout << "Out name: " + this->getOutName()->getName() << std::endl;
    std::cout << "Condition expression: " + this->getConditionExpr()->to_string() << std::endl;
    std::cout << "Start expression: " + this->getStartExpr()->to_string() << std::endl;

    std::cout << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

    std::cout << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;
}

IfVertex::IfVertex(int depth, int number, int line,
    expr* conditionExpr,
    Vertex* parent){

    this->vertexType = whileVF;

    this->depth = depth;
    this->number = number;
    this->line = line;
    this->parent = parent;

    this->conditionExpr = conditionExpr;
}

expr* IfVertex::getConditionExpr(){
    return this->conditionExpr;
}

void IfVertex::printInfo(){
    std::cout << "Vertex number: " << this->getNumber() << std::endl;
    std::cout << "Vertex address: " << this << std::endl;
    std::cout << "Vertex type: ";
    std::cout << this->getVertexType() << " " << "(if)" << std::endl;
    std::cout << "Vertex line: " << this->getLine() << std::endl;
    std::cout << "Vertex depth: " << this->getDepth() << std::endl;

    std::cout << "Condition expression: " + this->getConditionExpr()->to_string() << std::endl;

    std::cout << "Declared outside DFs:";
    for (auto i: this->getDeclaredOutsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (auto i: this->getDeclaredInsideIdsMap()){
        std::cout << " " << i.first;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Identifier* i: this->getUseSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Identifier* i: this->getDefSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices inside:";
    for (auto i: this->getInsideSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Vertices before (\"in\"):";
    for (auto b: this->getInSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;

    std::cout << "Vertices after (\"out\"):";
    for (auto b: this->getOutSet()){
        std::cout << " " << b.getPointerTo() << " [" << b.getPointerTo()->getNumber() << "] (" << b.getId() << ")";
    }
    std::cout << std::endl;
}
