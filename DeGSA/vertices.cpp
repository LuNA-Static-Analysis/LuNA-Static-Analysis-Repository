#include "enums.hpp"
#include "vertices.hpp"

Vertex::Binding::Binding(Vertex* pointerTo, Id* id){
    this->id = id;
    this->pointerTo = pointerTo;
}

Id* Vertex::Binding::getId(){
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

//virtual ~Vertex();

VertexType Vertex::getVertexType(){
    return vertexType;
}

std::set<Id*> Vertex::getUseSet(){
    return use;
}

std::set<Id*> Vertex::getDefSet(){
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

void Vertex::addIn(Vertex* vertex, Id* id){
    this->in.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

void Vertex::addOut(Vertex* vertex, Id* id){
    this->out.insert(Binding(vertex, id)); //TODO this should not allow duplicates; does it allow it here?
}

void Vertex::addInside(Vertex* vertex){
    auto temp = this->inside.find(vertex);
    if (temp == this->inside.end()){
        this->inside.insert(vertex);
    }
}

void Vertex::addUse(Id* id){
    auto temp = this->use.find(id);
    if (temp == this->use.end()){
        this->use.insert(id);
    }
}

void Vertex::addDef(Id* id){
    auto temp = this->def.find(id);
    if (temp == this->def.end()){
        this->def.insert(id);
    }
}

std::vector<Id*> Vertex::getDeclaredInsideDFsVector(){
    return declaredInsideDFsVector;
}

std::vector<Id*> Vertex::getDeclaredOutsideDFsVector(){
    return declaredOutsideDFsVector;
}

std::vector<Id*> Vertex::getDeclaredBothDFsVector(){
    return declaredBothDFsVector;
}

std::set<Id*> Vertex::getDeclaredBothDFsSet(){
    return declaredBothDFsSet;
}

void Vertex::setDeclaredInsideDFsVector(std::vector<Id*> declaredInsideDFsVector){
    this->declaredInsideDFsVector = declaredInsideDFsVector;
}

void Vertex::setDeclaredOutsideDFsVector(std::vector<Id*> declaredOutsideDFsVector){
    this->declaredOutsideDFsVector = declaredOutsideDFsVector;
}

void Vertex::setDeclaredBothDFsVector(std::vector<Id*> declaredBothDFsVector){
    this->declaredBothDFsVector = declaredBothDFsVector;
}

void Vertex::setDeclaredBothDFsSet(std::set<Id*> declaredBothDFsSet){
    this->declaredBothDFsSet = declaredBothDFsSet;
}

void Vertex::printInfo(){ // virtual?
    std::cout << "printInfo() placeholder" << std::endl;
}


//todo check if this works
CFVertex::CFVertex(int depth, int number, int line,
    std::string name, VertexType vertexType){

    this->depth = depth;
    this->number = number;
    this->line = line;

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
    for (Id* i: this->getDeclaredOutsideDFsVector()){//TODO
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (Id* i: this->getDeclaredInsideDFsVector()){//TODO
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Id* i: this->getUseSet()){//TODO
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Id* i: this->getDefSet()){//TODO
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

//todo check if this works
ForVertex::ForVertex(int depth, int number, int line,
    Id* iterator, expr* leftBorder, expr* rightBorder){

    this->vertexType = forVF;

    this->depth = depth;
    this->number = number;
    this->line = line;

    this->iterator = iterator;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;
}

Id* ForVertex::getIterator(){
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
    for (Id* i: this->getDeclaredOutsideDFsVector()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Declared inside DFs:";
    for (Id* i: this->getDeclaredInsideDFsVector()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Use DFs:";
    for (Id* i: this->getUseSet()){
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "Def DFs:";
    for (Id* i: this->getDefSet()){
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
