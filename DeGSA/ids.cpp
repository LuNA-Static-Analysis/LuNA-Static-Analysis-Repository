#include "ids.hpp"

std::string Identifier::getName(){
    return this->name;
}

IdentifierType Identifier::getType(){
    return this->type;
}

std::set<Vertex*> Identifier::getUseSet(){
    return useSet;
}

std::set<Vertex*> Identifier::getDefSet(){
    return defSet;
}

Identifier::Identifier(){}

Identifier::~Identifier(){}

std::set<Identifier*> SubArgName::getNameReferenceSet(){
    return this->nameReferenceSet;
}

std::set<std::pair<Identifier*, int>> SubArgName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    for (auto i: this->getNameReferenceSet()){
        auto temp = i->getRoots();
        for (auto j: temp){
            result.insert(j);
        }
    }

    return result;
}

int SubArgName::getLine(){
    return this->line;
}

void SubArgName::markAsUse(Vertex* currentVertex){
    reference->markAsUse(currentVertex);
}

void SubArgName::markAsDef(Vertex* currentVertex){
    reference->markAsDef(currentVertex);
}

SubArgName::SubArgName(std::string name, std::set<Identifier*> nameReferenceSet, int line){
    this->type = subArgName;
    this->name = name;
    this->nameReferenceSet = nameReferenceSet;
    this->line = line;
}

SubArgName::~SubArgName(){}

void BaseDFName::addUse(int size, Vertex* vertex){
    auto maybePair = this->sizeToUseDefVectors.find(size);
    if (maybePair != this->sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeUseVector = maybePair->second.first;
        if (maybeUseVector == nullptr) // check if it was defined and create new vector if not
            maybeUseVector = new std::vector<Vertex*>;
        // then add this vertex as use
        maybeUseVector->push_back(vertex);
    } else { // create new pair and a new vector
        this->sizeToUseDefVectors.insert(std::make_pair(size, std::make_pair(
            new std::vector<Vertex*>(),
            new std::vector<Vertex*>()
        )));
        this->sizeToUseDefVectors.find(size)->second.first->push_back(vertex);
    }
}

void BaseDFName::addDef(int size, Vertex* vertex){
    auto maybePair = this->sizeToUseDefVectors.find(size);
    if (maybePair != this->sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeDefVector = maybePair->second.second;
        if (maybeDefVector == nullptr) // check if it was defined and create new vector if not
            maybeDefVector = new std::vector<Vertex*>;
        // then add this vertex as def
        maybeDefVector->push_back(vertex);
    } else { // create new pair and a new vector
        this->sizeToUseDefVectors.insert(std::make_pair(size, std::make_pair(
            new std::vector<Vertex*>(),
            new std::vector<Vertex*>()
        )));
        this->sizeToUseDefVectors.find(size)->second.second->push_back(vertex);
    }
}

std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> BaseDFName::getMap(){
    return this->sizeToUseDefVectors;
}

std::set<std::pair<Identifier*, int>> BaseDFName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

int BaseDFName::getLine(){
    return this->line;
}

void BaseDFName::markAsUse(Vertex* currentVertex){
    //todo
}

void BaseDFName::markAsDef(Vertex* currentVertex){
    //todo
}

BaseDFName::BaseDFName(std::string name, int line){
    this->name = name;
    this->type = baseDFName;
    this->sizeToUseDefVectors = {};
    this->line = line;
}

BaseDFName::~BaseDFName(){}

Identifier* IndexedDFName::getBase(){
    return this->base;
}

std::vector<Expression*> IndexedDFName::getExpressionsVector(){
    return this->expressionsVector;
}

std::set<std::pair<Identifier*, int>> IndexedDFName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    std::set<std::pair<Identifier*, int>> baseRoots = this->base->getRoots();
    for (auto br: baseRoots){
        result.insert(std::make_pair(br.first, br.second + this->expressionsVector.size()));
    }
    return result;
}

int IndexedDFName::getLine(){
    return this->line;
}

void IndexedDFName::markAsUse(Vertex* currentVertex){
    for (auto exp: expressionsVector){
        exp->markAsUse(currentVertex);
    }
    base->markAsUse(currentVertex);//todo
}

void IndexedDFName::markAsDef(Vertex* currentVertex){
    for (auto exp: expressionsVector){
        exp->markAsUse(currentVertex);
    }
    base->markAsDef(currentVertex);//todo
}

IndexedDFName::IndexedDFName(std::string name, Identifier* base, std::vector<Expression*> expressionsVector, int line){
    this->name = name;
    this->type = indexedDFName;

    if ((base->getType() != baseDFName) && (base->getType() != letName)){
        //TODO error!
    }

    this->base = base;
    this->expressionsVector = expressionsVector;
    this->line = line;
}

IndexedDFName::~IndexedDFName(){}

expr* ForIteratorName::getLeftBorder(){
    return this->leftBorder;
}

expr* ForIteratorName::getRightBorder(){
    return this->rightBorder;
}

std::set<std::pair<Identifier*, int>> ForIteratorName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

int ForIteratorName::getLine(){
    return this->forVertex->getLine();
}

void ForIteratorName::markAsUse(Vertex* currentVertex){
    //markAsUse(currentVertex); //todo
}

void ForIteratorName::markAsDef(Vertex* currentVertex){
    //todo error (iterator should be marked as defined by ForVertex automatically)
}

ForIteratorName::ForIteratorName(std::string iteratorName, expr* leftBorder, expr* rightBorder){
    this->name = iteratorName;
    this->type = forIteratorName;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;
}

ForIteratorName::~ForIteratorName(){}

expr* WhileIteratorName::getConditionExpr(){
    return this->conditionExpr;
}

expr* WhileIteratorName::getStartExpr(){
    return this->startExpr;
}

std::set<std::pair<Identifier*, int>> WhileIteratorName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

int WhileIteratorName::getLine(){
    return this->whileVertex->getLine();
}

void WhileIteratorName::markAsUse(Vertex* currentVertex){
    //markAsUse(currentVertex); //todo
}

void WhileIteratorName::markAsDef(Vertex* currentVertex){
    //todo error (iterator should be marked as defined by WhileVertex automatically)
}

WhileIteratorName::WhileIteratorName(std::string name, expr* conditionExpr, expr* startExpr){
    this->type = whileIteratorName;
    this->name = name;
    this->conditionExpr = conditionExpr;
    this->startExpr = startExpr;
}

WhileIteratorName::~WhileIteratorName(){}

expr* LetName::getAssignedExpr(){
    return this->assignedExpr;
}

std::set<Identifier*> LetName::getNameReferenceSet(){
    return this->nameReferenceSet;
}

std::set<std::pair<Identifier*, int>> LetName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    for (auto i: this->getNameReferenceSet()){
        auto temp = i->getRoots();
        for (auto j: temp){
            result.insert(j);
        }
    }

    return result;
}

int LetName::getLine(){
    return this->letVertex->getLine();
}

void LetName::markAsUse(Vertex* currentVertex){
    //markAsUse(currentVertex); //todo
}

void LetName::markAsDef(Vertex* currentVertex){
    //todo
}

LetName::LetName(std::string name, expr* assignedExpr, std::set<Identifier*> nameReferenceSet){
    this->name = name;
    this->type = letName;
    this->assignedExpr = assignedExpr;
    this->nameReferenceSet = nameReferenceSet;
}

LetName::~LetName(){}

std::set<std::pair<Identifier*, int>> MainArgName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

int MainArgName::getLine(){
    return this->line;
}

void MainArgName::markAsUse(Vertex* currentVertex){
    //markAsUse(currentVertex); //todo
}

void MainArgName::markAsDef(Vertex* currentVertex){
    //todo error
}

MainArgName::MainArgName(std::string name){
    this->type = mainArgName;
    this->name = name;
    this->line = 0;
}

MainArgName::~MainArgName(){}

IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    std::vector<std::string>* errorReports){
    // expression could be either simple or a complex DF
    int indices = 0;
    std::string baseName;
    expr* complexDF = expression; // but in our terms every DF based on a baseName is complex
    while(true){
        indices++;
        if (dynamic_cast<complex_id*>(complexDF) == nullptr){
            baseName = complexDF->to_string();
            break;
        } else {
            complexDF = dynamic_cast<complex_id*>(complexDF)->id_;
        }
    }
    indices--;
    // now create an IndexedDFName and initialize it
    std::vector<Expression*> expressionsVector(indices);
    complex_id* newComplexDF = dynamic_cast<complex_id*>(expression);//todo rename
    for (int i = 0; i < indices; i++){
        std::cout << "idiot" << indices << std::endl;//debug todo
        std::cout << newComplexDF->to_string() << std::endl;
        //TODO isn't it backwards vector now?
        expressionsVector[i] = new Expression(newComplexDF->expr_);
        newComplexDF = dynamic_cast<complex_id*>(newComplexDF->id_);
    }
    auto base = nameTable.find(baseName);
    if (base != nameTable.end()){
        //TODO check if indexation is allowed; report if not
        IndexedDFName* temp = new IndexedDFName(baseName, base->second, expressionsVector, line);
        return temp;
    } else {
        std::string report = "ERROR: no name \"" + baseName + "\" found at line " + std::to_string(line) + "\n";
        errorReports->push_back(report);
        return nullptr;
    }
}
