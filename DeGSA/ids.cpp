#include "ids.hpp"

std::string Identifier::getName(){
    return this->name;
}

IdentifierType Identifier::getType(){
    return this->type;
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

BaseDFName::BaseDFName(std::string name, int line){
    this->name = name;
    this->type = baseDFName;
    this->sizeToUseDefVectors = {};
    this->line = line;
}

BaseDFName::~BaseDFName(){}

IndexedDFName::IndexedDFName(std::string name, Identifier* base, std::vector<expr*> expressionsVector, int line){
    this->name = name;
    this->type = indexedDFName;

    if ((base->getType() != baseDFName) && (base->getType() != letName)){
        //TODO error!
    }

    this->base = base;
    this->expressionsVector = expressionsVector;
    this->line = line;
}

Identifier* IndexedDFName::getBase(){
    return this->base;
}

std::vector<expr*> IndexedDFName::getExpressionsVector(){
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

WhileIteratorName::WhileIteratorName(std::string name, expr* conditionExpr, expr* startExpr){
    this->name = name;
    this->conditionExpr = conditionExpr;
    this->startExpr = startExpr;
}

WhileIteratorName::~WhileIteratorName(){}

std::set<std::pair<Identifier*, int>> WhileOutName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

//todo this is wrong
int WhileOutName::getLine(){
    return this->whileVertex->getLine();
}

WhileOutName::WhileOutName(std::string name){
    this->name = name;
}

WhileOutName::~WhileOutName(){}

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

LetName::LetName(std::string name, expr* assignedExpr, std::set<Identifier*> nameReferenceSet){
    this->name = name;
    this->type = letName;
    this->assignedExpr = assignedExpr;
    this->nameReferenceSet = nameReferenceSet;
}

LetName::~LetName(){}
