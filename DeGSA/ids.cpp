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

Expression* SubArgName::getReference(){
    return this->reference;
}

int SubArgName::getLine(){
    return this->line;
}

std::vector<std::string> SubArgName::markAsUse(Vertex* currentVertex, int size){
    return reference->markAsUse(currentVertex, size);
}

std::vector<std::string> SubArgName::markAsDef(Vertex* currentVertex, int size){
    return reference->markAsDef(currentVertex, size);
}

SubArgName::SubArgName(std::string name, Expression* reference, int line){
    this->type = subArgNameType;
    this->name = name;
    this->reference = reference;
    this->line = line;
}

SubArgName::~SubArgName(){}

std::vector<std::string> BaseDFName::markAsUse(Vertex* vertex, int size){

    std::cout << "Base DF name " << this->getName() << " is being marked as used" << std::endl;

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

    return {};
}

std::vector<std::string> BaseDFName::markAsDef(Vertex* vertex, int size){

    std::cout << "Base DF name " << this->getName() << " is being marked as defined" << std::endl;

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

    return {};
}

std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> BaseDFName::getMap(){
    return this->sizeToUseDefVectors;
}

int BaseDFName::getLine(){
    return this->line;
}

BaseDFName::BaseDFName(std::string name, int line){
    this->name = name;
    this->type = baseDFNameType;
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

int IndexedDFName::getLine(){
    return this->line;
}

std::vector<std::string> IndexedDFName::markAsUse(Vertex* currentVertex, int size){
    std::cout << "Indexed DF name " << this->getName() << " of size " << this->getExpressionsVector().size() << " is being marked as used" << std::endl;
    std::vector<std::string> reports = {};
    for (auto exp: expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsUse()" << std::endl;
        } else {
            for (auto r: exp->markAsUse(currentVertex, 0)) reports.push_back(r);
        }
    }
    for (auto r: base->markAsUse(currentVertex, size + expressionsVector.size())) reports.push_back(r);
    return reports;
}

std::vector<std::string> IndexedDFName::markAsDef(Vertex* currentVertex, int size){
    std::cout << "Indexed DF name " << this->getName() << " of size " << this->getExpressionsVector().size() << " is being marked as defined" << std::endl;
    std::vector<std::string> reports = {};
    for (auto exp: expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsDef()" << std::endl;
        } else {
            for (auto r: exp->markAsUse(currentVertex, 0)) reports.push_back(r);
        }
    }
    for (auto r: base->markAsDef(currentVertex, size + expressionsVector.size())) reports.push_back(r);
    return reports;
}

IndexedDFName::IndexedDFName(std::string name, Identifier* base, std::vector<Expression*> expressionsVector, int line){
    this->name = name;
    this->type = indexedDFNameType;

    if ((base->getType() != baseDFNameType) && (base->getType() != letNameType) && (base->getType() != subArgNameType)){
        //TODO error!
    }

    this->base = base;
    this->expressionsVector = expressionsVector;
    this->line = line;
}

IndexedDFName::~IndexedDFName(){}

Expression* ForIteratorName::getLeftBorder(){
    if (this->forVertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(this->forVertex)->getLeftBorder();
    }
}

Expression* ForIteratorName::getRightBorder(){
    if (this->forVertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(this->forVertex)->getRightBorder();
    }
}

int ForIteratorName::getLine(){
    if (forVertex != nullptr) return this->forVertex->getLine();
    else return -1;
}

void ForIteratorName::setVertex(Vertex* currentVertex){
    if (this->forVertex == nullptr) {
        this->forVertex = currentVertex;
    }
}

std::vector<std::string> ForIteratorName::markAsUse(Vertex* currentVertex, int size){
    std::cout << "For iterator " << this->getName() << " is being marked as used" << std::endl;
    this->useSet.insert(currentVertex);
    return {};
}

std::vector<std::string> ForIteratorName::markAsDef(Vertex* currentVertex, int size){
    std::cout << "For iterator " << this->getName() << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by ForVertex automatically)
    std::vector<std::string> reports = {};
    reports.push_back("ERROR: markAsDef called on \"for\" iterator");
    return reports;
}

ForIteratorName::ForIteratorName(std::string name){
    this->name = name;
    this->type = forIteratorNameType;
    this->forVertex = nullptr;
    this->useSet = {};
    this->defSet = {};//todo
    this->line = -2;//todo
}

ForIteratorName::~ForIteratorName(){}

Expression* WhileIteratorName::getConditionExpr(){
    if (this->whileVertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(this->whileVertex)->getConditionExpr();
    }
}

Expression* WhileIteratorName::getStartExpr(){
    if (this->whileVertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(this->whileVertex)->getStartExpr();
    }
}

int WhileIteratorName::getLine(){
    if (whileVertex != nullptr) return this->whileVertex->getLine();
    else return -1;
}

void WhileIteratorName::setVertex(Vertex* currentVertex){
    if (this->whileVertex == nullptr) {
        this->whileVertex = currentVertex;
    }
}

std::vector<std::string> WhileIteratorName::markAsUse(Vertex* currentVertex, int size){
    std::cout << "While iterator " << this->getName() << " is being marked as used" << std::endl;
    this->useSet.insert(currentVertex);
    return {};
}

std::vector<std::string> WhileIteratorName::markAsDef(Vertex* currentVertex, int size){
    std::cout << "While iterator " << this->getName() << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by WhileVertex automatically)
    std::vector<std::string> reports = {};
    //todo add line
    reports.push_back("ERROR: markAsDef called on \"while\" iterator");
    return reports;
}

WhileIteratorName::WhileIteratorName(std::string name){
    this->name = name;
    this->type = whileIteratorNameType;
    this->whileVertex = nullptr;
    this->useSet = {};
    this->defSet = {};//todo
    this->line = -2;//todo
}

WhileIteratorName::~WhileIteratorName(){}

Expression* LetName::getReference(){
    return this->reference;
}

int LetName::getLine(){
    return this->letVertex->getLine();
}

void LetName::setVertex(Vertex* currentVertex){
    if (this->letVertex == nullptr) {
        this->letVertex = currentVertex;
    }
}

std::vector<std::string> LetName::markAsUse(Vertex* currentVertex, int size){
    std::cout << "Let name " << this->getName() << " is being marked as used" << std::endl;
    return this->reference->markAsUse(currentVertex, size);
}

std::vector<std::string> LetName::markAsDef(Vertex* currentVertex, int size){
    std::cout << "Let name " << this->getName() << " is being marked as defined" << std::endl;
    return this->reference->markAsDef(currentVertex, size);
}

LetName::LetName(std::string name, Expression* assignedExpression){
    this->name = name;
    this->type = letNameType;
    this->letVertex = nullptr;
    this->useSet = {};
    this->defSet = {};
    this->reference = assignedExpression;
}

LetName::~LetName(){}

int MainArgName::getLine(){
    return this->mainVertex->getLine();
}

void MainArgName::setVertex(Vertex* currentVertex){
    if (this->mainVertex == nullptr) {
        this->mainVertex = currentVertex;
    }
}

std::vector<std::string> MainArgName::markAsUse(Vertex* currentVertex, int size){
    std::cout << "Main arg name " << this->getName() << " is being marked as used" << std::endl;
    //this->markAsUse(currentVertex, size);
    //todo indices + where to store information about main arg being used?
    return {};
}

std::vector<std::string> MainArgName::markAsDef(Vertex* currentVertex, int size){
    std::cout << "Main arg name " << this->getName() << " is being marked as defined" << std::endl;
    //todo it is an error right?
    std::vector<std::string> reports = {};
    reports.push_back("INTERNAL ERROR: trying to mark argument of main() as def by vertex at line " + currentVertex->getLine());
    return reports;
}

MainArgName::MainArgName(std::string name){
    this->name = name;
    this->type = mainArgNameType;
    this->mainVertex = nullptr;
    this->useSet = {};
    this->defSet = {};
}

MainArgName::~MainArgName(){}

// it is used inside Expression constructor when engaging indexed name
// todo check if it works
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    std::vector<std::string>* errorReports){

    // expression could be either simple or a complex DF
    int indices = -1;
    std::string baseName;
    // complexDF is used to avoid corrupting expression object
    expr* complexDF = expression; // in our terms every DF based on a baseName is complex
    while(true){
        indices++;
        if (dynamic_cast<complex_id*>(complexDF) == nullptr){ // base name found (no indices)
            baseName = complexDF->to_string();
            break;
        } else {
            complexDF = dynamic_cast<complex_id*>(complexDF)->id_; // still has indices
        }
    }

    // now create an IndexedDFName and initialize it
    std::vector<Expression*> expressionsVector(indices);
    complex_id* newComplexDF = dynamic_cast<complex_id*>(expression);
    for (int i = 0; i < indices; i++){
        expressionsVector[indices - i - 1] = new Expression(newComplexDF->expr_, nameTable, errorReports);
        newComplexDF = dynamic_cast<complex_id*>(newComplexDF->id_);
    }
    auto base = nameTable.find(baseName);
    if (base != nameTable.end()){
        //TODO check if indexation is allowed; report if not
        // get roots to check if we are allowed to indexate base name
        // indexation is allowed if baseName has a type of:
        // baseDFName, TODO
        // indexation is not allowed if baseName has a type of:
        // for iterator, while iterator
        IndexedDFName* temp = new IndexedDFName(baseName, base->second, expressionsVector, line);
        std::cout << "Created new IndexedDFName object with base name " << baseName << " and expressionsVector: ";
        std::cout << std::flush;
        for (auto e: temp->getExpressionsVector()){
            std::cout << e->getExpr()->to_string() << " " << std::flush;
        }
        std::cout << std::endl;
        return temp;
    } else {
        std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- no base name found visible: " << baseName << std::endl;
        std::string report = "ERROR: no name \"" + baseName + "\" found at line " + std::to_string(line) + "\n";
        errorReports->push_back(report);
        return nullptr;
    }
}
