#include "ids.hpp"
#include "json_reporter.cpp"

Vertex* Identifier::getVertex(){
    return this->vertex;
}

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

int Identifier::getLine(){
    if (this->vertex != nullptr){
        return this->vertex->getLine();
    } else {
        return -1;
    }
}

void Identifier::setVertex(Vertex* currentVertex){
    this->vertex = currentVertex;
}

Identifier::Identifier(){}

Identifier::~Identifier(){}

Expression* SubArgName::getReference(){
    return this->reference;
}

std::vector<std::string> SubArgName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    return reference->markAsUse(currentVertex, size);
}

std::vector<std::string> SubArgName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);
    return reference->markAsDef(currentVertex, size);
}

bool SubArgName::isIndexable(){
    return reference->isIndexable();
}

SubArgName::SubArgName(std::string name, Expression* reference){
    this->type = subArgNameType;
    this->name = name;
    this->reference = reference;
}

SubArgName::~SubArgName(){}

std::vector<std::string> BaseDFName::markAsUse(Vertex* vertex, int size){
    useSet.insert(vertex);

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
    defSet.insert(vertex);

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

bool BaseDFName::isIndexable(){
    return true;
}

std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> BaseDFName::getMap(){
    return this->sizeToUseDefVectors;
}

BaseDFName::BaseDFName(std::string name){
    this->name = name;
    this->type = baseDFNameType;
    this->sizeToUseDefVectors = {};
}

BaseDFName::~BaseDFName(){}

Identifier* IndexedDFName::getBase(){
    return this->base;
}

std::vector<Expression*> IndexedDFName::getExpressionsVector(){
    return this->expressionsVector;
}

std::vector<std::string> IndexedDFName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    std::cout << "Indexed DF name " << this->getName() << " of size " << this->getExpressionsVector().size() << " is being marked as used" << std::endl;
    std::vector<std::string> reports = {};
    for (auto exp: expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsUse()" << std::endl;
        } else {
            for (auto r: exp->markAsUse(currentVertex, 0)) reports.push_back(r);
        }
    }
    if (base != nullptr)
        for (auto r: base->markAsUse(currentVertex, size + expressionsVector.size())) reports.push_back(r);
    else
        std::cout << "INTERNAL ERROR: markAsDef encountered nullptr base of an IndexedDF" << std::endl;
    return reports;
}

std::vector<std::string> IndexedDFName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);
    std::cout << "Indexed DF name " << this->getName() << " of size " << this->getExpressionsVector().size() << " is being marked as defined" << std::endl;
    std::vector<std::string> reports = {};
    for (auto exp: expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsDef()" << std::endl;
        } else {
            for (auto r: exp->markAsUse(currentVertex, 0)) reports.push_back(r);
        }
    }
    if (base != nullptr)
        for (auto r: base->markAsDef(currentVertex, size + expressionsVector.size())) reports.push_back(r);
    else
        std::cout << "INTERNAL ERROR: markAsDef encountered nullptr base of an IndexedDF" << std::endl;
    return reports;
}

bool IndexedDFName::isIndexable(){
    /* all indexed DFs are checked at construction, so already constucted
    ones are definetely indexable*/
    return true;
}

IndexedDFName::IndexedDFName(std::string name, Identifier* base, std::vector<Expression*> expressionsVector, std::vector<std::string>* errorReports, Vertex* currentVertex){
    this->name = name;
    this->type = indexedDFNameType;

    if ((base->getType() == baseDFNameType) ||
        (base->getType() == letNameType) ||
        (base->getType() == subArgNameType)
    ){
        this->base = base;
    } else {
        this->base = nullptr;
        errorReports->push_back(JsonReporter::create36(name, currentVertex));

        std::cout << "INTERNAL ERROR: indexation of an unsuitable Identifier" << std::endl;
    }

    this->expressionsVector = expressionsVector;
}

IndexedDFName::~IndexedDFName(){}

Expression* ForIteratorName::getLeftBorder(){
    if (this->vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(this->vertex)->getLeftBorder();
    }
}

Expression* ForIteratorName::getRightBorder(){
    if (this->vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(this->vertex)->getRightBorder();
    }
}

std::vector<std::string> ForIteratorName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    std::cout << "For iterator " << this->getName() << " is being marked as used" << std::endl;
    this->useSet.insert(currentVertex);
    return {};
}

std::vector<std::string> ForIteratorName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);//todo is it ok?
    std::cout << "INTERNAL ERROR: for iterator " << this->getName() << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by ForVertex automatically)
    std::vector<std::string> reports = {};
    reports.push_back(JsonReporter::create26(
        this->getName(),
        currentVertex
    ));
    return reports;
}

bool ForIteratorName::isIndexable(){
    return false;
}

void ForIteratorName::setVertex(Vertex* vertex){
    if (this->vertex == nullptr){
        this->vertex = vertex;
        this->defSet.insert(vertex);
        vertex->addDef(this);
    }
}

ForIteratorName::ForIteratorName(std::string name){
    this->name = name;
    this->type = forIteratorNameType;
    this->vertex = nullptr;
    this->useSet = {};
    this->defSet = {};
}

ForIteratorName::~ForIteratorName(){}

Expression* WhileIteratorName::getConditionExpr(){
    if (this->vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(this->vertex)->getConditionExpr();
    }
}

Expression* WhileIteratorName::getStartExpr(){
    if (this->vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(this->vertex)->getStartExpr();
    }
}

std::vector<std::string> WhileIteratorName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    std::cout << "While iterator " << this->getName() << " is being marked as used" << std::endl;
    this->useSet.insert(currentVertex);
    return {};
}

std::vector<std::string> WhileIteratorName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);//todo is it ok?
    std::cout << "INTERNAL ERROR: while iterator " << this->getName() << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by WhileVertex automatically)
    std::vector<std::string> reports = {};
    reports.push_back(JsonReporter::create26(
        this->getName(),
        currentVertex
    ));
    return reports;
}

bool WhileIteratorName::isIndexable(){
    return false;
}

void WhileIteratorName::setVertex(Vertex* vertex){
    if (this->vertex == nullptr){
        this->vertex = vertex;
        this->defSet.insert(vertex);
        vertex->addDef(this);
    }
}

WhileIteratorName::WhileIteratorName(std::string name){
    this->name = name;
    this->type = whileIteratorNameType;
    this->vertex = nullptr;
    this->useSet = {};
    this->defSet = {};
}

WhileIteratorName::~WhileIteratorName(){}

Expression* LetName::getReference(){
    return this->reference;
}

std::vector<std::string> LetName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    std::cout << "Let name " << this->getName() << " is being marked as used" << std::endl;
    return this->reference->markAsUse(currentVertex, size);
}

std::vector<std::string> LetName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);
    std::cout << "Let name " << this->getName() << " is being marked as defined" << std::endl;
    return this->reference->markAsDef(currentVertex, size);
}

bool LetName::isIndexable(){
    return reference->isIndexable();
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

std::vector<std::string> MainArgName::markAsUse(Vertex* currentVertex, int size){
    useSet.insert(currentVertex);
    std::cout << "Main arg name " << this->getName() << " is being marked as used" << std::endl;
    this->useSet.insert(currentVertex);
    return {};
}

std::vector<std::string> MainArgName::markAsDef(Vertex* currentVertex, int size){
    defSet.insert(currentVertex);
    std::cout << "Main arg name " << this->getName() << " is being marked as defined" << std::endl;
    std::vector<std::string> reports = {};
    reports.push_back(JsonReporter::create26(
        this->getName(),
        currentVertex
    ));
    return reports;
}

bool MainArgName::isIndexable(){
    return false;
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
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    std::vector<std::string>* errorReports, Vertex* currentVertex){

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
        expressionsVector[indices - i - 1] = new Expression(newComplexDF->expr_, nameTable, errorReports, currentVertex);
        newComplexDF = dynamic_cast<complex_id*>(newComplexDF->id_);
    }
    auto base = nameTable.find(baseName);
    if (base != nameTable.end()){

        if (base->second->isIndexable()){
            IndexedDFName* temp = new IndexedDFName(baseName, base->second, expressionsVector, errorReports, currentVertex);
            std::cout << "Created new IndexedDFName object with base name " << baseName << " and expressionsVector: ";
            std::cout << std::flush;
            for (auto e: temp->getExpressionsVector()){
                std::cout << e->getExpr()->to_string() << " " << std::flush;
            }
            std::cout << std::endl;
        return temp;
        } else {
            std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- base is not indexable: " << baseName << std::endl;
            //std::string report = "ERROR: not indexable name \"" + baseName + "\" indexed at line " + std::to_string(line) + "\n";
            errorReports->push_back(JsonReporter::create36(
                baseName,
                currentVertex
            ));
            return nullptr;
        }
    } else {
        std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- no base name found visible: " << baseName << std::endl;
        //std::string report = "ERROR: no name \"" + baseName + "\" found at line " + std::to_string(line) + "\n";
        errorReports->push_back(JsonReporter::create14(//todo
            base->second
        ));
        return nullptr;
    }
}
