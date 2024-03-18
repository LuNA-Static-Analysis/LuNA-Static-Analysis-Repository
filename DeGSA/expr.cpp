#include "expr.hpp"

ExpressionType Expression::getType(){
    return this->type;
}

expr* Expression::getExpr(){
    return this->ASTexpr;
}

std::string Expression::getConstant(){
    return this->constant;
}

Expression::Expression(std::string constant, ExpressionType type){//todo
    this->ASTexpr = nullptr;
    this->constant = constant;
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->type = type;
}

Expression::Expression(expr* ASTexpr){
    this->ASTexpr = ASTexpr;
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
}

// this function gets an expression, recursively goes through it and
// returns an obejct of type Expression, plus creates objects for IndexedDFs
// nameTable stores information about what Ids are visible currently, and we can
// find the Identifier object by its name
// if there is no required Identifiers, then it's an error
Expression::Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, std::vector<std::string>* errorReports){

    this->ASTexpr = ASTexpr;

    // default initialization
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;

    integer* lunaInteger = dynamic_cast<integer*>(ASTexpr);//todo does not happen
    if (lunaInteger != NULL) {
        this->type = intNode;
        this->constant = lunaInteger->to_string();
        return;
    }

    real* lunaReal = dynamic_cast<real*>(ASTexpr); //todo does not happen
    if (lunaReal != NULL) {
        this->type = realNode;
        this->constant = lunaReal->to_string();
        return;
    }

    luna_string* lunaString = dynamic_cast<luna_string*>(ASTexpr);
    if (lunaString != NULL){
        //todo is real and is int and is string; merge to master, find in utils.cpp
        //lunaString.is_real();
        this->type = stringNode;
        this->constant = lunaString->to_string();
        return;
    }

    luna_cast* lunaCast = dynamic_cast<luna_cast*>(ASTexpr);
    if (lunaCast != NULL){
        this->type = realCastNode;//todo add type to which we are casting
        this->leftExpr = new Expression(lunaCast->expr_, nameTable, errorReports);
        return;
    }

    bin_op* lunaBinOp = dynamic_cast<bin_op*>(ASTexpr);
    if (lunaBinOp != NULL){

        // deduction of binary operation type
        std::string op = (lunaBinOp->to_string()).substr(lunaBinOp->left_->to_string().size(), 
            lunaBinOp->to_string().size() - lunaBinOp->left_->to_string().size() - lunaBinOp->right_->to_string().size());
        while (op[0] == ' '){
            op = op.substr(1, op.size() - 1);
        }
        switch(op[0]){
            case '+': this->type = addNode; break;
            case '-': this->type = subtractNode; break;
            case '*': this->type = multiplyNode; break;
            case '/': this->type = divideNode; break;
            case '>': this->type = greaterNode; break;
            case '<': this->type = lesserNode; break;
            case '=': if (op.size() > 1 && op[1] == '=') {this->type = equalNode; break;}
            case '!': if (op.size() > 1 && op[1] == '=') {this->type = nonEqualNode; break;}
            default: std::cout << "INTERNAL ERROR: UNKNOWN OPERATION IN EXPRESSION CONSTRUCTOR AT LINE " << ASTexpr->line_ << ": " << op[0] << std::endl;
                this->type = addNode; //todo temporary
        }

        this->leftExpr = new Expression(lunaBinOp->left_, nameTable, errorReports);
        this->rightExpr = new Expression(lunaBinOp->right_, nameTable, errorReports);
        return;
    }

    // name found
    id* df = dynamic_cast<id*>(ASTexpr);
    if (df != NULL){

        this->type = identifierNode;

        simple_id* simpleDF = dynamic_cast<simple_id*>(ASTexpr);
        // this is whatever name -- for iterator, while iterator, DF...
        // it is not necessarily a correct expression! it could be indexed "for" iterator, for example
        if (simpleDF != NULL){

            this->type = identifierNode;

            std::string simpleDFName = *(simpleDF->value_->value_);
            auto identifierName = nameTable.find(simpleDFName);
            if (identifierName != nameTable.end()){

                switch(identifierName->second->getType()){
                    case subArgNameType:
                        // argument of a sub
                        this->identifier = identifierName->second;
                        break;
                    case baseDFNameType:
                        // simple DF (no indices)
                        this->identifier = new IndexedDFName(simpleDFName, identifierName->second, {}, ASTexpr->line_);
                        break;
                    case forIteratorNameType:
                        this->identifier = identifierName->second;
                        break;
                    case whileIteratorNameType:
                        this->identifier = identifierName->second;
                        break;
                    case valueNameType:
                        this->identifier = identifierName->second;
                        break;
                    case letNameType:
                        this->identifier = identifierName->second;
                        break;
                    case mainArgNameType:
                        // argument of a main
                        this->identifier = identifierName->second;
                        break;
                    default:
                        std::cout << "INTERNAL ERROR: UNKNOWN IDENTIFIER TYPE" << std::endl;
                        break;
                }
            } else {
                std::string report = "ERROR: no name \"" + simpleDFName + "\" found!" + "\n";
                errorReports->push_back(report);
            }

            return;
        }

        complex_id* complexDF = dynamic_cast<complex_id*>(ASTexpr);
        if (complexDF != NULL){

            IndexedDFName* temp = parseIndexedDFExpression(complexDF, nameTable, ASTexpr->line_, errorReports);
            if (temp != nullptr){
                this->type = identifierNode;
                this->identifier = temp;
            } else {
                std::cout << "INTERNAL ERROR: creating Expression object (IndexedDFName) at line " + std::to_string(ASTexpr->line_) + "\n";
            }

            return;
        }

        std::cout << "INTERNAL ERROR: creating Expression object (Identifier) at line " + std::to_string(ASTexpr->line_) + "\n";
    }

    std::cout << "INTERNAL ERROR: creating Expression object at line " + std::to_string(ASTexpr->line_) + "\n";
    return;
}

//todo only works with constants!
Expression Expression::operator + (const Expression& other) {//todo how is expression transferred here?

        Expression left = this->getAsConstant();
        Expression right = other.getAsConstant();

        if (left.type == realNode && right.type == realNode ||
            left.type == intNode && right.type == realNode || 
            left.type == realNode && right.type == intNode){
            
            return Expression(
                std::to_string(stod(left.constant) + stod(right.constant)),
                realNode);
            
        } else if (this->type == intNode && other.type == intNode){
            return Expression(
                std::to_string(stoi(left.constant) + stoi(right.constant)),
                intNode);
        } else {
            std::cout << "INTERNAL ERROR: + operator used with unsuitable type" << std::endl;
            return Expression("", noneNode);
        }
}

Identifier* Expression::getAsIdentifier(){
    if (this->type == identifierNode){
        return this->identifier;
    } else {
        return nullptr;
    }
}

// naive implementation: returns noneNode once encounters an identifier
// creates new object and returns a pointer; never return already existing object!
Expression Expression::getAsConstant() const {
    std::cout << this->type << std::endl;
    switch (this->type){
        case addNode: 
            std::cout << "cool" << std::endl;
            return *(this->leftExpr) + *(this->rightExpr);
            //todo cases
        case intNode://todo use copy constructors?
            std::cout << "very cool" << std::endl;
            return Expression(this->constant, intNode);
        case stringNode:
            return Expression(this->constant, stringNode);
        case realNode:
            return Expression(this->constant, realNode);
        default:
            std::cout << "INTERNAL ERROR: getAsConstant returned noneNode" << std::endl;
            return Expression("", noneNode);
    }
}

//todo implement interval analysis
std::vector<std::string> Expression::markAsUse(Vertex* currentVertex, int size){
    std::vector<std::string> reports = {};
    std::cout << "Expression " << this->getExpr()->to_string() << " is being marked as used" << std::endl;
    switch(type){

        case addNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case subtractNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case multiplyNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case divideNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case greaterNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;

        case lesserNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;

        case equalNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;

        case nonEqualNode:
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case assignNode:
            //todo error?
            std::cout << "INTERNAL ERROR (WARNING): Expression.markAsUse ended at assignNode \"default\"" << std::endl; break;
        
        case identifierNode:
            for (auto r: identifier->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case stringNode: return reports;

        case intNode: return reports;

        case realNode: return reports;

        case realCastNode: 
            for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;

        //todo other nodes!
        
        default: std::cout << "WARNING: Expression.markAsUse ended as \"default\"" << std::endl;

    }

    return reports;
}

//todo implement interval analysis
std::vector<std::string> Expression::markAsDef(Vertex* currentVertex, int size){
    std::vector<std::string> reports = {};
    std::cout << "Expression " << this->getExpr()->to_string() << " is being marked as defined" << std::endl;
    switch(type){

        case addNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (+) operation\n");
            return reports;
        
        case subtractNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (-) operation\n");
            return reports;
        
        case multiplyNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (*) operation\n");
            return reports;
        
        case divideNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (/) operation\n");
            return reports;
        
        case assignNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (=) operation\n");
            return reports;

        case greaterNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (>) operation\n");
            return reports;

        case lesserNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (<) operation\n");
            return reports;

        case equalNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (==) operation\n");
            return reports;

        case nonEqualNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (!=) operation\n");
            return reports;
        
        case identifierNode: 

            switch(identifier->getType()){

                case subArgNameType: { //ok
                    SubArgName* subArgName = dynamic_cast<SubArgName*>(identifier);
                    for (auto r: subArgName->getReference()->markAsDef(currentVertex, size)) reports.push_back(r);
                    return reports;
                }
                case baseDFNameType: {//ok
                    BaseDFName* baseDFName = dynamic_cast<BaseDFName*>(identifier);
                    for (auto r: baseDFName->markAsDef(currentVertex, size)) reports.push_back(r);
                    return reports;
                }
                case indexedDFNameType: {//ok
                    IndexedDFName* indexedDFName = dynamic_cast<IndexedDFName*>(identifier);
                    indexedDFName->markAsDef(currentVertex, size);
                    return reports;
                }
                case forIteratorNameType: //error
                    reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark \"for\" iterator\n");
                    return reports;
                
                case whileIteratorNameType: //error
                    reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark \"while\" iterator\n");
                    return reports;
                
                case valueNameType://todo
                    return reports;
                
                case letNameType: {//ok
                    LetName* letName = dynamic_cast<LetName*>(identifier);
                    for (auto r: letName->getReference()->markAsDef(currentVertex, size)) reports.push_back(r);
                    return reports;
                }
                case mainArgNameType:
                    reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark main() argument\n");
                    return reports;
                
                default:
                    std::cout << "INTERNAL ERROR (WARNING): Expression.markAsDef() ended by default at identifier node" << std::endl;
                    return reports;
                
            }
        
        case stringNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA string constant\n");
            return reports;
        
        case intNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA int constant\n");
            return reports;
        
        case realNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA real constant\n");
            return reports;
        
        case realCastNode:
             reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA cast\n");
            return reports;
        
        //todo other casts!

        default: 
            std::cout << "WARNING: Expression.markAsDef ended as \"default\"" << std::endl;
            return reports;
    }

    return reports;
}
