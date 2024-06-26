#include "expr.hpp"
#include "json_reporter.cpp"

ExpressionType Expression::getType(){
    return this->type;
}

expr* Expression::getExpr(){
    return this->ASTexpr;
}

std::string Expression::getConstant(){
    return this->constant;
}

Vertex* Expression::getVertex(){
    return this->vertex;
}

Expression::Expression(std::string constant, ExpressionType type, Vertex* currentVertex){
    this->ASTexpr = nullptr;
    this->constant = constant;
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->type = type;
    this->vertex = currentVertex;
}

Expression::Expression(expr* ASTexpr, Vertex* currentVertex){
    this->ASTexpr = ASTexpr;
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->vertex = currentVertex;
}

/* 
    this function gets an expression, recursively goes through it and
    returns an obejct of type Expression, plus creates objects for IndexedDFs
    nameTable stores information about what Ids are visible currently, and we can
    find the Identifier object by its name
    if there is no required Identifiers, then it's an error
*/
//TODO stop parsing strings and use jsons or inheritance
Expression::Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, std::vector<std::string>* errorReports, Vertex* currentVertex){

    this->ASTexpr = ASTexpr;

    // default initialization
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->type = noneNode;
    this->vertex = currentVertex;

    luna_string* lunaString = dynamic_cast<luna_string*>(ASTexpr);
    if (lunaString != NULL){

        std::string s = *(lunaString->get_value());
        if (std::regex_match(s, std::regex("[0-9]+"))){ // int
            this->type = intNode;
        }
        if (std::regex_match(s, std::regex("[0-9]+[.][0-9]+"))){ // real
            this->type = realNode;
        }
        if (std::regex_match(s, std::regex("\"[^\"]*\""))){ // string
            this->type = stringNode;
        }

        this->constant = s;
        return;
    }

    luna_cast* lunaCast = dynamic_cast<luna_cast*>(ASTexpr);
    if (lunaCast != nullptr){

        to_int* intCast = dynamic_cast<to_int*>(lunaCast);
        if (intCast != nullptr)
            this->type = intCastNode;
        
        to_real* realCast = dynamic_cast<to_real*>(lunaCast);
        if (realCast != nullptr)
            this->type = realCastNode;

        to_str* stringCast = dynamic_cast<to_str*>(lunaCast);
        if (stringCast != nullptr)
            this->type = stringCastNode;

        this->leftExpr = new Expression(lunaCast->expr_, nameTable, errorReports, currentVertex);
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
            case '%': this->type = modulusNode; break;
            case '>':
                if (op.size() > 1 && op[1] == '=') {
                    this->type = greaterOrEqualNode; break;
                } else {
                    this->type = greaterNode; break;
                }
            case '<':
                if (op.size() > 1 && op[1] == '=') {
                    this->type = lesserOrEqualNode; break;
                } else {
                    this->type = lesserNode; break;
                }
            case '=': if (op.size() > 1 && op[1] == '=') {this->type = equalNode; break;}
            case '!': if (op.size() > 1 && op[1] == '=') {this->type = nonEqualNode; break;}
            case '&':
                if (op.size() > 1 && op[1] == '&') {
                    this->type = andNode;
                }
                break;
            case '|':
                if (op.size() > 1 && op[1] == '|') {
                    this->type = andNode;
                }
                break;
            default: std::cout << "INTERNAL ERROR: unknown operation in Expression constructor at line " << ASTexpr->line_ << ": " << op[0] << std::endl;
                this->type = noneNode;
        }

        this->leftExpr = new Expression(lunaBinOp->left_, nameTable, errorReports, currentVertex);
        this->rightExpr = new Expression(lunaBinOp->right_, nameTable, errorReports, currentVertex);
        return;
    }

    //todo ternary operator

    // name found
    id* df = dynamic_cast<id*>(ASTexpr);
    if (df != NULL){

        this->type = identifierNode;

        simple_id* simpleDF = dynamic_cast<simple_id*>(ASTexpr);
        // this is whatever name -- for iterator, while iterator, DF...
        // it is not necessarily a correct expression! it could be indexed "for" iterator, for example
        if (simpleDF != NULL){

            std::string simpleDFName = *(simpleDF->value_->value_);
            auto identifierName = nameTable.find(simpleDFName);
            if (identifierName != nameTable.end()){

                switch(identifierName->second->getType()){
                    case baseDFNameType: // simple DF (no indices)
                        this->identifier = new IndexedDFName(simpleDFName, identifierName->second, {}, errorReports, currentVertex);
                        this->identifier->setVertex(currentVertex);
                        break;
                    case subArgNameType: // argument of a sub
                    case forIteratorNameType:
                    case whileIteratorNameType:
                    case valueNameType:
                    case letNameType:
                    case mainArgNameType: // argument of a main
                        this->identifier = identifierName->second;
                        break;
                    default:
                        std::cout << "INTERNAL ERROR: UNKNOWN IDENTIFIER TYPE" << std::endl;
                        break;
                }
            } else {
                /*errorReports->push_back(JsonReporter::create14(
                    simpleDFName,
                    "[]",
                    "[]",
                    "[]"
                ));//todo callstacks*/
            }

            return;
        }

        complex_id* complexDF = dynamic_cast<complex_id*>(ASTexpr);
        if (complexDF != NULL){

            IndexedDFName* temp = parseIndexedDFExpression(complexDF, nameTable, ASTexpr->line_, errorReports, currentVertex);
            if (temp != nullptr){
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

Expression Expression::binOp(){
    Expression left = this->leftExpr->getAsConstant();
    Expression right = this->rightExpr->getAsConstant();

    if (left.type == realNode && right.type == realNode ||
        left.type == intNode && right.type == realNode || 
        left.type == realNode && right.type == intNode){ // result is double

        double l = std::stod(left.constant);
        double r = std::stod(right.constant);
        switch(this->type){
            case addNode: return Expression(std::to_string(l + r), realNode, nullptr);
            case subtractNode: return Expression(std::to_string(l - r), realNode, nullptr);
            case multiplyNode: return Expression(std::to_string(l * r), realNode, nullptr);
            case divideNode: return Expression(std::to_string(l / r), realNode, nullptr);
            case modulusNode: 
                std::cout << "INTERNAL ERROR: attempt to use modulus with double" << std::endl;
                return Expression("", noneNode, nullptr);
            case greaterNode: return Expression(std::to_string(l > r), intNode, nullptr);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode, nullptr);
            case lesserNode: return Expression(std::to_string(l < r), intNode, nullptr);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode, nullptr);
            case equalNode: return Expression(std::to_string(l == r), intNode, nullptr);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode, nullptr);
            case andNode: return Expression(std::to_string(l && r), intNode, nullptr);
            case orNode: return Expression(std::to_string(l || r), intNode, nullptr);

            default:
                std::cout << "INTERNAL ERROR: binOp reached default in switch" << std::endl;
                return Expression("", noneNode, nullptr);
        }
            
    } else if (left.type == intNode && left.type == intNode){ // result is int

        int l = std::stoi(left.constant);
        int r = std::stoi(right.constant);
        switch(this->type){
            case addNode: return Expression(std::to_string(l + r), intNode, nullptr);
            case subtractNode: return Expression(std::to_string(l - r), intNode, nullptr);
            case multiplyNode: return Expression(std::to_string(l * r), intNode, nullptr);
            case divideNode: return Expression(std::to_string(l / r), intNode, nullptr);
            case modulusNode: return Expression(std::to_string(l % r), intNode, nullptr);
            case greaterNode: return Expression(std::to_string(l > r), intNode, nullptr);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode, nullptr);
            case lesserNode: return Expression(std::to_string(l < r), intNode, nullptr);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode, nullptr);
            case equalNode: return Expression(std::to_string(l == r), intNode, nullptr);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode, nullptr);
            case andNode: return Expression(std::to_string(l && r), intNode, nullptr);
            case orNode: return Expression(std::to_string(l || r), intNode, nullptr);
            
            default:
                std::cout << "INTERNAL ERROR: binOp reached default in switch" << std::endl;
                return Expression("", noneNode, nullptr);
        }

    } else {
        std::cout << "INTERNAL ERROR: binOp calculation used with unsuitable type:" << std::endl;
        std::cout << "Left: " << left.type << "; right: " << right.type << std::endl;
        return Expression("", noneNode, nullptr);
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
Expression Expression::getAsConstant(){
    switch (this->type){

        // binary operations
        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:
            return this->binOp();
        
        // constants
        case intNode:
            return Expression(this->constant, intNode, nullptr);
        case stringNode:
            return Expression(this->constant, stringNode, nullptr);
        case realNode:
            return Expression(this->constant, realNode, nullptr);

        case identifierNode: {
            // in case of a let or sub, we can try to get value
            if (this->identifier != nullptr){
                LetName* letName = dynamic_cast<LetName*>(this->identifier);
                if (letName != nullptr){
                    return letName->getReference()->getAsConstant();
                }
                SubArgName* subArgName = dynamic_cast<SubArgName*>(this->identifier);
                if (subArgName != nullptr){
                    return subArgName->getReference()->getAsConstant();
                }
                return Expression("", noneNode, nullptr);//todo temporary
            } else {
                return Expression("", noneNode, nullptr);
            }
        }

        case intCastNode: {
            Expression insideExpression = this->leftExpr->getAsConstant();
            switch(insideExpression.type){
                case intNode: return insideExpression;
                case realNode: return Expression(std::to_string((int)std::stod(insideExpression.constant)), intNode, nullptr);
                default: return Expression("", noneNode, nullptr);
            }
        }

        case realCastNode: {
            Expression insideExpression = this->leftExpr->getAsConstant();
            switch(insideExpression.type){
                case intNode: return Expression(std::to_string((double)std::stoi(insideExpression.constant)), realNode, nullptr);
                case realNode: return insideExpression;
                default: return Expression("", noneNode, nullptr);
            }
        }

        // expression unsuitable for being a constant
        default:
            std::cout << "INTERNAL ERROR: getAsConstant returned noneNode (default)" << std::endl;
            return Expression("", noneNode, nullptr);
    }
}

std::vector<std::string> Expression::markAsUse(Vertex* currentVertex, int size){
    std::vector<std::string> reports = {};
    std::cout << "Expression " << this->getExpr()->to_string() << " is being marked as used" << std::endl;
    switch(type){

        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:
            if (leftExpr != nullptr)
                for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            if (rightExpr != nullptr)
                for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case identifierNode:
            if (identifier != nullptr)
                for (auto r: identifier->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        case stringNode:
        case intNode:
        case realNode: 
            return reports;

        case realCastNode: 
        case intCastNode:
        case stringCastNode:
            if (leftExpr != nullptr)
                for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            return reports;
        
        // only noneNode must be left
        default: std::cout << "INTERNAL ERROR: Expression.markAsUse ended as \"default\"" << std::endl;

    }

    return reports;
}

std::vector<std::string> Expression::markAsDef(Vertex* currentVertex, int size){
    std::vector<std::string> reports = {};
    std::cout << "Expression " << this->getExpr()->to_string() << " is being marked as defined" << std::endl;
    switch(type){
        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:{
            reports.push_back(JsonReporter::create26(
                this->getExpr()->to_string(),
                currentVertex
            ));
            return reports;
        }
        
        case identifierNode:

            if (identifier != nullptr)
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
                case forIteratorNameType: {//error
                    reports.push_back(JsonReporter::create26(
                        this->getExpr()->to_string(),
                        currentVertex
                    ));
                    return reports;
                }
                
                case whileIteratorNameType:{ //error
                    reports.push_back(JsonReporter::create26(
                        this->getExpr()->to_string(),
                        currentVertex
                    ));
                    return reports;
                }
                
                case valueNameType://todo
                    return reports;
                
                case letNameType: {//ok
                    LetName* letName = dynamic_cast<LetName*>(identifier);
                    for (auto r: letName->getReference()->markAsDef(currentVertex, size)) reports.push_back(r);
                    return reports;
                }
                case mainArgNameType: {
                    reports.push_back(JsonReporter::create26(
                        this->getExpr()->to_string(),
                        currentVertex
                    ));
                    return reports;
                }
                
                default:
                    std::cout << "INTERNAL ERROR (WARNING): Expression.markAsDef() ended by default at identifier node" << std::endl;
                    return reports;
                
            }
        
        case stringNode:
        case intNode:
        case realNode:
        case intCastNode:
        case realCastNode:
        case stringCastNode:
            reports.push_back(JsonReporter::create26(
                this->getExpr()->to_string(),
                currentVertex
            ));
            return reports;

        // only noneNode must be left
        default: 
            std::cout << "WARNING: Expression.markAsDef ended as \"default\"" << std::endl;
            return reports;
    }

    return reports;
}

bool Expression::isIndexable(){
    // only identifiers are indexable
    if (type == identifierNode){
        return identifier->isIndexable();
    } else {
        return false;
    }
}
