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

Expression::Expression(std::string constant, ExpressionType type){
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
Expression::Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, std::vector<std::string>* errorReports, Vertex* currentVertex){

    this->ASTexpr = ASTexpr;

    // default initialization
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->type = noneNode;

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
                        this->identifier = new IndexedDFName(simpleDFName, identifierName->second, {}, errorReports);
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
                std::string report = "ERROR: no name \"" + simpleDFName + "\" found!" + "\n";
                errorReports->push_back(report);
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
            case addNode: return Expression(std::to_string(l + r), realNode);
            case subtractNode: return Expression(std::to_string(l - r), realNode);
            case multiplyNode: return Expression(std::to_string(l * r), realNode);
            case divideNode: return Expression(std::to_string(l / r), realNode);
            case modulusNode: 
                std::cout << "INTERNAL ERROR: attempt to use modulus with double" << std::endl;
                return Expression("", noneNode);
            case greaterNode: return Expression(std::to_string(l > r), intNode);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode);
            case lesserNode: return Expression(std::to_string(l < r), intNode);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode);
            case equalNode: return Expression(std::to_string(l == r), intNode);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode);
            case andNode: return Expression(std::to_string(l && r), intNode);
            case orNode: return Expression(std::to_string(l || r), intNode);

            default:
                std::cout << "INTERNAL ERROR: binOp reached default in switch" << std::endl;
                return Expression("", noneNode);
        }
            
    } else if (left.type == intNode && left.type == intNode){ // result is int

        int l = std::stoi(left.constant);
        int r = std::stoi(right.constant);
        switch(this->type){
            case addNode: return Expression(std::to_string(l + r), intNode);
            case subtractNode: return Expression(std::to_string(l - r), intNode);
            case multiplyNode: return Expression(std::to_string(l * r), intNode);
            case divideNode: return Expression(std::to_string(l / r), intNode);
            case modulusNode: return Expression(std::to_string(l % r), intNode);
            case greaterNode: return Expression(std::to_string(l > r), intNode);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode);
            case lesserNode: return Expression(std::to_string(l < r), intNode);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode);
            case equalNode: return Expression(std::to_string(l == r), intNode);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode);
            case andNode: return Expression(std::to_string(l && r), intNode);
            case orNode: return Expression(std::to_string(l || r), intNode);
            
            default:
                std::cout << "INTERNAL ERROR: binOp reached default in switch" << std::endl;
                return Expression("", noneNode);
        }

    } else {
        std::cout << "INTERNAL ERROR: binOp calculation used with unsuitable type:" << std::endl;
        std::cout << "Left: " << left.type << "; right: " << right.type << std::endl;
        return Expression("", noneNode);
    }
}

Expression Expression::terOp(){ // strictly constants!
    Expression condition = this->ternaryOperatorCondition->getAsConstant();
    if (condition.type == realNode){
        if (std::stod(condition.constant) != 0){
            return this->leftExpr->getAsConstant();
        } else {
            return this->rightExpr->getAsConstant();
        }
    } else if (condition.type == intNode) {
        if (std::stoi(condition.constant) != 0){
            return this->leftExpr->getAsConstant();
        } else {
            return this->rightExpr->getAsConstant();
        }
    } else {
        std::cout << "INTERNAL ERROR: unsuitable type inside ternary operator constant calculation" << std::endl;
    }

    return Expression("", noneNode);
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

        // ternary operations
        case ternaryNode:
            return this->terOp();
        
        // constants
        case intNode:
            return Expression(this->constant, intNode);
        case stringNode:
            return Expression(this->constant, stringNode);
        case realNode:
            return Expression(this->constant, realNode);

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
                return Expression("", noneNode);//todo temporary
            } else {
                return Expression("", noneNode);
            }
        }

        case intCastNode: {
            Expression insideExpression = this->leftExpr->getAsConstant();
            switch(insideExpression.type){
                case intNode: return insideExpression;
                case realNode: return Expression(std::to_string((int)std::stod(insideExpression.constant)), intNode);
                default: return Expression("", noneNode);
            }
        }

        case realCastNode: {
            Expression insideExpression = this->leftExpr->getAsConstant();
            switch(insideExpression.type){
                case intNode: return Expression(std::to_string((double)std::stoi(insideExpression.constant)), realNode);
                case realNode: return insideExpression;
                default: return Expression("", noneNode);
            }
        }

        // expression unsuitable for being a constant
        default:
            std::cout << "INTERNAL ERROR: getAsConstant returned noneNode (default)" << std::endl;
            return Expression("", noneNode);
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
        
        case ternaryNode:
            //todo marks must depend on outcome of an ternary operator -- implement constant check at least
            if (leftExpr != nullptr)
                for (auto r: leftExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            if (rightExpr != nullptr)
                for (auto r: rightExpr->markAsUse(currentVertex, size)) reports.push_back(r);
            if (ternaryOperatorCondition != nullptr)
                for (auto r: ternaryOperatorCondition->markAsUse(currentVertex, size)) reports.push_back(r);
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
        case modulusNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (%) operation\n");
            return reports;
        case greaterNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (>) operation\n");
            return reports;
        case greaterOrEqualNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (>=) operation\n");
            return reports;
        case lesserNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (<) operation\n");
            return reports;
        case lesserOrEqualNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (<=) operation\n");
            return reports;
        case equalNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (==) operation\n");
            return reports;
        case nonEqualNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (!=) operation\n");
            return reports;
        case andNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (&&) operation\n");
            return reports;
        case orNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark binary (||) operation\n");
            return reports;
        
        case ternaryNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark ternary operator\n");
            return reports;
        
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
        
        case intCastNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA int cast\n");
            return reports;
        
        case realCastNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA real cast\n");
            return reports;

        case stringCastNode:
            reports.push_back("ERROR: initializing unsuitable expression -- Expression.markAsDef used to mark LuNA string cast\n");
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
