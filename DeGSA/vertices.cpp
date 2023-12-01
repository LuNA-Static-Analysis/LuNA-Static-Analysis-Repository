#include <map>
#include <set>
#include "../parser/ast.hpp"

enum VertexType {

    forVF = 1,
    ifVF = 2,
    whileVF = 3,
    letVF = 4,
    importVF = 5,
    subVF = 6

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};

// this class represents a vertex in a DDG
// vertex is a computational fragment (VF), so it could require some (or none) VFs to be ran before, and also allow other VFs to run
// also vertex can contain VFs inside, if its operator allows to have blocks (i.e subprogram, but not an import)
class Vertex {

    // this struct serves as a binding between vertices of the same level
    // it has a pointer to a vertice and a name of a DF used or defined
    struct Binding {

    private:

        std::string name;//todo use set or vector here?
        Vertex* pointerTo;

    public:

        Binding(Vertex* pointerTo, std::string name){
            this->name = name;
            this->pointerTo = pointerTo;
        }

        std::string getName(){
            return this->name;
        }

        Vertex* getPointerTo(){
            return this->pointerTo;
        }

        bool operator<(const Binding b) const {//TODO optimize + check (this is utterly retarded)
            if ((long)(this->pointerTo) < (long)b.pointerTo){
                return true;
            } else if (this->name.compare(b.name) > 0){
                return true;
            } else {
                return false;
            }
        }
    };

    protected:

        std::set<Binding> in; // vertices that must be ran directly before current
        std::set<Binding> out; // vertices that require directly current vertex to be ran
        std::set<Vertex*> inside; // vertices that are inside the body of a current vertex

        /* DFs that are visible inside block of this vertice, but not including ones that are declared in this block
            has duplicates*/
        std::vector<std::string> declaredOutsideDFsVector;
        /* DFs that are declared in current block
            has duplicates */
        std::vector<std::string> declaredInsideDFsVector;
        /* concatenation of previous two containers
            has duplicates */
        std::vector<std::string> declaredBothDFsVector;
        /* set of previous container
            has no duplicates */
        std::set<std::string> declaredBothDFsSet;

        std::set<std::string> use; // list of DFs that are used in this vertex
        std::set<std::string> def; // list of DFs that are defined in this vertex

        VertexType vertexType; // type of a vertex (VF type)
        int depth; // amount of blocks that this vertex is in
        int number; // unique number of a vertice
        int line; // line in code that this operator is in

    public:
        Vertex(){};

        //virtual ~Vertex();

        VertexType getVertexType(){
            return vertexType;
        }

        std::set<std::string> getUseSet(){
            return use;
        }

        std::set<std::string> getDefSet(){
            return def;
        }

        std::set<Vertex*> getInsideSet(){
            return inside;
        }

        std::set<Binding> getInSet(){
            return in;
        }

        std::set<Binding> getOutSet(){
            return out;
        }

        int getDepth(){
            return depth;
        }

        int getNumber(){
            return number;
        }

        int getLine(){
            return line;
        }

        void addIn(Vertex* vertex, std::string dfName){
            this->in.insert(Binding(vertex, dfName)); //TODO this should not allow duplicates; does it allow it here?
        }

        void addOut(Vertex* vertex, std::string dfName){
            this->out.insert(Binding(vertex, dfName)); //TODO this should not allow duplicates; does it allow it here?
        }

        void addInside(Vertex* vertex){
            auto temp = this->inside.find(vertex);
            if (temp == this->inside.end()){
                this->inside.insert(vertex);
            }
        }

        void addUse(std::string name){
            auto temp = this->use.find(name);
            if (temp == this->use.end()){
                this->use.insert(name);
            }
        }

        void addDef(std::string name){
            auto temp = this->def.find(name);
            if (temp == this->def.end()){
                this->def.insert(name);
            }
        }

        std::vector<std::string> getDeclaredInsideDFsVector(){
            return declaredInsideDFsVector;
        }

        std::vector<std::string> getDeclaredOutsideDFsVector(){
            return declaredOutsideDFsVector;
        }

        std::vector<std::string> getDeclaredBothDFsVector(){
            return declaredBothDFsVector;
        }

        std::set<std::string> getDeclaredBothDFsSet(){
            return declaredBothDFsSet;
        }

        void setDeclaredInsideDFsVector(std::vector<std::string> declaredInsideDFsVector){
            this->declaredInsideDFsVector = declaredInsideDFsVector;
        }

        void setDeclaredOutsideDFsVector(std::vector<std::string> declaredOutsideDFsVector){
            this->declaredOutsideDFsVector = declaredOutsideDFsVector;
        }

        void setDeclaredBothDFsVector(std::vector<std::string> declaredBothDFsVector){
            this->declaredBothDFsVector = declaredBothDFsVector;
        }

        void setDeclaredBothDFsSet(std::set<std::string> declaredBothDFsSet){
            this->declaredBothDFsSet = declaredBothDFsSet;
        }

        virtual void printInfo(){
            std::cout << "printInfo() placeholder" << std::endl;
        }
        
};

class CFVertex: public Vertex {

    private:

        std::string name; // name of an import/sub

    public:

        //todo copy constructor

        //todo check if this works
        CFVertex(int depth, int number, int line,
            std::string name, VertexType vertexType){

            this->depth = depth;
            this->number = number;
            this->line = line;

            this->name = name;
            this->vertexType = vertexType; // import or sub
        }

        std::string getName(){
            return name;
        }

        void printInfo() {

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
            for (std::string i: this->getDeclaredOutsideDFsVector()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Declared inside DFs:";
            for (std::string i: this->getDeclaredInsideDFsVector()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Use DFs:";
            for (std::string i: this->getUseSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Def DFs:";
            for (std::string i: this->getDefSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices inside:";
            for (auto i: this->getInsideSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices before (\"in\"):";
            for (auto i: this->getInSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

            std::cout << "Vertices after (\"out\"):";
            for (auto i: this->getOutSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

        }

};

class ForVertex: public Vertex {

    private:

        std::string iteratorName;
        std::string leftBorder; // either DF, variable or a constant; just try to parse as int to find out
        std::string rightBorder; // either DF, variable or a constant; just try to parse as int to find out

    public:

        //todo copy constructor

        //todo check if this works
        ForVertex(int depth, int number, int line,
            std::string iteratorName, std::string leftBorder, std::string rightBorder){

            this->vertexType = forVF;

            this->depth = depth;
            this->number = number;
            this->line = line;

            this->iteratorName = iteratorName;
            this->leftBorder = leftBorder;
            this->rightBorder = rightBorder;
        }

        std::string getIteratorName(){
            return iteratorName;
        }

        std::string getLeftBorder(){
            return leftBorder;
        }

        std::string getRightBorder(){
            return rightBorder;
        }

        void printInfo(){

            std::cout << "Vertex number: " << this->getNumber() << std::endl;
            std::cout << "Vertex address: " << this << std::endl;
            std::cout << "Vertex type: ";
            std::cout << this->getVertexType() << " " << "(for)" << std::endl;
            std::cout << "Vertex line: " << this->getLine() << std::endl;
            std::cout << "Vertex depth: " << this->getDepth() << std::endl;

            std::cout << "Iterator name: " + this->getIteratorName() << std::endl;
            std::cout << "Left border: " + this->getLeftBorder() << std::endl;
            std::cout << "Right border: " + this->getRightBorder() << std::endl;

            std::cout << "Declared outside DFs:";
            for (std::string i: this->getDeclaredOutsideDFsVector()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Declared inside DFs:";
            for (std::string i: this->getDeclaredInsideDFsVector()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Use DFs:";
            for (std::string i: this->getUseSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Def DFs:";
            for (std::string i: this->getDefSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices inside:";
            for (auto i: this->getInsideSet()){
                std::cout << " " << i;
            }
            std::cout << std::endl;

            std::cout << "Vertices before (\"in\"):";
            for (auto i: this->getInSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

            std::cout << "Vertices after (\"out\"):";
            for (auto i: this->getOutSet()){
                std::cout << " " << i.getPointerTo() << " [" << i.getPointerTo()->getNumber() << "] (" << i.getName() << ")";
            }
            std::cout << std::endl;

        }

};