import {parseStatement, Statement, TStatement, TContext, Context} from './statement';
import {BodyNode, DfCond, getNames, StatementNode} from '../luna-ast';
import {TMetaInfo, updateNodeCreator} from '../meta-info';
import {LunaDf} from '../df/luna-df';
import {getPromelaDfFullName, isDeclared} from '../df/promela-df';
import {addEntryDfs, TContextInfo, updateRegistry} from '../df/context-info';
import {TProctypeBodyNode} from '../promela-node/proctype-body-node/proctype-body-node';
import {DefNodeState, EndDefNodeState, processNodes} from '../promela-node/node-creator';
import {filter, flatMapArray, mapArray, reduce} from '../utils';

export const BodyStatement = (bodyNode: BodyNode, context: TContext): TStatement => {
    const [dfNode, ...statements] = bodyNode;
    const dfDeclaredStatement = processNodes({begin: -1})(DefNodeState)(getNames(dfNode))(context);
    const statement = handleStatements(statements)(dfDeclaredStatement.context);
    const declaredDfNames: string[] = [...new Set(
        flatMapArray((node: TProctypeBodyNode) =>
            mapArray(getPromelaDfFullName)(
                filter(
                    isDeclared(dfNode)
                )(node.promelaDfs)
            )
        )([...dfDeclaredStatement.promelaNodes, ...statement.promelaNodes])
    )];
    const information = handleDeclaredDfNames(declaredDfNames)(statement)(dfNode.begin);
    const promelaNodes = [...information.allDefNodes, ...statement.promelaNodes, ...information.endDefNodes];
    return Statement(information.context, promelaNodes);
};

type TNodesInformation = {
    allDefNodes: readonly TProctypeBodyNode[],
    endDefNodes: readonly TProctypeBodyNode[],
    context: TContext
};

const NodesInformation = (allDefNodes: readonly TProctypeBodyNode[],
                          endDefNodes: readonly TProctypeBodyNode[],
                          context: TContext): TNodesInformation =>
    ({ allDefNodes, endDefNodes, context });

const handleStatements = (statements: StatementNode[]) =>
                         (context: TContext): TStatement =>
    reduce(statements)
          (Statement(context, []))
          ((previousValue: TStatement) => (statementNode) => {
              const statement = parseStatement(statementNode)(previousValue.context);
              const contextInfo = updateRegistry(previousValue.context.childInfo)(statement.context.childInfo);
              const context2 = Context(contextInfo, statement.context.metaInfo);
              return Statement(context2, [...previousValue.promelaNodes, ...statement.promelaNodes]);
          });

const handleDeclaredDfNames = (declaredDfNames: string[]) =>
                              (statement: TStatement) =>
                              (begin: number): TNodesInformation =>
    reduce(declaredDfNames)
          (NodesInformation([], [], statement.context))
          (( previousValue: TNodesInformation) => (dfName) => {
              const lunaDf = LunaDf(DfCond([dfName], begin));
              const [defNode, nodeCreator1] = DefNodeState(lunaDf, previousValue.context.childInfo, begin)(previousValue.context.metaInfo.nodeCreator);
              const [endDefNode, nodeCreator2] = EndDefNodeState(lunaDf, previousValue.context.childInfo, begin)(nodeCreator1);
              const context = Context(
                  addEntryDfs(previousValue.context.childInfo)(defNode.promelaDfs),
                  updateNodeCreator(statement.context.metaInfo)(nodeCreator2))
              return NodesInformation([...previousValue.allDefNodes, defNode], [...previousValue.endDefNodes, endDefNode], context);
          });
