import {BiOperatorCond, BranchableNode, DfCond, ForNode} from '../../luna-ast';
import {TMetaInfo, updateNodeCreator} from '../../meta-info';
import {TCondition, parseCondNode} from '../../condition';
import {addEntryDfs, isConstant, isNotConstant, TContextInfo, updateRegistry} from '../../df/context-info';
import {Context, Statement, TContext, TStatement} from '../statement';
import {BodyStatement} from '../body-statement';
import {LunaDf} from '../../df/luna-df';
import {
    DefNodeState, EndDefNodeState, FormulaStatement, getFormulaName, IfNode, InitNodeState, processNodes, UseNodeState
} from '../../promela-node/node-creator';
import {getStatementByBranchCondition, _handleIDK} from './branchable-statement';

export const ForStatement = (forNode: ForNode, context: TContext): TStatement => {
    const getCondition = (node: ForNode): TCondition => parseCondNode(BiOperatorCond('<=', [node.first, node.last]));
    const condition: TCondition = getCondition(forNode);
    return getStatementByBranchCondition(condition.branchCondition)
                                        (() => Statement(context, []))
                                        (() => handleTRUE(context)(forNode))
                                        (() => _handleIDK(context)(forNode)(getCondition));
};

const handleTRUE = (context: TContext) =>
                   (node: ForNode): TStatement => {
    const lunaDf = LunaDf(DfCond([node.var], node.begin));
    const iteratorStatement = handleIterator(node)(context);
    const bodyStatement = handleBody(node)(iteratorStatement.context);
    const [endDefNode, nodeCreator] = EndDefNodeState(lunaDf, bodyStatement.context.childInfo, node.begin)(bodyStatement.context.metaInfo.nodeCreator);
    const metaInfo = updateNodeCreator(bodyStatement.context.metaInfo)(nodeCreator);
    const contextInfo = addEntryDfs(bodyStatement.context.childInfo)(endDefNode.promelaDfs);
    const promelaNodes = [
        ...iteratorStatement.promelaNodes,
        ...bodyStatement.promelaNodes,
        endDefNode
    ];
    return Statement(
        Context(contextInfo, metaInfo),
        promelaNodes
    );
};

const handleIterator = (node: ForNode) =>
                       (context: TContext): TStatement => {
    const childInfo = context.childInfo;
    const metaInfo = context.metaInfo;
    const begin = node.begin;
    const lunaDf = LunaDf(DfCond([node.var], begin));
    const [defNode, nodeCreator1] = DefNodeState(lunaDf, childInfo, begin)(metaInfo.nodeCreator);
    const contextInfo1 = addEntryDfs(childInfo)(defNode.promelaDfs);
    const [initNode, nodeCreator2] = InitNodeState(lunaDf, contextInfo1, begin)(nodeCreator1);
    const iterValue = parseCondNode(node.first).value;
    return Statement(
        Context(
            addEntryDfs(
                childInfo
                // addAlias(childInfo)(lunaDf.fullName)({lunaName: `mmm`, indexes: [], fullName: `${iterValue}`, begin})
            )(initNode.promelaDfs),
            updateNodeCreator(metaInfo)(nodeCreator2)
        ),
        [defNode, initNode]
    )
};

const handleBody = (node: ForNode) =>
                   (context: TContext): TStatement => {
    return t1(node)(context);
    // return (parseCondNode(node.last).value > parseCondNode(node.first).value)
    //     ? t1(node)(metaInfo)(contextInfo)
    //     : (t1(node)(t1(node)(metaInfo)[1])(t1(node)(metaInfo)[2]));
};

const t1 = (node: ForNode) =>
           (context: TContext): TStatement => {
    const statement = BodyStatement(node.body, context);
    const childInfo = updateRegistry(context.childInfo)(statement.context.childInfo);
    return Statement(
        Context(childInfo, statement.context.metaInfo),
        statement.promelaNodes
    );
};
