import {ExecStatement, TExecStatement} from './exec-statement';
import {TCondNode, DfCond, TDfCond, ExecNode} from '../../luna-ast';
import {getSub, TMetaInfo, updateNodeCreator} from '../../meta-info';
import {BodyStatement} from '../body-statement';
import {LunaDf, getNestedDfs, TLunaDf} from '../../df/luna-df';
import {
    addAlias, addConstant, addEntryDfs, TContextInfo, EmptyContextInfo, getCanonicalLunaDf
} from '../../df/context-info';
import {parseCondNode} from '../../condition';
import {UseNodeState, TNodeCreator, updateDfPull} from '../../promela-node/node-creator';
import {reduce} from '../../utils';
import {getFromLunaDf} from '../../df/df-pull';
import {Context, Statement, TContext} from "../statement";

export const ExecSubStatement = (execNode: ExecNode, context: TContext): TExecStatement => {
    const sub = getSub(context.metaInfo)(execNode.code);
    const [newNodeCreator, newContextInfo] =
        execNode.args.reduce(([previousNodeCreator, previousContextInfo]: [TNodeCreator, TContextInfo], arg: TCondNode, index: number) => {
            return arg.type === 'id'
                ? handleRefArg([previousNodeCreator, previousContextInfo])(arg)(sub.get(index))
                : handleExpressionArg([previousNodeCreator, previousContextInfo])(arg)(sub.get(index))(sub.begin);
        }, [context.metaInfo.nodeCreator, context.childInfo]);
    const bodyStatement = BodyStatement(sub.bodyNode, Context(newContextInfo, updateNodeCreator(context.metaInfo)(newNodeCreator)));
    const contextInfo = {
        ...EmptyContextInfo(),
        dfRegistry: bodyStatement.context.childInfo.dfRegistry
    };
    return ExecStatement(
        Statement(
            Context(contextInfo, bodyStatement.context.metaInfo),
            bodyStatement.promelaNodes),
        execNode
    );
};

const handleRefArg = ([previousNodeCreator, previousContextInfo]: [TNodeCreator, TContextInfo]) =>
                     (arg: TDfCond) =>
                     (argName: string) => {
    const lunaDf = getCanonicalLunaDf(previousContextInfo)(LunaDf(arg));
    const [generatedPromelaDf, updatedDfPull] = getFromLunaDf(previousContextInfo)(lunaDf)(previousNodeCreator.dependsGraph.dfPull);
    const updatedNodeCreator = updateDfPull(previousNodeCreator)(updatedDfPull);
    const updatedContextInfo =
        addEntryDfs(
            addAlias(previousContextInfo)(argName)(lunaDf)
        )([generatedPromelaDf]);
    return handleNestedDfs(getNestedDfs(lunaDf))(updatedNodeCreator)(updatedContextInfo);
};

const handleExpressionArg = ([previousNodeCreator, previousContextInfo]: [TNodeCreator, TContextInfo]) =>
                            (arg: TCondNode) =>
                            (argName: string) =>
                            (begin: number) => {
    const condition = parseCondNode(arg);
    const updatedContextInfo = addConstant(previousContextInfo)(LunaDf(DfCond([argName], begin)))(condition);
    return handleNestedDfs(condition.lunaDfs)(previousNodeCreator)(updatedContextInfo);
};

const handleNestedDfs = (lunaDfs: readonly TLunaDf[]) =>
                        (nodeCreator: TNodeCreator) =>
                        (contextInfo: TContextInfo): [TNodeCreator, TContextInfo] =>
    reduce(lunaDfs)
          ([nodeCreator, contextInfo])
          (([previousNodeCreator, previousContextInfo]: [TNodeCreator, TContextInfo]) => (df) => {
            const [useNode, updatedNodeCreator] = UseNodeState(df, previousContextInfo, df.begin)(previousNodeCreator);
            return [updatedNodeCreator, addEntryDfs(previousContextInfo)(useNode.promelaDfs)] as [TNodeCreator, TContextInfo];
          }) as [TNodeCreator, TContextInfo];
