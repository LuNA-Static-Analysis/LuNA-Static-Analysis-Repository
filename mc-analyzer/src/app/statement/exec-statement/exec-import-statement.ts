import {ArgType, TCondNode, DfCond, ExecNode, RuleNode} from '../../luna-ast';
import {getImport, TMetaInfo} from '../../meta-info';
import {TCondition, getBaseLunaDf, parseCondNode} from '../../condition';
import {ExecStatement, TExecStatement, getArgs} from './exec-statement';
import {LunaDf, getNestedDfs, TLunaDf} from '../../df/luna-df';
import {TContextInfo, getCanonicalLunaDf, isNotConstant} from '../../df/context-info';
import {filter, flatMapArray, indexedFilter, mapArray, reduce} from '../../utils';
import {
    DependsNodeState, DestroyNodeState, InitNodeState, UseNodeState, processNodes
} from '../../promela-node/node-creator';
import {pipe} from "fp-ts/function";
import {Context, Statement, TContext} from "../statement";

export const ExecImportStatement = (execNode: ExecNode, context: TContext): TExecStatement => {
    const args = getArgs(execNode);
    const importArgTypes = getImport(context.metaInfo)(execNode.code).args;
    const dfInits = getDfInits(execNode.args)(importArgTypes)(args)(context.childInfo);
    const dfUses = getDfUses(importArgTypes)(dfInits)(args)(context.childInfo);
    const steps = [
        { action: InitNodeState, param: dfInits },
        { action: DependsNodeState, param: getDefDependencies(dfUses)(dfInits) },
        { action: UseNodeState, param: dfUses },
        { action: DestroyNodeState, param: getDfDestroys(execNode.rules) }
    ];
    const newStatement =
        reduce(steps)
              (Statement(context, []))
              ((prev) => (step) => {
                  const processedStatement = processNodes(execNode)(step.action)(step.param)(prev.context);
                  return Statement(
                      processedStatement.context,
                      [...prev.promelaNodes, ...processedStatement.promelaNodes]
                  );
              });
    return ExecStatement(newStatement, execNode);
};

const getDfInits = (statementArgs: readonly TCondNode[]) =>
                   (importArgs: readonly ArgType[]) =>
                   (args: readonly TCondition[]) =>
                   (childInfo: TContextInfo): readonly TLunaDf[] =>
    pipe(
        indexedFilter<TCondition>((i: number) => isInit(statementArgs[i])(importArgs[i])(childInfo))(args),
        flatMapArray((arg: TCondition) => arg.lunaDfs),
        mapArray(getCanonicalLunaDf(childInfo))
    );

const getDfUses = (importArgs: readonly ArgType[]) =>
                  (dfsInit: readonly TLunaDf[]) =>
                  (args: readonly TCondition[]) =>
                  (childInfo: TContextInfo): readonly TLunaDf[] => {
        const usesArgs = indexedFilter<TCondition>((i: number) => isUse(importArgs[i]))(args);
        const dfUses = [
            ...flatMapArray((arg: TCondition) => filter(isNotConstant(childInfo))(arg.lunaDfs))(usesArgs),
            ...flatMapArray(getNestedDfs)(dfsInit)
        ];
        return mapArray(getCanonicalLunaDf(childInfo))(dfUses);
    };

const isInit = (statementArg: TCondNode) =>
               (importArg: ArgType) =>
               (childInfo: TContextInfo): boolean =>
    statementArg.type === 'id' &&
    importArg === 'name' &&
    isNotConstant(childInfo)(LunaDf(statementArg));

const isUse = (importArg: ArgType): boolean =>
    importArg !== 'name';

const isDestroy = (rule: RuleNode) =>
    rule.ruletype === 'enum' &&
    rule.type === 'rule' &&
    rule.property === 'delete';

const getDfDestroys = (rules: readonly RuleNode[]): readonly TLunaDf[] =>
    flatMapArray(ruleToLunaDfs)(
        filter(isDestroy)(rules)
    );

const getDefDependencies = (dfUses: readonly TLunaDf[]) =>
                           (dfInits: readonly TLunaDf[]) =>
    flatMapArray((dfInit: TLunaDf) =>
        mapArray(
            (dfUse: TLunaDf): [TLunaDf, TLunaDf] => [dfInit, dfUse]
        )(dfUses)
    )(dfInits);

const toCondition = (rule: RuleNode) =>
                    (item: [string, ...TCondNode[]]) =>
    parseCondNode(DfCond(item, rule.begin));

const ruleToLunaDfs = (rule: RuleNode) =>
    mapArray(getBaseLunaDf)(
        mapArray(toCondition(rule))(rule.items)
    );
