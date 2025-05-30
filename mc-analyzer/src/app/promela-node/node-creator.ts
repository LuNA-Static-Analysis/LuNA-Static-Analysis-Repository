import { EmptyLtl, LtlSem4, LtlSem3_2, LtlSem2_1, LtlSem3_1, LtlSem3_6, equalsLtl, getLtlStatement, TLtl } from '../ltl/ltl';
import {DfPull, TDfPull, getAll, getFromLunaDf} from '../df/df-pull';
import {TLunaDf} from '../df/luna-df';
import {addEntryDfs, TContextInfo} from '../df/context-info';
import {ProctypeNode, getTable, TProctypeNode} from './promela-ast-node/proctype-node';
import {PositionTable, PositionTableNode, getEntries, TPositionTableNode} from './position-table';
import {createInitDependsNode, PromelaAstNode, getNodeContent, TPromelaASTNode} from './promela-ast-node/promela-ast-node';
import {addDependency, DependsGraph, TDependsGraph, findAllCycles} from '../depends-graph';
import {addIndent, combine, flatMapArray, get, indexedFlatMap, joinNewLine, mapArray, reduce, sort, State} from '../utils';
import {BeginContained} from '../luna-ast';
import {updateNodeCreator} from '../meta-info';
import {
    ProctypeBodyNode, DependsNode, equalsDependsNodeContent, extractPromelaDfsFromNode, getNodeBegins,
    TProctypeBodyNode
} from './proctype-body-node/proctype-body-node';
import {PromelaDf, TPromelaDf} from '../df/promela-df';
import {Context, TStatement, TContext, Statement} from '../statement/statement';
import { flatMap, map, modify, of, put } from 'fp-ts/lib/State';
import { pipe } from 'fp-ts/lib/function';
import {getCond} from '../statement/branchable-statement/branchable-statement';

export type TNodeCreator = {
    readonly defines: readonly TPromelaASTNode[];
    readonly ltls: readonly TLtl[];
    readonly proctypeNodes: readonly TProctypeNode[];
    readonly initDependsNodes: readonly TPromelaASTNode[];
    readonly dependsGraph: TDependsGraph;
    readonly initializedOnes: Set<string>;
    readonly formulas: Map<string, string>;
    conds?: string[];
};

export type TFormulaStatement = {
    lunaFormula: string;
    prettyFormula: string;
    promelaFormula: string;
}

export const FormulaStatement = (lunaFormula: string, prettyFormula: string, promelaFormula: string) =>
    ({ lunaFormula, prettyFormula, promelaFormula });

export let formulaCounter = 0;

export const getFormulaName = () =>
    `cond${formulaCounter++}`;

export const NodeCreator = (subName: string): TNodeCreator => {
    return {
        defines: mapArray((define: string) => PromelaAstNode([], define))([
            '#define CONCAT3(a, b, c) a##b##c',
            '#define def(name) isdef_##name = true; true',
            '#define destroy(name) destroy_count_##name = destroy_count_##name + 1',
            '#define enddef(name) enddef_##name: isdef_##name = false;use_count_##name = 0;init_count_##name = 0;destroy_count_##name = 0;',
            '#define init(name) init_count_##name = init_count_##name + 1; init_count_##name = init_count_##name',
            '#define use(name) use_count_##name = use_count_##name + 1',
            '#define define_check_init(is_used, enddef_label, is_init) (enddef_label -> (is_used -> is_init))',
            '#define define_check_usage(is_used, enddef_label, is_init) (enddef_label -> (is_init -> is_used))',
            `#define check_init(name) define_check_init(use_count_##name > 0, ${subName}@enddef_##name, init_count_##name > 0)`,
            `#define check_usage(name) define_check_usage(use_count_##name > 0, ${subName}@enddef_##name, init_count_##name > 0)`,
            '#define depends_on(varA, varB) CONCAT3(depends_on_, varA, _##varB)=true;true;',
            '#define inc_true(name) true_count_##name = true_count_##name + 1; true_count_##name = true_count_##name',
            '#define inc_false(name) false_count_##name = false_count_##name + 1; true_count_##name = true_count_##name',
            '#define init_var(name) bool isdef_##name = false;' +
                                   'int name = 0;' +
                                   'int init_count_##name = 0;' +
                                   'int use_count_##name = 0;' +
                                   'int destroy_count_##name = 0;' +
                                   'int true_count_##name = 0;' +
                                   'int false_count_##name = 0'
            ]
        ),
        ltls: [],
        proctypeNodes: [],
        initDependsNodes: [],
        dependsGraph: DependsGraph(new Map(), DfPull(-1, new Map())),
        initializedOnes: new Set<string>(),
        formulas: new Map()
    };
};

export const getPositionTable = (nodeCreator: TNodeCreator) => {
    const shift: number = 1 +
        nodeCreator.defines.length +
        nodeCreator.ltls.length +
        nodeCreator.initDependsNodes.length +
        getAll(nodeCreator.dependsGraph.dfPull).length;
    return PositionTable(
        indexedFlatMap(mapNodeToTableNode(shift))(nodeCreator.proctypeNodes));
};

export const getPromelaCode = (nodeCreator: TNodeCreator): string => {
    const defineNodes = joinNewLine(mapArray(getNodeContent)(nodeCreator.defines));
    const ltlNodes = joinNewLine(sort(mapArray(getLtlStatement)(nodeCreator.ltls)));
    const proctypeNodes = joinNewLine(mapArray(getNodeContent)(nodeCreator.proctypeNodes));
    const initVarNodes = joinNewLine(sort([
        ...new Set([
            ...mapArray(getNodeContent)(createInitVarNodes(nodeCreator)),
            ...[
                ...nodeCreator.ltls
                    .map(ltl => ltl.statement.match(/^ltl SEM[56]_(cond\d+)/)?.[1])
                    .filter(Boolean),
                ...nodeCreator.conds
            ].map(n => `init_var(${n})`)
        ])
    ]));
    return `${defineNodes}\n${initVarNodes}\n${ltlNodes}\n${proctypeNodes}\n`;
};

export const addProctype = (nodeCreator: TNodeCreator) =>
                           (promelaNodes: readonly TProctypeBodyNode[]) =>
                           (subName: string): TNodeCreator => {
   const combines = generateBinaryCombinations(Array.from({ length: formulaCounter}).map((_, i) => `cond${i}`));
   const body = joinNewLine(mapArray(addIndent(1))(combines
       .map(obj => `:: ${
           Object.entries(obj)
               .map(([key, val]) => `${key}=${val}`)
               .join(';')}`
       )));
   const conds = Array.from({ length: formulaCounter}).map((_, i) => `cond${i}`);
   if (conds.length > 0) {
       const ifNode = {
           content: `if\n${body}\nfi`,
           promelaDfs: conds.map(c => PromelaDf(undefined, c)),
           begins: [-1, ...combines.map(_ => -1), -1, -1],
           ltls: []
       };
       return {
           ...nodeCreator,
           proctypeNodes: [...nodeCreator.proctypeNodes, ProctypeNode(subName, [ifNode, ...promelaNodes])],
           conds
       };
   }
   return {
       ...nodeCreator,
       proctypeNodes: [...nodeCreator.proctypeNodes, ProctypeNode(subName, promelaNodes)],
       conds: []
   };
}


export type NodeState = State<TNodeCreator, TProctypeBodyNode>;

const generateBinaryCombinations = (variables: any) => {
    const numCombinations = 1 << variables.length;
    const results = [];

    for (let i = 0; i < numCombinations; i++) {
        const combination = {};
        variables.forEach((name: any, index: any) => {
            const bit = (i >> (variables.length - index - 1)) & 1;
            combination[name] = bit;
        });
        results.push(combination);
        if (results.length > 2**10) {
            break;
        }
    }
    return results;
};


const createNodeState = (getDf: State<TDfPull, TPromelaDf>) =>
                        (createLtl: ((promelaDf: TPromelaDf) =>
                        (nodeCreator: TNodeCreator) => readonly TLtl[])) =>
                        (create: (ltls: readonly TLtl[]) => TProctypeBodyNode) =>
                        (updateNode: ((node: TProctypeBodyNode) => State<TNodeCreator, TProctypeBodyNode>)): NodeState =>
    pipe(
        get<TNodeCreator>(),
        flatMap((nodeCreator: TNodeCreator): State<TNodeCreator, readonly TLtl[]> => {
            const [promelaDf, dfPull] = getDf(nodeCreator.dependsGraph.dfPull);
            return pipe(
                put(updateDfPull(nodeCreator)(dfPull)),
                map(() => createLtl(promelaDf)(nodeCreator))
            );
        }),
        map(create),
        flatMap(updateNode)
    );

const ltlTable =
    {
        'init': (begin: number) => (promelaDf: TPromelaDf) =>
                (nodeCreator: TNodeCreator) =>
                    nodeCreator.initializedOnes.has(promelaDf.promelaName)
                        ? [LtlSem2_1(promelaDf), LtlSem4(promelaDf, begin)]
                        : [LtlSem4(promelaDf, begin)],
        'use': (begin: number) => (promelaDf: TPromelaDf) => () => [LtlSem3_1(promelaDf, begin)],
        'destroy': (promelaDf: TPromelaDf) => () => [LtlSem3_6(promelaDf)],
        'depends': ([promelaDfUse, promelaDfInit]: [TPromelaDf, TPromelaDf]) =>
                   (dependsGraph: TDependsGraph) => [
                            EmptyLtl([promelaDfUse, promelaDfInit]),
                            ...mapArray(LtlSem3_2)(findAllCycles(dependsGraph))
                        ],
        'def': ((promelaDf: TPromelaDf) => () => [EmptyLtl([promelaDf])]),
        'enddef': ((promelaDf: TPromelaDf) => () => [EmptyLtl([promelaDf])])
    };

export const InitNodeState = (lunaDf: TLunaDf, parentInfo: TContextInfo, begin: number): NodeState =>
    createNodeState(getFromLunaDf(parentInfo)(lunaDf))
                   (ltlTable.init(begin))
                   ((ltls: readonly TLtl[]) => ProctypeBodyNode(`init({0});`, [begin], ltls))
                   (updateInitNode);

export const UseNodeState = (lunaDf: TLunaDf, parentInfo: TContextInfo, begin: number): NodeState =>
     createNodeState(getFromLunaDf(parentInfo)(lunaDf))
                    (ltlTable.use(begin))
                    ((ltls: readonly TLtl[]) => ProctypeBodyNode(`use({0});`, [begin], ltls))
                    (updateUseNode);

export const DependsNodeState = ([dfUse, dfInit]: [TLunaDf, TLunaDf], parentInfo: TContextInfo, begin: number): NodeState =>
    pipe(
        get<TNodeCreator>(),
        flatMap((nodeCreator: TNodeCreator) => {
            const [promelaDfUse, dfPull1] = getFromLunaDf(parentInfo)(dfUse)(nodeCreator.dependsGraph.dfPull);
            const [promelaDfInit, dfPull2] = getFromLunaDf(parentInfo)(dfInit)(dfPull1);
            const dependsGraph = addDependency({...nodeCreator.dependsGraph, dfPull: dfPull2})(promelaDfUse)(promelaDfInit);
            const dependsNode = {
                ...ProctypeBodyNode(`depends_on({0}, {1});`, [begin],
                    ltlTable.depends([promelaDfUse, promelaDfInit])(dependsGraph)
                ),
                initDependsNodes: createInitDependsNode(promelaDfUse)(promelaDfInit)
            };
            const updatedNodeCreator = updateDependsNode(dependsNode)(
                updateDependsGraph(dependsGraph)(nodeCreator)
            );
            return pipe(
                put(updatedNodeCreator),
                map(() => dependsNode)
            );
        })
    );

export const IfNode = (nodeSequence: readonly TProctypeBodyNode[], contextInfo: TContextInfo, formula: TFormulaStatement): TProctypeBodyNode => {
    const cond = getCond(formula, contextInfo.constants);
    const begins = [-1, -1]
        .concat(flatMapArray(getNodeBegins)(nodeSequence))
        .concat([-1, -1]);
    console.error({nodeSequence})
    const [head, ...tail]: readonly string[] = ['', ...mapArray(getNodeContent)(nodeSequence)];
    const header = (cond === 'false')
        ? `if\n:: false ->`
        : `if\n:: ${cond === 'true' || formula.promelaFormula} -> inc_true(${formula.promelaFormula});`;
    const firstNode = addIndent(1)(head).trim();
    const otherNodes = joinNewLine(mapArray(addIndent(1))(tail));
    const allNodes = `${firstNode}\n${otherNodes}${otherNodes.length > 0 ? '\n' : ''}`;
    const footer = (cond === 'true') ? ':: else -> skip;\nfi' : `:: else -> inc_false(${formula.promelaFormula});\nfi`;
    const content = `${header}${allNodes}${footer}`;
    const ltl = EmptyLtl(
        flatMapArray(extractPromelaDfsFromNode)(nodeSequence)
    );
    return ProctypeBodyNode(content, begins, [ltl]);
};

export const DestroyNodeState = (lunaDf: TLunaDf, parentInfo: TContextInfo, begin: number): NodeState =>
    pipe(
        get<TNodeCreator>(),
        flatMap((nodeCreator: TNodeCreator) => {
            const [promelaDf, dfPull] = getFromLunaDf(parentInfo)(lunaDf)(nodeCreator.dependsGraph.dfPull);
            const destroyNode = ProctypeBodyNode(`destroy({0});`, [begin], ltlTable.destroy(promelaDf)());
            const updatedNodeCreator = updateDestroyNode(
                updateDependsGraph({...nodeCreator.dependsGraph, dfPull})(nodeCreator)
            )(destroyNode);
            return pipe(
                put(updatedNodeCreator),
                map(() => destroyNode)
            );
        })
    );


export const DefNodeState = (lunaDf: TLunaDf, parentInfo: TContextInfo, begin: number): NodeState =>
    createNodeState(getFromLunaDf(parentInfo)(lunaDf))
                   (ltlTable.def)
                   ((ltls: readonly TLtl[]) => ProctypeBodyNode(`def({0});`, [begin], ltls))
                   (of);

export const EndDefNodeState = (lunaDf: TLunaDf, parentInfo: TContextInfo, begin: number): NodeState =>
    createNodeState(getFromLunaDf(parentInfo)(lunaDf))
                   (ltlTable.enddef)
                   ((ltls: readonly TLtl[]) => ProctypeBodyNode(`enddef({0});`, [begin], ltls))
                   (of);

export type CreateNodeFunc = (lunaDf: TLunaDf | [TLunaDf, TLunaDf], parentInfo: TContextInfo, begin: number) => State<TNodeCreator, TProctypeBodyNode>;

export const processNodes = (beginNode: BeginContained) =>
                            (createNodeFunc: CreateNodeFunc) =>
                            (dfList: readonly TLunaDf[] | readonly [TLunaDf, TLunaDf][]) =>
                            (context: TContext): TStatement => {
    const result = reduce<TLunaDf | [TLunaDf, TLunaDf]>(dfList)
        ({childInfo: context.childInfo, nodeCreator: context.metaInfo.nodeCreator, promelaNodes: []})
        ((previousValue) => (curValue) => {
            const [node, nodeCreator] = createNodeFunc(curValue, context.childInfo, beginNode.begin)(previousValue.nodeCreator);
            return {
                nodeCreator,
                childInfo: addEntryDfs(previousValue.childInfo)(node.promelaDfs),
                promelaNodes: [...previousValue.promelaNodes, node]
            };
        });
    return Statement(
        Context(result.childInfo, updateNodeCreator(context.metaInfo)(result.nodeCreator)),
        result.promelaNodes
    );
};

export const updateDfPull = (nodeCreator: TNodeCreator) =>
                            (dfPull: TDfPull): TNodeCreator =>
    ({
        ...nodeCreator,
        dependsGraph: {
            ...nodeCreator.dependsGraph,
            dfPull
        }
    });

const updateDependsGraph = (dependsGraph: TDependsGraph) =>
                           (nodeCreator: TNodeCreator): TNodeCreator =>
    ({...nodeCreator, dependsGraph});

const createInitVarNodes = (nodeCreator: TNodeCreator): readonly TPromelaASTNode[] =>
    [
        ...nodeCreator.initDependsNodes,
        ...mapArray(
            (promelaDf: TPromelaDf) => PromelaAstNode([promelaDf], `init_var({0});`)
        )(getAll(nodeCreator.dependsGraph.dfPull))
    ];

const updateInitNode = (initNode: TProctypeBodyNode): NodeState =>
    pipe(
        modify((nodeCreator: TNodeCreator) =>
            ({
                ...nodeCreator,
                ltls: combineLtls(nodeCreator)(initNode.ltls),
                initializedOnes: new Set<string>([...nodeCreator.initializedOnes, initNode.promelaDfs[0].promelaName])
            })
        ),
        map(() => initNode)
    );


const updateUseNode = (useNode: TProctypeBodyNode): NodeState =>
    pipe(
        modify((nodeCreator: TNodeCreator) =>
            ({
                ...nodeCreator,
                ltls: combineLtls(nodeCreator)(useNode.ltls)
            })
        ),
        map(() => useNode)
    );

const updateDependsNode = (dependsNode: DependsNode) =>
                          (nodeCreator: TNodeCreator): TNodeCreator => {
    const initDependsNodes = (!nodeCreator.initDependsNodes.some(equalsDependsNodeContent(dependsNode)))
        ? [...nodeCreator.initDependsNodes, dependsNode.initDependsNodes]
        : nodeCreator.initDependsNodes;
    return {
        ...nodeCreator,
        initDependsNodes,
        ltls: combineLtls(nodeCreator)(dependsNode.ltls)
    };
};

const updateDestroyNode = (nodeCreator: TNodeCreator) =>
                          (destroyNode: TProctypeBodyNode): TNodeCreator =>
    ({
        ...nodeCreator,
        ltls: combineLtls(nodeCreator)(destroyNode.ltls)
    });

export const combineLtls = (nodeCreator: TNodeCreator) =>
                           (newLtls: readonly TLtl[]): readonly TLtl[] =>
    combine(nodeCreator.ltls)(newLtls)(equalsLtl);

const mapNodeToTableNode = (shift: number) =>
                           (proctypeNode: TProctypeNode) =>
                           (index: number): readonly TPositionTableNode[] =>
    mapArray(getPositionNode(shift)(index))
       (getEntries(getTable(proctypeNode.nodeSequence)));

const getPositionNode = (shift: number) =>
                        ((index: number) => ([promelaLine, lunaBegin]: readonly [number, number]): TPositionTableNode =>
    PositionTableNode(index + promelaLine + shift, lunaBegin));
