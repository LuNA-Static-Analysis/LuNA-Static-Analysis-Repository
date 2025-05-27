import {TProctypeBodyNode} from '../proctype-body-node/proctype-body-node';
import {PositionTable, PositionTableNode, TPositionTable, TPositionTableNode} from '../position-table';
import {addIndent, filter, flatMapArray, indexedMap, joinNewLine, mapArray} from '../../utils';
import {getNodeContent, TPromelaASTNode} from './promela-ast-node';
import {formulaCounter} from "../node-creator";

export type TProctypeNode = TPromelaASTNode & {
    readonly nodeSequence: readonly TProctypeBodyNode[];
};


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

export const ProctypeNode = (name: string, nodeSequence: readonly TProctypeBodyNode[]): TProctypeNode => {
    const contents = mapArray(getNodeContent)(nodeSequence);
    const bodyText = joinNewLine(mapArray(addIndent(1))(contents));
    const combines = generateBinaryCombinations(Array.from({ length: formulaCounter}).map((_, i) => `cond${i}`));
    const body = joinNewLine(mapArray(addIndent(1))(combines
        .map(obj => `:: ${
            Object.entries(obj)
                .map(([key, val]) => `${key}=${val}`)
                .join(';')}`
            )));
    return formulaCounter === 0
        ? { nodeSequence, content: `active proctype ${name}() {\n${bodyText}\n}` }
        : { nodeSequence, content: `active proctype ${name}() {\n   if\n${body}\n   fi\n${bodyText}\n}`};
};

export const getTable = (nodeSequence: readonly TProctypeBodyNode[]): TPositionTable =>
    PositionTable(
        filter(isExpression)(
            indexedMap(createTableNode)(getBeginsProctypeNode(nodeSequence))
        ));

const getBeginsProctypeNode = (proctype: readonly TProctypeBodyNode[]) =>
    [-1].concat(
        flatMapArray((node: TProctypeBodyNode) => node.begins)(proctype)
    ).concat([-1]);

const createTableNode = (begin: number) =>
                        (index: number) =>
    PositionTableNode(index, begin);

const isExpression = (tableNode: TPositionTableNode) =>
    tableNode.begin !== -1;
