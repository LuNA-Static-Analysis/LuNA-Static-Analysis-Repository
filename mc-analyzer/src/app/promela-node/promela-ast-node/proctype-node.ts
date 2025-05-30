import {TProctypeBodyNode} from '../proctype-body-node/proctype-body-node';
import {PositionTable, PositionTableNode, TPositionTable, TPositionTableNode} from '../position-table';
import {addIndent, filter, flatMapArray, indexedMap, joinNewLine, mapArray} from '../../utils';
import {getNodeContent, TPromelaASTNode} from './promela-ast-node';
import {formulaCounter} from "../node-creator";
import {PromelaDf, TPromelaDf} from "../../df/promela-df";
import {TLtl} from "../../ltl/ltl";
import {LunaDf, TLunaDf} from "../../df/luna-df";

export type TProctypeNode = TPromelaASTNode & {
    readonly nodeSequence: readonly TProctypeBodyNode[];
};

export const ProctypeNode = (name: string, nodeSequence: readonly TProctypeBodyNode[]): TProctypeNode => {
    const contents = mapArray(getNodeContent)(nodeSequence);
    const bodyText = joinNewLine(mapArray(addIndent(1))(contents));
    return { nodeSequence, content: `active proctype ${name}() {\n${bodyText}\n}` };
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
