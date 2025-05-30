import {mapArray} from '../utils';

export type TPositionTable = Map<number, number>;

export type TPositionTableNode = {
    readonly line: number;
    readonly begin: number;
};

export const PositionTable = (nodes: readonly TPositionTableNode[]): TPositionTable =>
    new Map(mapArray(EntryNode)(nodes));

export const getBegin = (positionTable: TPositionTable) => (promelaLine: number): number =>
    positionTable.get(promelaLine);

export const getEntries = (positionTable: TPositionTable): readonly [number, number][] =>
    [...positionTable.entries()];

export const PositionTableNode = (line: number, begin: number): TPositionTableNode =>
    ({ line, begin });

const EntryNode = (node: TPositionTableNode): readonly [number, number] =>
    [node.line, node.begin];
