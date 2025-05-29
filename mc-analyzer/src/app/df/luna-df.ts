import {TCondNode, TDfCond} from '../luna-ast';
import {createLunaDfIndexes, getIndexNames, getLunaDfIndexNestedDfs, TLunaDfIndex} from './luna-df-index';
import {flatMapArray, mapArray} from '../utils';

export type TLunaDf = {
    readonly lunaName: string;
    readonly indexes: readonly TLunaDfIndex[];
    readonly fullName: string;
    readonly begin: number;
};

export const LunaDf = (dfNode: TDfCond) => {
    const [lunaName, ...condIndexes]: readonly [string, ...TCondNode[]] = dfNode.ref;
    const indexes = createLunaDfIndexes(condIndexes);
    return {
        lunaName,
        indexes,
        fullName: getLunaDfFullName(lunaName)(indexes),
        begin: dfNode.begin
    };
};

export const getNestedDfs = (lunaDf: TLunaDf): readonly TLunaDf[] =>
    flatMapArray(getLunaDfIndexNestedDfs)(lunaDf.indexes);

const getLunaDfFullName = (lunaName: string) =>
                          (indexes: readonly TLunaDfIndex[]) =>
    lunaName + getIndexNames(mapArray((idx: TLunaDfIndex) => idx.name)(indexes));
