import {TCondNode, LiteralCond} from '../luna-ast';
import {LunaDf, TLunaDf} from './luna-df';
import {flatMapArray, join, joinEmpty, mapArray} from '../utils';

export type TLunaDfIndex = {
    readonly name: string;
    readonly nestedDfs: readonly TLunaDf[];
};

export const LunaDfIndex = (cond: TCondNode): TLunaDfIndex => {
    if (!cond) {
        return {name: "0", nestedDfs: []};
    }
    switch (cond.type) {
        case 'rconst':
        case 'iconst':
            return { name: literalToString(cond), nestedDfs: [] };
        case 'id': {
            const df: TLunaDf = LunaDf(cond);
            return { name: df.fullName, nestedDfs: [df] };
        }
        default: {
            const indexes = mapArray(LunaDfIndex)(cond.operands);
            return {
                name: join(cond.type)(mapArray(getIndexName)(indexes)),
                nestedDfs: flattenNestedDfs(indexes)
            };
        }
    }
};

export const Expr = (cond: TCondNode): string => {
    switch (cond.type) {
        case 'rconst':
        case 'iconst':
            return `(${literalToString(cond)})`;
        case 'id':
            return `(${LunaDf(cond).fullName})`;
        default: {
            const indexes = mapArray(Expr)(cond.operands);
            return `(${join(cond.type)(indexes)})`;
        }
    }
};

export const getLunaDfIndexNestedDfs = (lunaDfIndex: TLunaDfIndex) =>
    lunaDfIndex.nestedDfs;

export const getIndexNames = (lunaDfIndexes: readonly string[]) =>
    joinEmpty(mapArray(wrapIndexName)(lunaDfIndexes));

export const createLunaDfIndexes = (conds: TCondNode[]): readonly TLunaDfIndex[]  =>
    mapArray(LunaDfIndex)(conds);

const getIndexName = (index: TLunaDfIndex): string => index.name;

const wrapIndexName = (indexName: string): string => `[${indexName}]`;

const extractNestedDfs = (index: TLunaDfIndex): readonly TLunaDf[] => index.nestedDfs;

const flattenNestedDfs = (lunaDfIndexes: readonly TLunaDfIndex[]): readonly TLunaDf[] =>
    flatMapArray(extractNestedDfs)(lunaDfIndexes);

const literalToString = (cond: LiteralCond): string => cond.value.toString();
