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
    if (cond.type as any === 'icast' || cond.type as any === 'rcast') {
        cond = (cond as any).expr as TCondNode;
    }
    switch (cond.type) {
        case 'rconst':
        case 'iconst':
        case 'sconst':
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
    if (cond.type as any === 'icast' || cond.type as any === 'rcast') {
        cond = (cond as any).expr as TCondNode;
    }
    switch (cond.type) {
        case 'rconst':
        case 'iconst':
        case 'sconst':
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
