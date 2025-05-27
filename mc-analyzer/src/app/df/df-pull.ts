import {PromelaDf, TPromelaDf} from './promela-df';
import {TLunaDf} from './luna-df';
import {TContextInfo, getCanonicalLunaDf, getEntryDf} from './context-info';
import {pipe} from 'fp-ts/function';
import {State, get, map, of, modify, flatMap} from 'fp-ts/State';
import {mapArray} from '../utils';

export type TDfPull = {
    readonly counter: number;
    readonly promelaDfs: Map<string, TPromelaDf>;
};

export const DfPull = (counter: number, promelaDfs: Map<string, TPromelaDf>): TDfPull =>
    ({ counter, promelaDfs });

export const getFromLunaDf = (parentInfo: TContextInfo) =>
                             (lunaDf: TLunaDf): State<TDfPull, TPromelaDf> => {
    const canonicalLunaDf = getCanonicalLunaDf(parentInfo)(lunaDf);
    const promelaDf = getEntryDf(parentInfo)(canonicalLunaDf);
    if (!promelaDf) {
        return generateName(lunaDf);
    }
    return pipe(
        get<TDfPull>(),
        map(() => promelaDf)
    );
};

export const getFromPromelaDfName = (dfPull: TDfPull) =>
                                    (promelaName: string): TPromelaDf =>
    dfPull.promelaDfs.get(promelaName);

export const getAllPromelaDfs = (dfPull: TDfPull): ((arr: readonly string[]) => readonly TPromelaDf[]) =>
    mapArray(getFromPromelaDfName(dfPull));

export const getAll = (dfPull: TDfPull) =>
    [...dfPull.promelaDfs.values()];

const increment = (state: TDfPull): TDfPull =>
    ({ ...state, counter: state.counter + 1 });

const updatePromelaDfs = (promelaDf: TPromelaDf): State<TDfPull, TPromelaDf> =>
    pipe(
        modify((state: TDfPull) =>
            ({ ...state, promelaDfs: new Map(state.promelaDfs).set(promelaDf.promelaName, promelaDf) })),
        flatMap(() => of(promelaDf))
    );

const nextName = (state:  TDfPull): string =>
    `var${state.counter}`;

const generateName = (lunaDf: TLunaDf): State<TDfPull, TPromelaDf> =>
    pipe(
        modify(increment),
        flatMap(get<TDfPull>),
        map(nextName),
        map((promelaName: string) => PromelaDf(lunaDf, promelaName)),
        flatMap(updatePromelaDfs)
    );
