import {TLunaDf} from './luna-df';
import {DfNode} from '../luna-ast';

export type TPromelaDf = {
    readonly promelaName: string;
    readonly lunaDf: TLunaDf;
};

export const PromelaDf = (lunaDf: TLunaDf, promelaName: string): TPromelaDf =>
    ({promelaName, lunaDf});

export const getPromelaDfFullName = (promelaDf: TPromelaDf) =>
    promelaDf.lunaDf.fullName;

export const getPromelaDfLunaName = (promelaDf: TPromelaDf) =>
    promelaDf.lunaDf.lunaName;

export const getPromelaName = (promelaDf: TPromelaDf) =>
    promelaDf.promelaName;

export const isDeclared = (dfNode: DfNode) => (promelaDf: TPromelaDf): boolean =>
    dfNode.names.some((name: string) => promelaDf.lunaDf.lunaName === name);
