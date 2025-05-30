import {getPromelaDfFullName, getPromelaName, TPromelaDf} from '../df/promela-df';
import {cycleToString, doubleArray, flatMapArray, includes, indexedMap, join, mapArray, shift} from '../utils';

export type TLtl = {
    readonly promelaDfs: readonly TPromelaDf[];
    readonly code: string;
    readonly name: string;
    readonly description: string;
    readonly statement: string;
    readonly bool?: 'true' | 'false';
    readonly condition?: string;
    readonly begin?: number;
};

type TSEM2_1Error = {
    error_code: 'SEM2.1',
    details: {
        initialized: {
            true: string,
            local: string,
            conditions: string[],
            where: ""
        }
    }
}

export const Ltl = (promelaDfs: readonly TPromelaDf[],
                    code: string, condition: string,
                    description: string, bool = undefined): TLtl => {
    const prettyCycle = join('_')(
        mapArray(getPromelaName)(promelaDfs)
    );
    const name = `${code}_${prettyCycle}`.replace('.', '_');
    const statement = `ltl ${name} ${condition}`;
    return { name, statement, promelaDfs, code, description, bool };
};

export const EmptyLtl = (promelaDfs: readonly TPromelaDf[]) =>
    ({
        promelaDfs,
        code: getEmptyLtlCode(),
        name: '',
        statement: '',
        description: ''
    });

export const getEmptyLtlCode = () => '';
export const getSem2_1Code = () => 'SEM2.1';
export const getSem3_1Code = () => 'SEM3.1';
export const getSem3_2Code = () => 'SEM3.2';
export const getSem3_6Code = () => 'SEM3.6';
export const getSem4Code = () => 'SEM4';
export const getSem5Code = () => 'SEM5';
export const getSem6Code = () => 'SEM6';


export const equalsLtl = (first: TLtl) =>
                         (second: TLtl): boolean =>
    (first.code === second.code && ({
        [getSem3_2Code()]: cycleEqualsLtl,
        [getSem2_1Code()]: defaultEqualsLtl,
        [getSem3_1Code()]: defaultEqualsLtl,
        [getSem3_6Code()]: defaultEqualsLtl,
        [getSem4Code()]: defaultEqualsLtl,
        [getSem5Code()]: defaultEqualsLtl,
        [getSem6Code()]: defaultEqualsLtl,
        [getEmptyLtlCode()]: () => () => false
    })[first.code](first)(second)) ?? false;

const defaultEqualsLtl = (first: TLtl) =>
                         (second: TLtl): boolean =>
    first.name === second.name;

export const LtlSem2_1 = (promelaDf: TPromelaDf): TLtl =>
    Ltl([promelaDf],
        getSem2_1Code(),
        `{[] (init_count_${promelaDf.promelaName} < 2)}`,
        `${promelaDf.lunaDf.fullName} может быть проинициализирован дважды`
    );

export const LtlSem3_1 = (promelaDf: TPromelaDf, begin: number): TLtl =>
    ({
        ...Ltl([promelaDf],
                getSem3_1Code(),
            `{[] (check_init(${promelaDf.promelaName}))}`,
            `${promelaDf.lunaDf.fullName} используется, но может быть не проинициализирован`),
        begin
    });

export const LtlSem3_6 = (promelaDf: TPromelaDf): TLtl =>
    Ltl([promelaDf],
        getSem3_6Code(),
        `{[] (destroy_count_${promelaDf.promelaName} > 0 -> use_count_${promelaDf.promelaName} < 2)}`,
        `${promelaDf.lunaDf.fullName} может быть использован после удаления`
    );

export const LtlSem4 = (promelaDf: TPromelaDf): TLtl =>
    Ltl([promelaDf],
        getSem4Code(),
        `{[] (check_usage(${promelaDf.promelaName}))}`,
        `${promelaDf.lunaDf.fullName} инициализируется, но может не использоваться`
    );

export const LtlSem3_2 = (promelaDfs: readonly TPromelaDf[]): TLtl =>
    Ltl(promelaDfs,
        getSem3_2Code(),
        `{[] !(${getLtlCondition(promelaDfs)})}`,
        `Циклическая зависимость по данным: ${getDfSequence(promelaDfs)}`
    );

export const LtlSem5 = (cond: string, bool: string): TLtl =>
    Ltl([{promelaName: cond, lunaDf: {fullName: cond}} as TPromelaDf],
        getSem5Code(),
        `{ [] ( (true_count_${cond} == 0) && (false_count_${cond} == 0) ) }`,
        `Формула в if/while истинна/ложна во всех путях выполнения`, bool
    );

export const LtlSem6 = (cond: string, bool: string): TLtl =>
    Ltl([{promelaName: cond, lunaDf: {fullName: cond}} as TPromelaDf],
        getSem6Code(),
        '{ [] (' +
            `\t( (true_count_${cond} > 0) -> (<> (false_count_${cond} > 0)) ) &&` +
            `\t( (false_count_${cond} > 0) -> (<> (true_count_${cond} > 0)) )` +
        ')}',
        `Формула в if/while истинна/ложна во всех путях выполнения`, bool
    );

const getDfSequence = (promelaDfs: readonly TPromelaDf[]): string =>
    join('->')(
        mapArray(getPromelaDfFullName)(promelaDfs.concat(promelaDfs[0]))
    );

const getLtlCondition = (promelaDfs: readonly TPromelaDf[]): string =>
    join(' && ')(
        indexedMap((promelaDf: TPromelaDf) => (index: number) =>
            `depends_on_${promelaDf.promelaName}_${shift(promelaDfs)(index).promelaName}`
        )(promelaDfs)
    );

export const ltlsToPromelaDfs = (ltls: readonly TLtl[]): readonly TPromelaDf[] =>
    flatMapArray((ltl: TLtl) => ltl.promelaDfs)(ltls);

export const cycleEqualsLtl = (first: TLtl) =>
                              (second: TLtl): boolean =>
    first.promelaDfs.length === second.promelaDfs.length &&
    includes(
        cycleToString(getPromelaName)(doubleArray(first.promelaDfs))
    )(cycleToString(getPromelaName)(second.promelaDfs));

export const getLtlStatement = (ltl: TLtl): string =>
    ltl.statement;
