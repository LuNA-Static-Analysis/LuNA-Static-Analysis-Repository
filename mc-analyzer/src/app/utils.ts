import {TPromelaDf} from './df/promela-df';

export const substitute = (template: string) =>
                          (promelaDfs: readonly TPromelaDf[]): string => {
    const templatePosition = /\{(\d+)}/g;
    return template
        .replace(templatePosition,
                (_match: string, index: number) => promelaDfs[index].promelaName);
};

export const join = (sep: string) =>
                    (arr: readonly string[]) =>
    arr.join(sep);

export const joinEmpty = join('');

export const joinNewLine = join('\n');

export const indexedMap = <T, M>(func: (elem: T) =>
                                       (index: number) => M) =>
                                (arr: readonly T[]): readonly M[] =>
    arr.map((elem, index) => func(elem)(index));

export const reduce = <T>(arr: readonly T[]) =>
                      <R>(initialValue: R) =>
                         (func: (previousValue: R) =>
                                (curValue: T) => R) =>
    arr.reduce((previousValue: R, node: T) => func(previousValue)(node), initialValue);

export const sort = (arr: readonly string[]): readonly string[] =>
    arr.slice().sort((a: string, b: string) => a.localeCompare(b));

export const filter = <T>(func: (elem: T) => boolean) =>
                         (arr: readonly T[]): readonly T[] =>
    arr.filter(func);

export const indexedFilter = <T>(func: (index: number) => boolean) =>
                                (arr: readonly T[]): readonly T[] =>
    arr.filter((_elem, index) => func(index));

export const indexedSelfFilter =
     <T>(func: (elem: T) =>
               (index: number) =>
               (self: readonly T[]) => boolean) =>
        (arr: readonly T[]): readonly T[] =>
    arr.filter((elem, index, self) => func(elem)(index)(self));

export const flatMapArray = <T, M>(func: (elem: T) => readonly M[]) =>
                             (arr: readonly T[]): readonly M[] =>
    arr.flatMap(func);

export const indexedFlatMap = <T, M>(func: (elem: T) =>
                                           (index: number) => readonly M[]) =>
                                    (arr: readonly T[]): readonly M[] =>
    arr.flatMap((elem, index) => func(elem)(index));

export const mapArray = <T, M>(func: (elem: T) => M) =>
                         (arr: readonly T[]): readonly M[] =>
    arr.map(func);

export const doubleArray = <T>(arr: readonly T[]): T[] =>
    [...arr, ...arr];

export const includes = (text: string) => (searchString: string): boolean =>
    text.includes(searchString);

export const cycleToString = <T>(func: (elem: T) => string) =>
                                (arr: readonly T[]) =>
    join(',')(
        mapArray(func)(arr)
    );

export const addIndent = (indentLevel: number) =>
                         (text: string) => {
    const indent = ' '.repeat(indentLevel * 3);
    return indent + text.replace(/\n/g, '\n' + indent);
};

export const combine = <T>(firstArr: readonly T[]) =>
                          (secondArr: readonly T[]) =>
                          (comparator: (first: T) =>
                                       (second: T) => boolean): readonly T[] =>
    [
        ...firstArr,
        ...secondArr.filter((elem: T) => !firstArr.some(comparator(elem)))
    ];

export const shift = <T>(arr: readonly T[]) =>
                        (index: number) =>
    arr[(index + 1) % arr.length];

export type State<S, A> = (state: S) =>
    [A, S];

export const bind = <S, A, B>(transform: (value: A) => State<S, B>) =>
                             (initialState: State<S, A>): State<S, B> =>
    (currentState: S) => {
        const [value, nextState] = initialState(currentState);
        return transform(value)(nextState);
    };

export const of = <S, A>(value: A) =>
                        (state: S): [A, S] =>
    [value, state];

export const get = <S>(): State<S, S> =>
                      (state: S) =>
    [state, state];

export const put = <S>(newState: S): State<S, void> =>
                      () =>
    [undefined, newState];
