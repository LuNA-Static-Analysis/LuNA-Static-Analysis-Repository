import {execSync} from 'child_process';
import {main} from "../app/main";

describe('Script Tests', () => {
    test('Script should exit with error if no arguments provided', () => {
        console.log(main().toString());

    });

    test('Script should main without errors when given a valid argument', () => {
        expect(() => execSync('./main test/main2.fa')).not.toThrow();
    });

    test('Script should print JSON array with errors if verification fails', () => {
        const result = execSync('./main test/main2.fa').toString();
        const errors = JSON.parse(result);
        expect(errors).toBeDefined();
        expect(errors).toBeInstanceOf(Array);
    });
});

