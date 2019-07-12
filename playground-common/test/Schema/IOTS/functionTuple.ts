import * as t from 'io-ts'

// Schema.IOTSSpec.CurrencySymbol
const CurrencySymbol = t.type({
    unCurrencySymbol: t.string
});

// Schema.IOTSSpec.TokenName
const TokenName = t.type({
    unTokenName: t.string
});

const SomeFunctionArgA = t.tuple([
    CurrencySymbol,
    TokenName
]);

const SomeFunctionArgReturn = t.string;

type SomeFunction = (
    a: t.TypeOf<typeof SomeFunctionArgA>
) => t.TypeOf<typeof SomeFunctionArgReturn>;
