# Notes

## Definitions of primitive combinators

### Call: flatten and interpret a list

call = choose {
    # we have a pair
    decons
    mng
    ...
}
{
    # primitive term
    mng
}
isPair

### isPair: check if a term is a pair

isPair (Pair a b) = True (Pair a b)
isPair a = False a

Alternative definition:

isPair = eqTok Pair typeof dup


### eqTok: check if two tokens are equal

Reduce to True if the following two tokens are equal, False otherwise.
