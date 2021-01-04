# style guide

- Canadian spelling
- Ambiguous spellings resolutions:
  - sub-expression, not subexpression
  - non-terminal, not nonterminal
- Instructions are typeset as code only if they are nouns or code, but not if
  description.
  - "A @typeset{jump} references ... " vs "The jump statement".
  - Does this make sense? Contradicts my style guide for other things.
- Language grammar styles:
  - atoms (registers, keywords) ought not be italicized.
  - predicates (int64?) ought be hyperlinked with @racket.
  - non-terminals ought be italicized.
  - this style is enforced by bettergrammar-lib if datum and datum-literals are provided.
