# style guide

- Canadian spelling
- Ambiguous spellings resolutions:
  - sub-expression, not subexpression
  - non-terminal, not nonterminal
- Instructions are typeset as code only if they are nouns or code, but not if
  description.
  - "A @typeset{jump} references ... " vs "The jump statement".
  - Does this make sense? Contradicts my style guide for other things.
- The book should be about concepts and design; references to representation, software, should be in margin notes.
  - e.g., the grammar definitions are abstract, but if I refer to their representation, that should be in margin notes or other explicit asides like examples.
  - references to the run-time system implementation should discuss design, reference to cpsc411/2c-run-time should be in margin-note.
- Language grammar styles:
  - atoms (registers, keywords) ought not be italicized.
  - predicates (int64?) ought be hyperlinked with @racket.
  - non-terminals ought be italicized.
  - this style is enforced by bettergrammar-lib if datum and datum-literals are provided.
