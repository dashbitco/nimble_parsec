# Used by "mix format"
locals_without_parens = [defparsec: 2, defparsec: 3]

[
  inputs: ["mix.exs", "{lib,test}/**/*.{ex,exs}"],
  locals_without_parens: locals_without_parens,
  export: [locals_without_parens: locals_without_parens]
]
