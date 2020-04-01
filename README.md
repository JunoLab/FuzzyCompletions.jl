# FuzzyCompletions.jl

`FuzzyCompletions` provides fuzzy completions.

Its API is totally compatible with compatible with the standard library module [`REPL.REPLCompletions`](https://github.com/JuliaLang/julia/blob/master/stdlib/REPL/src/REPLCompletions.jl);
so this package can be used as a drop-in replacement for your completion provider.

## example

You can make your REPL complete fuzzily with the following code:

```julia
using REPL
using REPL.LineEdit
using FuzzyCompletions

struct FuzzyCompletionProvider <: REPL.CompletionProvider
    mod::Module
end

function LineEdit.complete_line(c::FuzzyCompletionProvider, s)
  partial = REPL.beforecursor(s.input_buffer)
  full = LineEdit.input_string(s)

  # module-aware repl backend completions
  comps, range, should_complete = completions(full, lastindex(partial), c.mod)
  filter!(c->score(c)≥0, comps)
  return unique!(FuzzyCompletions.completion_text.(comps)), partial[range], should_complete
end

let main_mode = Base.active_repl.interface.modes[1]
    main_mode.complete = FuzzyCompletionProvider(Main) # or whatever module where you want to get completes from
end
```

Then, your REPL will work like

```julia-repl
julia> sthing
isnothing something # fuzzy completions

julia> regex<tab>
Regex      RegexMatch # cases doesn't need to match
```

But I'm sure you will dislike this behavior;
we usually want CLI to complete eagerly, not to be an indecisive boy.

## consumers

Fuzzy completions can be useful within IDE or somewhere some richer UI for showing/selecting completion suggestions is available:
- [Juno](https://junolab.org/): TODO add a link to actual implementation

## acknowledgment

The original idea of the implementation came from [this patch by @pfitzseb](https://github.com/pfitzseb/julia/commit/740dd16843c16cb0b87264911f43abf8485652fe).

## license

This package is under [MIT License](LICENSE.md).
