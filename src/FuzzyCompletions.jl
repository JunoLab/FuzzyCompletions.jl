# adapted from https://github.com/JuliaLang/julia/blob/cced577b79316ea38482d84b69e7be9666d14041/stdlib/REPL/src/REPLCompletions.jl
# by aviatesk, 2020/02/28

module FuzzyCompletions

export completions, shell_completions, bslash_completions, completion_text, score

using Base.Meta
using Base: propertynames, something
using REPL

# subtracting a small term proportional to levenshtein distance allows for case-sensitive matching
# without affecting other behaviours
const DISCOUNT_COEF_LEVENSTEIN = 1e-4

fuzzyscore(needle::String, haystack::String) =
    REPL.fuzzyscore(needle, haystack) - DISCOUNT_COEF_LEVENSTEIN * REPL.levenshtein(needle, haystack)
fuzzyscore(needle, haystack) = fuzzyscore(string(needle), string(haystack))

abstract type Completion end

# NOTE no fuzzyness
struct TextCompletion <: Completion
    text::String
end

struct KeywordCompletion <: Completion
    keyword::String
    score::Float64
    KeywordCompletion(keyword::String) = new(keyword, Inf)
    KeywordCompletion(keyword::String, needle) = new(keyword, fuzzyscore(needle, keyword))
end

struct PathCompletion <: Completion
    path::String
    score::Float64
    PathCompletion(path::String) = new(path, Inf)
    PathCompletion(path::String, needle) = new(path, fuzzyscore(needle, path))
end

struct ModuleCompletion <: Completion
    parent::Module
    mod::String
    score::Float64
    ModuleCompletion(parent::Module, mod::String) = new(parent, mod, Inf)
    ModuleCompletion(parent::Module, mod::String, needle) = new(parent, mod, fuzzyscore(needle, mod))
end

struct PackageCompletion <: Completion
    package::String
    score::Float64
    PackageCompletion(package::String) = new(package, Inf)
    PackageCompletion(package::String, needle) = new(package, fuzzyscore(needle, package))
end

struct PropertyCompletion <: Completion
    value
    property::Symbol
    score::Float64
    PropertyCompletion(value, property::Symbol) = new(value, property, Inf)
    PropertyCompletion(value, property::Symbol, needle) = new(value, property, fuzzyscore(needle, property))
end

struct FieldCompletion <: Completion
    typ::DataType
    field::Symbol
    score::Float64
    FieldCompletion(typ::DataType, field::Symbol) = new(typ, field, Inf)
    FieldCompletion(typ::DataType, field::Symbol, needle) = new(typ, field, fuzzyscore(needle, field))
end

# NOTE no fuzzyness
struct MethodCompletion <: Completion
    tt
    method::Method
    MethodCompletion(@nospecialize(tt), method::Method) = new(tt, method)
end

struct BslashCompletion <: Completion
    bslash::String
    score::Float64
    BslashCompletion(bslash::String) = new(bslash, Inf)
    BslashCompletion(bslash::String, needle) = new(bslash, fuzzyscore(needle, bslash))
end

struct ShellCompletion <: Completion
    text::String
end

struct DictCompletion <: Completion
    dict::AbstractDict
    key::String
    score::Float64
    DictCompletion(dict::AbstractDict, key::String) = new(dict, key, Inf)
    DictCompletion(dict::AbstractDict, key::String, needle) = new(dict, key, fuzzyscore(needle, key))
end

struct KeywordArgumentCompletion <: Completion
    kwarg::String
    score::Float64
    KeywordArgumentCompletion(kwarg::String) = new(kwarg, Inf)
    KeywordArgumentCompletion(kwarg::String, needle) = new(kwarg, fuzzyscore(kwarg, needle))
end

# interface definition
function Base.getproperty(@nospecialize(c::Completion), name::Symbol)
    if name === :score
        return getfield(c, :score)::Float64
    elseif name === :text
        return getfield(c, :text)::String
    elseif name === :keyword
        return getfield(c, :keyword)::String
    elseif name === :path
        return getfield(c, :path)::String
    elseif name === :parent
        return getfield(c, :parent)::Module
    elseif name === :mod
        return getfield(c, :mod)::String
    elseif name === :package
        return getfield(c, :package)::String
    elseif name === :property
        return getfield(c, :property)::Symbol
    elseif name === :field
        return getfield(c, :field)::Symbol
    elseif name === :method
        return getfield(c, :method)::Method
    elseif name === :bslash
        return getfield(c, :bslash)::String
    elseif name === :text
        return getfield(c, :text)::String
    elseif name === :key
        return getfield(c, :key)::String
    end
    return getfield(c, name)
end

_completion_text(c::TextCompletion) = c.text
_completion_text(c::KeywordCompletion) = c.keyword
_completion_text(c::PathCompletion) = c.path
_completion_text(c::ModuleCompletion) = c.mod
_completion_text(c::PackageCompletion) = c.package
_completion_text(c::PropertyCompletion) = string(c.property)
_completion_text(c::FieldCompletion) = string(c.field)
_completion_text(c::MethodCompletion) = repr(c.method)
_completion_text(c::BslashCompletion) = c.bslash
_completion_text(c::ShellCompletion) = c.text
_completion_text(c::DictCompletion) = c.key
_completion_text(c::KeywordArgumentCompletion) = c.kwarg*'='

completion_text(@nospecialize c::Completion) = _completion_text(c)::String

const Completions = Tuple{Vector{Completion}, UnitRange{Int}, Bool}

score(c::Completion) = c.score
score(c::TextCompletion) = 0.0
score(c::MethodCompletion) = 0.0
score(c::ShellCompletion) = 0.0

import REPL.REPLCompletions:
    appendmacro!

function filtered_mod_names(ffunc::Function, mod::Module, name::AbstractString, all::Bool = false, imported::Bool = false)
    ssyms = names(mod, all = all, imported = imported)
    filter!(ffunc, ssyms)
    syms = String[string(s) for s in ssyms]
    macros = filter(x -> startswith(x, "@" * name), syms)
    appendmacro!(syms, macros, "_str", "\"")
    appendmacro!(syms, macros, "_cmd", "`")
    filter!(sym -> '#' ∉ sym, syms)
    return [ModuleCompletion(mod, sym, name) for sym in syms]
end

# REPL Symbol Completions
function complete_symbol(sym, @nospecialize(ffunc), context_module=Main)::Vector{Completion}
    mod = context_module
    name = sym

    lookup_module = true
    t = Union{}
    val = nothing
    if something(findlast(in(non_identifier_chars), sym), 0) < something(findlast(isequal('.'), sym), 0)
        # Find module
        lookup_name, name = rsplit(sym, ".", limit=2)

        ex = Meta.parse(lookup_name, raise=false, depwarn=false)

        @static if VERSION ≥ v"1.10.0-DEV.941"
            res = repl_eval_ex(ex, context_module)
            res === nothing && return Completion[]
            if res isa Const
                val = res.val
                if isa(val, Module)
                    mod = val
                    lookup_module = true
                else
                    lookup_module = false
                    t = typeof(val)
                end
            else
                lookup_module = false
                t = Core.Compiler.widenconst(res)
            end
        else
            b, found = get_value(ex, context_module)
            if found
                val = b
                if isa(b, Module)
                    mod = b
                    lookup_module = true
                elseif Base.isstructtype(typeof(b))
                    lookup_module = false
                    t = typeof(b)
                end
            else # If the value is not found using get_value, the expression contain an advanced expression
                lookup_module = false
                t, found = get_type(ex, context_module)
            end
            found || return Completion[]
        end
        # Ensure REPLCompletion do not crash when asked to complete a tuple, #15329
        !lookup_module && t <: Tuple && return Completion[]
    end

    suggestions = Completion[]
    if lookup_module
        # We will exclude the results that the user does not want, as well
        # as excluding Main.Main.Main, etc., because that's most likely not what
        # the user wants
        p = s->(!Base.isdeprecated(mod, s) && s != nameof(mod) && ffunc(mod, s)::Bool)
        # Looking for a binding in a module
        if mod == context_module
            # Also look in modules we got through `using`
            mods = ccall(:jl_module_usings, Any, (Any,), context_module)::Vector
            for m in mods
                append!(suggestions, filtered_mod_names(p, m::Module, name))
            end
            append!(suggestions, filtered_mod_names(p, mod, name, true, true))
        else
            append!(suggestions, filtered_mod_names(p, mod, name, true, false))
        end
    elseif val !== nothing # looking for a property of an instance
        for property in propertynames(val, false)
            s = string(property)
            push!(suggestions, PropertyCompletion(val, property, name))
        end
    else
        # Looking for a member of a type
        if t isa DataType && t != Any
            # Check for cases like Type{typeof(+)}
            if t isa DataType && t.name === Base._TYPE_NAME
                t = typeof(t.parameters[1])
            end
            # Only look for fields if this is a concrete type
            if isconcretetype(t)
                fields = fieldnames(t)
                for field in fields
                    s = string(field)
                    push!(suggestions, FieldCompletion(t, field, name))
                end
            end
        end
    end
    suggestions
end

import REPL.REPLCompletions:
    sorted_keywords

# NOTE:
# I would like to be a bit strict on `KeywordCompletion`s:
# they are so common that it would look verbose if they appear in every completion.
# I want to restict their fuzzyness only after a strict match on the first character.
function complete_keyword(s::Union{String,SubString{String}})
    c = first(s, 1)
    filtered_keywords = filter(k -> startswith(k, c), sorted_keywords)
    Completion[KeywordCompletion(kw, s) for kw in filtered_keywords]
end

function complete_path(path::AbstractString, pos; use_envpath=false, shell_escape=false)
    # https://github.com/JunoLab/FuzzyCompletions.jl/issues/7
    if endswith(path, ' ')
        # in order to keep type stability, we just return empty completion for this case
        return Completion[], 0:-1, false
        # return REPL.REPLCompletions.complete_path(
        #     path, pos;
        #     use_envpath  = use_envpath,
        #     shell_escape = shell_escape)
    end

    if Base.Sys.isunix() && occursin(r"^~(?:/|$)", path)
        # if the path is just "~", don't consider the expanded username as a prefix
        if path == "~"
            dir, prefix = homedir(), ""
        else
            dir, prefix = splitdir(homedir() * path[2:end])
        end
    else
        dir, prefix = splitdir(path)
    end
    local files
    try
        if isempty(dir)
            files = readdir()
        elseif isdir(dir)
            files = readdir(dir)
        else
            return Completion[], 0:-1, false
        end
    catch
        return Completion[], 0:-1, false
    end

    matches = Set{String}()
    for file in files
        id = try isdir(joinpath(dir, file)) catch; false end
        # joinpath is not used because windows needs to complete with double-backslash
        push!(matches, id ? file * (@static Sys.iswindows() ? "\\\\" : "/") : file)
    end

    if use_envpath && length(dir) == 0
        # Look for files in PATH as well
        local pathdirs = split(ENV["PATH"], @static Sys.iswindows() ? ";" : ":")

        for pathdir in pathdirs
            local actualpath
            try
                actualpath = realpath(pathdir)
            catch
                # Bash doesn't expect every folder in PATH to exist, so neither shall we
                continue
            end

            if actualpath != pathdir && in(actualpath,pathdirs)
                # Remove paths which (after resolving links) are in the env path twice.
                # Many distros eg. point /bin to /usr/bin but have both in the env path.
                continue
            end

            local filesinpath
            try
                filesinpath = readdir(pathdir)
            catch e
                # Bash allows dirs in PATH that can't be read, so we should as well.
                if isa(e, SystemError)
                    continue
                else
                    # We only handle SystemErrors here
                    rethrow()
                end
            end

            for file in filesinpath
                # In a perfect world, we would filter on whether the file is executable
                # here, or even on whether the current user can execute the file in question.
                isfile(joinpath(pathdir, file)) && push!(matches, file)
            end
        end
    end

    matchList = Completion[PathCompletion(shell_escape ? replace(s, r"\s" => s"\\\0") : s, prefix) for s in matches]
    startpos = pos - lastindex(prefix) + 1 - count(isequal(' '), prefix)
    # The pos - lastindex(prefix) + 1 is correct due to `lastindex(prefix)-lastindex(prefix)==0`,
    # hence we need to add one to get the first index. This is also correct when considering
    # pos, because pos is the `lastindex` a larger string which `endswith(path)==true`.
    return sort_suggestions!(matchList), startpos:pos, !isempty(matchList)
end

function complete_expanduser(path::AbstractString, r)
    expanded = expanduser(path)
    return Completion[PathCompletion(expanded)], r, path != expanded
end

import REPL.REPLCompletions:
    find_start_brace,
    latex_symbols,
    emoji_symbols,
    non_identifier_chars,
    whitespace_chars,
    bslash_separators,
    subscripts,
    subscript_regex,
    superscripts,
    superscript_regex,
    afterusing,
    dict_identifier_key

@static if VERSION ≥ v"1.10.0-DEV.941"

import REPL.REPLCompletions: repl_eval_ex
import Core: Const

else

import REPL.REPLCompletions: get_value, get_type

end

@static if VERSION ≥ v"1.9.0-DEV.1034"

import REPL.REPLCompletions:
    identify_possible_method_completion,
    is_broadcasting_expr,
    _complete_methods,
    complete_methods!

function complete_keyword_argument(partial, last_idx, context_module)
    frange, ex, wordrange, = identify_possible_method_completion(partial, last_idx)
    fail = Completion[], 0:-1, frange
    ex.head === :call || is_broadcasting_expr(ex) || return fail

    kwargs_flag, funct, args_ex, kwargs_ex = _complete_methods(ex, context_module, true)::Tuple{Int, Any, Vector{Any}, Set{Symbol}}
    kwargs_flag == 2 && return fail # one of the previous kwargs is invalid

    methods = REPL.REPLCompletions.Completion[]
    complete_methods!(methods, funct, Any[Vararg{Any}], kwargs_ex, -1, kwargs_flag == 1)
    # TODO: use args_ex instead of Any[Vararg{Any}] and only provide kwarg completion for
    # method calls compatible with the current arguments.

    # For each method corresponding to the function call, provide completion suggestions
    # for each keyword that starts like the last word and that is not already used
    # previously in the expression. The corresponding suggestion is "kwname=".
    # If the keyword corresponds to an existing name, also include "kwname" as a suggestion
    # since the syntax "foo(; kwname)" is equivalent to "foo(; kwname=kwname)".
    last_word = partial[wordrange] # the word to complete
    kwargs = Set{String}()
    for m in methods
        m::REPL.REPLCompletions.MethodCompletion
        possible_kwargs = Base.kwarg_decl(m.method)
        current_kwarg_candidates = String[]
        for _kw in possible_kwargs
            kw = String(_kw)
            if !endswith(kw, "...") && startswith(kw, last_word) && _kw ∉ kwargs_ex
                push!(current_kwarg_candidates, kw)
            end
        end
        union!(kwargs, current_kwarg_candidates)
    end

    suggestions = Completion[KeywordArgumentCompletion(kwarg, partial) for kwarg in kwargs]
    repl_completions = @static if VERSION ≥ v"1.10.0-DEV.981"
        REPL.REPLCompletions.complete_symbol(nothing, last_word, Returns(true), context_module)
    else
        REPL.REPLCompletions.complete_symbol(last_word, (mod,x)->true, context_module)
    end
    append!(suggestions, transform_to_fuzzy_completion.(repl_completions))

    return sort!(suggestions, by=completion_text), wordrange
end

else # @static if VERSION ≥ v"1.9.0-DEV.1034"

import REPL.REPLCompletions:
    should_method_complete

end # @static if VERSION ≥ v"1.9.0-DEV.1034"

function transform_to_fuzzy_completion(@nospecialize c)
    if isa(c, REPL.REPLCompletions.MethodCompletion)
        @static if VERSION ≥ v"1.8.0-DEV.1419"
            return MethodCompletion(c.tt, c.method)
        else
            return MethodCompletion(c.input_types, c.method)
        end
    elseif isa(c, REPL.REPLCompletions.TextCompletion)
        return TextCompletion(c.text)
    elseif @static VERSION ≥ v"1.9.0-DEV.1034" && isa(c, REPL.REPLCompletions.KeywordArgumentCompletion)
        return KeywordArgumentCompletion(c.kwarg)
    elseif isa(c, REPL.REPLCompletions.ModuleCompletion)
        return ModuleCompletion(c.parent, c.mod)
    else
        throw("FuzzyCompletions.jl not synced with REPL.REPLCompletions")
    end
end

# bypass to REPL.REPLCompletions.complete_methods
function complete_methods(ex_org::Expr, context_module::Module=Main, shift::Bool=false)
    @static if hasmethod(REPL.REPLCompletions.complete_methods, (Expr,Module,Bool))
        out = REPL.REPLCompletions.complete_methods(ex_org, context_module, shift)
    else
        out = REPL.REPLCompletions.complete_methods(ex_org, context_module)
    end
    return Completion[transform_to_fuzzy_completion(c) for c in out]
end

# bypass to REPL.REPLCompletions.complete_any_methods
function complete_any_methods(ex_org::Expr, callee_module::Module, context_module::Module, moreargs::Bool, shift::Bool)
    out = REPL.REPLCompletions.complete_any_methods(ex_org, callee_module, context_module, moreargs, shift)
    return Completion[transform_to_fuzzy_completion(c) for c in out]
end

function close_path_completion(str, startpos, r, paths, pos)
    length(paths) == 1 || return false  # Only close if there's a single choice...
    _path = str[startpos:prevind(str, first(r))] * (paths[1]::PathCompletion).path
    path = expanduser(replace(_path, r"\\ " => " "))
    # ...except if it's a directory...
    try
        isdir(path)
    catch e
        e isa Base.IOError || rethrow() # `path` cannot be determined to be a file
    end && return false
    # ...and except if there's already a " at the cursor.
    return lastindex(str) <= pos || str[nextind(str, pos)] != '"'
end

function bslash_completions(string, pos)
    slashpos = something(findprev(isequal('\\'), string, pos), 0)
    if (something(findprev(in(bslash_separators), string, pos), 0) < slashpos &&
        !(1 < slashpos && (string[prevind(string, slashpos)]=='\\')))
        # latex / emoji symbol substitution
        s = string[slashpos:pos]
        latex = get(latex_symbols, s, "")
        if !isempty(latex) # complete an exact match
            return (true, (Completion[BslashCompletion(latex)], slashpos:pos, true))
        elseif occursin(subscript_regex, s)
            sub = map(c -> subscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sub)], slashpos:pos, true))
        elseif occursin(superscript_regex, s)
            sup = map(c -> superscripts[c], s[3:end])
            return (true, (Completion[BslashCompletion(sup)], slashpos:pos, true))
        end
        emoji = get(emoji_symbols, s, "")
        if !isempty(emoji)
            return (true, (Completion[BslashCompletion(emoji)], slashpos:pos, true))
        end
        # return possible matches; these cannot be mixed with regular
        # Julian completions as only latex / emoji symbols contain the leading \
        suggestions = Completion[BslashCompletion(k, s) for k in keys(startswith(s, "\\:") ? emoji_symbols : latex_symbols)]
        return (true, (sort_suggestions!(suggestions), slashpos:pos, true))
    end
    return (false, (Completion[], 0:-1, false))
end

# This needs to be a separate non-inlined function, see #19441
@noinline find_dict_matches(identifier) = String[repr(key) for key in keys(identifier)]

@static if isdefined(Base, :parsed_toml)

function project_deps_get_completion_candidates(pkgstarts::String, project_file::String)
    loading_candidates = String[]
    d = Base.parsed_toml(project_file)
    pkg = get(d, "name", nothing)::Union{String, Nothing}
    if pkg !== nothing && startswith(pkg, pkgstarts)
        push!(loading_candidates, pkg)
    end
    deps = get(d, "deps", nothing)::Union{Dict{String, Any}, Nothing}
    if deps !== nothing
        for (pkg, _) in deps
            startswith(pkg, pkgstarts) && push!(loading_candidates, pkg)
        end
    end
    return Completion[PackageCompletion(name) for name in loading_candidates]
end

else # @static if isdefined(Base, :TOML)

function project_deps_get_completion_candidates(pkgstarts::String, project_file::String)
    loading_candidates = String[]
    open(project_file) do io
        state = :top
        for line in eachline(io)
            if occursin(Base.re_section, line)
                state = occursin(Base.re_section_deps, line) ? :deps : :other
            elseif state === :top
                if (m = match(Base.re_name_to_string, line)) !== nothing
                    root_name = String(m.captures[1])
                    push!(loading_candidates, root_name)
                end
            elseif state === :deps
                if (m = match(Base.re_key_to_string, line)) !== nothing
                    dep_name = m.captures[1]
                    push!(loading_candidates, dep_name)
                end
            end
        end
    end
    return Completion[PackageCompletion(name, pkgstarts) for name in loading_candidates]
end

end # @static if isdefined(Base, :TOML)

function completions(string::String, pos::Int, context_module::Module = Main, shift::Bool=true)
    # First parse everything up to the current position
    partial = string[1:pos]
    inc_tag = Base.incomplete_tag(Meta.parse(partial, raise=false, depwarn=false))

    # ?(x, y)TAB lists methods you can call with these objects
    # ?(x, y TAB lists methods that take these objects as the first two arguments
    # MyModule.?(x, y)TAB restricts the search to names in MyModule
    rexm = match(r"(\w+\.|)\?\((.*)$", partial)
    if rexm !== nothing
        # Get the module scope
        if isempty(rexm.captures[1])
            callee_module = context_module
        else
            modname = Symbol(rexm.captures[1][1:end-1])
            if isdefined(context_module, modname)
                callee_module = getfield(context_module, modname)
                if !isa(callee_module, Module)
                    callee_module = context_module
                end
            else
                callee_module = context_module
            end
        end
        moreargs = !endswith(rexm.captures[2], ')')
        callstr = "_(" * rexm.captures[2]
        if moreargs
            callstr *= ')'
        end
        ex_org = Meta.parse(callstr, raise=false, depwarn=false)
        if isa(ex_org, Expr)
            return complete_any_methods(ex_org, callee_module::Module, context_module, moreargs, shift), (0:length(rexm.captures[1])+1) .+ rexm.offset, false
        end
    end

    # if completing a key in a Dict
    identifier, partial_key, loc = dict_identifier_key(partial,inc_tag, context_module)
    if identifier !== nothing
        matches = find_dict_matches(identifier)::Vector{String}
        length(matches)==1 && (lastindex(string) <= pos || string[nextind(string,pos)] != ']') && (matches[1]*=']')
        if length(matches)>0
            suggestions = Completion[DictCompletion(identifier, match, partial_key) for match in matches]
            return sort_suggestions!(suggestions), loc:pos, false
        end
    end

    # otherwise...
    if inc_tag in [:cmd, :string]
        m = match(r"[\t\n\r\"`><=*?|]| (?!\\)", reverse(partial))
        startpos = nextind(partial, reverseind(partial, m.offset))
        r = startpos:pos

        expanded = complete_expanduser(replace(string[r], r"\\ " => " "), r)
        expanded[3] && return expanded  # If user expansion available, return it

        paths, r, success = complete_path(replace(string[r], r"\\ " => " "), pos)

        if inc_tag === :string && close_path_completion(string, startpos, r, paths, pos)
            paths[1] = PathCompletion((paths[1]::PathCompletion).path * "\"")
        end

        # within string scope, append latex completions as well
        if inc_tag === :string
            should_bs, bs_ret = bslash_completions(string, pos)
            if should_bs
                sort_suggestions!(append!(first(bs_ret), paths))
                return bs_ret
            end
        end
        #Latex symbols can be completed for strings
        (success || inc_tag === :cmd) && return paths, r, success
    end

    ok, ret = bslash_completions(string, pos)
    ok && return ret

    # Make sure that only bslash_completions is working on strings
    inc_tag==:string && return Completion[], 0:-1, false
    if @static VERSION ≥ v"1.9.0-DEV.1034" && inc_tag === :other
        frange, ex, wordrange, method_name_end = identify_possible_method_completion(partial, pos)
        if last(frange) != -1 && all(isspace, @view partial[wordrange]) # no last argument to complete
            if ex.head === :call
                return complete_methods(ex, context_module, shift), first(frange):method_name_end, false
            elseif is_broadcasting_expr(ex)
                return complete_methods(ex, context_module, shift), first(frange):(method_name_end - 1), false
            end
        end
    elseif @static VERSION < v"1.9.0-DEV.1034" && inc_tag === :other && should_method_complete(partial)
        frange, method_name_end = find_start_brace(partial)
        # strip preceding ! operator
        s = replace(partial[frange], r"\!+([^=\(]+)" => s"\1")
        ex = Meta.parse(s * ")", raise=false, depwarn=false)

        if isa(ex, Expr)
            if ex.head === :call
                return complete_methods(ex, context_module), first(frange):method_name_end, false
            elseif ex.head === :. && begin
                    x = ex.args[2]
                    isa(x, Expr) && x.head === :tuple
                end
                return complete_methods(ex, context_module), first(frange):(method_name_end - 1), false
            end
        end
    elseif inc_tag === :comment
        return Completion[], 0:-1, false
    end

    @static if VERSION ≥ v"1.9.0-DEV.1034"
    # Check whether we can complete a keyword argument in a function call
    kwarg_completion, wordrange = complete_keyword_argument(partial, pos, context_module)
    isempty(wordrange) || return kwarg_completion, wordrange, !isempty(kwarg_completion)
    end

    dotpos = something(findprev(isequal('.'), string, pos), 0)
    startpos = nextind(string, something(findprev(in(non_identifier_chars), string, pos), 0))
    # strip preceding ! operator
    if (m = match(r"^\!+", string[startpos:pos])) !== nothing
        startpos += length(m.match)
    end

    ffunc = (mod,x)->true
    suggestions = Completion[]
    comp_keywords = true
    if afterusing(string, startpos)
        # We're right after using or import. Let's look only for packages
        # and modules we can reach from here

        # If there's no dot, we're in toplevel, so we should
        # also search for packages
        s = string[startpos:pos]
        if dotpos <= startpos
            for dir in Base.load_path()
                if basename(dir) in Base.project_names && isfile(dir)
                    append!(suggestions, project_deps_get_completion_candidates(s, dir))
                end
                isdir(dir) || continue
                for pname in readdir(dir)
                    if pname[1] != '.' && pname != "METADATA" && pname != "REQUIRE"
                        # Valid file paths are
                        #   <Mod>.jl
                        #   <Mod>/src/<Mod>.jl
                        #   <Mod>.jl/src/<Mod>.jl
                        if isfile(joinpath(dir, pname))
                            endswith(pname, ".jl") && push!(suggestions,
                                                            PackageCompletion(pname[1:prevind(pname, end-2)], s))
                        else
                            mod_name = if endswith(pname, ".jl")
                                pname[1:prevind(pname, end-2)]
                            else
                                pname
                            end
                            if isfile(joinpath(dir, pname, "src",
                                               "$mod_name.jl"))
                                push!(suggestions, PackageCompletion(mod_name, s))
                            end
                        end
                    end
                end
            end
        end
        ffunc = (mod,x)->(Base.isbindingresolved(mod, x) && isdefined(mod, x) && isa(getfield(mod, x), Module))
        comp_keywords = false
    end
    startpos == 0 && (pos = -1)
    dotpos < startpos && (dotpos = startpos - 1)
    s = string[startpos:pos]
    comp_keywords && append!(suggestions, complete_keyword(s))
    # if the start of the string is a `.`, try to consume more input to get back to the beginning of the last expression
    if 0 < startpos <= lastindex(string) && string[startpos] == '.'
        i = prevind(string, startpos)
        while 0 < i
            c = string[i]
            if c in (')', ']')
                if c == ')'
                    c_start = '('
                    c_end = ')'
                elseif c == ']'
                    c_start = '['
                    c_end = ']'
                end
                frange, end_of_identifier = find_start_brace(string[1:prevind(string, i)], c_start=c_start, c_end=c_end)
                isempty(frange)  && break # unbalanced parens
                startpos = first(frange)
                i = prevind(string, startpos)
            elseif c in ['\'', '\"', '\`']
                s = "$c$c"*string[startpos:pos]
                break
            else
                break
            end
            s = string[startpos:pos]
        end
    end
    append!(suggestions, complete_symbol(s, ffunc, context_module))
    return sort_suggestions!(suggestions, s), (dotpos+1):pos, false
end

@inline function sort_suggestions!(suggestions, s=nothing)
    sort!(suggestions, by=score, rev=true)
    s !== nothing && sort!(suggestions, by=(x)->startswith(completion_text(x), s); rev=true)
    return suggestions
end

function shell_completions(string, pos)
    # First parse everything up to the current position
    scs = string[1:pos]
    local args, last_parse
    try
        args, last_parse = Base.shell_parse(scs, true)::Tuple{Expr,UnitRange{Int}}
    catch
        return Completion[], 0:-1, false
    end
    ex = args.args[end]::Expr
    # Now look at the last thing we parsed
    isempty(ex.args) && return Completion[], 0:-1, false
    arg = ex.args[end]
    if all(s -> isa(s, AbstractString), ex.args)
        arg = arg::AbstractString
        # Treat this as a path

        # As Base.shell_parse throws away trailing spaces (unless they are escaped),
        # we need to special case here.
        # If the last char was a space, but shell_parse ignored it search on "".
        ignore_last_word = arg != " " && scs[end] == ' '
        prefix = ignore_last_word ? "" : join(ex.args)

        # Also try looking into the env path if the user wants to complete the first argument
        use_envpath = !ignore_last_word && length(args.args) < 2

        return complete_path(prefix, pos, use_envpath=use_envpath, shell_escape=true)
    elseif isexpr(arg, :incomplete) || isexpr(arg, :error)
        partial = scs[last_parse]
        ret, range = completions(partial, lastindex(partial))
        range = range .+ (first(last_parse) - 1)
        return ret, range, true
    end
    return Completion[], 0:-1, false
end

end # module
