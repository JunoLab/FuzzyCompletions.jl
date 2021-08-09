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

struct KeywordCompletion <: Completion
    keyword::String
    score::Float64
end
KeywordCompletion(keyword::String) = KeywordCompletion(keyword, Inf)
KeywordCompletion(keyword::String, needle) = KeywordCompletion(keyword, fuzzyscore(needle, keyword))

struct PathCompletion <: Completion
    path::String
    score::Float64
end
PathCompletion(path::String) = PathCompletion(path, Inf)
PathCompletion(path::String, needle) = PathCompletion(path, fuzzyscore(needle, path))

struct ModuleCompletion <: Completion
    parent::Module
    mod::String
    score::Float64
end
ModuleCompletion(parent::Module, mod::String) = ModuleCompletion(parent, mod, Inf)
ModuleCompletion(parent::Module, mod::String, needle) = ModuleCompletion(parent, mod, fuzzyscore(needle, mod))

struct PackageCompletion <: Completion
    package::String
    score::Float64
end
PackageCompletion(package::String) = PackageCompletion(package, Inf)
PackageCompletion(package::String, needle) = PackageCompletion(package, fuzzyscore(needle, package))

struct PropertyCompletion <: Completion
    value
    property::Symbol
    score::Float64
end
PropertyCompletion(value, property::Symbol) = PropertyCompletion(value, property, Inf)
PropertyCompletion(value, property::Symbol, needle) = PropertyCompletion(value, property, fuzzyscore(needle, property))

struct FieldCompletion <: Completion
    typ::DataType
    field::Symbol
    score::Float64
end
FieldCompletion(typ::DataType, field::Symbol) = FieldCompletion(typ, field, Inf)
FieldCompletion(typ::DataType, field::Symbol, needle) = FieldCompletion(typ, field, fuzzyscore(needle, field))

# NOTE: no fuzzyness is needed for this
struct MethodCompletion <: Completion
    func
    input_types::Type
    method::Method
    orig_method::Union{Nothing,Method}
end

struct BslashCompletion <: Completion
    bslash::String
    score::Float64
end
BslashCompletion(bslash::String) = BslashCompletion(bslash, Inf)
BslashCompletion(bslash::String, needle) = BslashCompletion(bslash, fuzzyscore(needle, bslash))

struct ShellCompletion <: Completion
    text::String
end

struct DictCompletion <: Completion
    dict::AbstractDict
    key::String
    score::Float64
end
DictCompletion(dict::AbstractDict, key::String) = DictCompletion(dict, key, Inf)
DictCompletion(dict::AbstractDict, key::String, needle) = DictCompletion(dict, key, fuzzyscore(needle, key))

# interface definition
function Base.getproperty(c::Completion, name::Symbol)
    if name === :keyword
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

_completion_text(c::KeywordCompletion) = c.keyword
_completion_text(c::PathCompletion) = c.path
_completion_text(c::ModuleCompletion) = c.mod
_completion_text(c::PackageCompletion) = c.package
_completion_text(c::PropertyCompletion) = string(c.property)
_completion_text(c::FieldCompletion) = string(c.field)
_completion_text(c::MethodCompletion) = sprint(io -> show(io, isnothing(c.orig_method) ? c.method : c.orig_method::Method))
_completion_text(c::BslashCompletion) = c.bslash
_completion_text(c::ShellCompletion) = c.text
_completion_text(c::DictCompletion) = c.key

completion_text(c) = _completion_text(c)::String

const Completions = Tuple{Vector{Completion}, UnitRange{Int}, Bool}

score(c::Completion) = c.score
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
function complete_symbol(sym, ffunc, context_module=Main)::Vector{Completion}
    mod = context_module
    name = sym

    lookup_module = true
    t = Union{}
    val = nothing
    if something(findlast(in(non_identifier_chars), sym), 0) < something(findlast(isequal('.'), sym), 0)
        # Find module
        lookup_name, name = rsplit(sym, ".", limit=2)

        ex = Meta.parse(lookup_name, raise=false, depwarn=false)

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
    should_method_complete,
    find_start_brace,
    get_value,
    get_type

# Method completion on function call expression that look like :(max(1))
function complete_methods(ex_org::Expr, context_module::Module=Main)
    func, found = get_value(ex_org.args[1], context_module)::Tuple{Any,Bool}
    !found && return Completion[]

    args_ex, kwargs_ex = complete_methods_args(ex_org.args[2:end], ex_org, context_module, true, true)

    out = Completion[]
    complete_methods!(out, func, args_ex, kwargs_ex)
    return out
end

function complete_any_methods(ex_org::Expr, callee_module::Module, context_module::Module, moreargs::Bool)
    out = Completion[]
    args_ex, kwargs_ex = try
        complete_methods_args(ex_org.args[2:end], ex_org, context_module, false, false)
    catch
        return out
    end

    for name in names(callee_module; all=true)
        if !Base.isdeprecated(callee_module, name) && isdefined(callee_module, name)
            func = getfield(callee_module, name)
            if !isa(func, Module)
                complete_methods!(out, func, args_ex, kwargs_ex, moreargs)
            elseif callee_module === Main::Module && isa(func, Module)
                callee_module2 = func
                for name in names(callee_module2)
                    if isdefined(callee_module2, name)
                        func = getfield(callee_module, name)
                        if !isa(func, Module)
                            complete_methods!(out, func, args_ex, kwargs_ex, moreargs)
                        end
                    end
                end
            end
        end
    end

    return out
end

function complete_methods_args(funargs::Vector{Any}, ex_org::Expr, context_module::Module, default_any::Bool, allow_broadcasting::Bool)
    args_ex = Any[]
    kwargs_ex = Pair{Symbol,Any}[]
    if allow_broadcasting && ex_org.head === :. && ex_org.args[2] isa Expr
        # handle broadcasting, but only handle number of arguments instead of
        # argument types
        for _ in (ex_org.args[2]::Expr).args
            push!(args_ex, Any)
        end
    else
        for ex in funargs
            if isexpr(ex, :parameters)
                for x in ex.args
                    n, v = isexpr(x, :kw) ? (x.args...,) : (x, x)
                    push!(kwargs_ex, n => get_type(get_type(v, context_module)..., default_any))
                end
            elseif isexpr(ex, :kw)
                n, v = (ex.args...,)
                push!(kwargs_ex, n => get_type(get_type(v, context_module)..., default_any))
            else
                push!(args_ex, get_type(get_type(ex, context_module)..., default_any))
            end
        end
    end
    return args_ex, kwargs_ex
end

function complete_methods!(out::Vector{Completion}, @nospecialize(func), args_ex::Vector{Any}, kwargs_ex::Vector{Pair{Symbol,Any}}, moreargs::Bool=true)
    ml = methods(func)
    # Input types and number of arguments
    if isempty(kwargs_ex)
        t_in = Tuple{Core.Typeof(func), args_ex...}
        na = length(t_in.parameters)::Int
        orig_ml = fill(nothing, length(ml))
    else
        isdefined(ml.mt, :kwsorter) || return out # kwargs given, but no kwarg method found
        kwfunc = ml.mt.kwsorter
        kwargt = NamedTuple{(first.(kwargs_ex)...,), Tuple{last.(kwargs_ex)...}}
        t_in = Tuple{Core.Typeof(kwfunc), kwargt, Core.Typeof(func), args_ex...}
        na = length(t_in.parameters)::Int
        orig_ml = ml
        ml = methods(kwfunc)
        func = kwfunc
    end
    if !moreargs
        na = typemax(Int)
    end

    for (method::Method, orig_method) in zip(ml, orig_ml)
        ms = method.sig

        # Check if the method's type signature intersects the input types
        if typeintersect(Base.rewrap_unionall(Tuple{(Base.unwrap_unionall(ms)::DataType).parameters[1 : min(na, end)]...}, ms), t_in) != Union{}
            push!(out, MethodCompletion(func, t_in, method, orig_method))
        end
    end
end

import REPL.REPLCompletions:
    latex_symbols,
    emoji_symbols,
    non_identifier_chars,
    whitespace_chars,
    bslash_separators,
    subscripts,
    subscript_regex,
    superscripts,
    superscript_regex,
    afterusing

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

function dict_identifier_key(str, tag, context_module = Main)
    if tag === :string
        str_close = str*"\""
    elseif tag === :cmd
        str_close = str*"`"
    else
        str_close = str
    end

    frange, end_of_identifier = find_start_brace(str_close, c_start='[', c_end=']')
    isempty(frange) && return (nothing, nothing, nothing)
    obj = context_module
    for name in split(str[frange[1]:end_of_identifier], '.')
        Base.isidentifier(name) || return (nothing, nothing, nothing)
        sym = Symbol(name)
        isdefined(obj, sym) || return (nothing, nothing, nothing)
        obj = getfield(obj, sym)
    end
    (isa(obj, AbstractDict) && length(obj)::Int < 1_000_000) || return (nothing, nothing, nothing)
    begin_of_key = something(findnext(!isspace, str, nextind(str, end_of_identifier) + 1), # +1 for [
                             lastindex(str)+1)
    return (obj::AbstractDict, str[begin_of_key:end], begin_of_key)
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

function completions(string, pos, context_module = Main)
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
            return complete_any_methods(ex_org, callee_module::Module, context_module, moreargs), (0:length(rexm.captures[1])+1) .+ rexm.offset, false
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

        if inc_tag === :string &&
           length(paths) == 1 &&  # Only close if there's a single choice,
           !isdir(expanduser(replace(string[startpos:prevind(string, first(r))] * paths[1].path,
                                     r"\\ " => " "))) &&  # except if it's a directory
           (lastindex(string) <= pos ||
            string[nextind(string,pos)] != '"')  # or there's already a " at the cursor.
            paths[1] = PathCompletion(paths[1].path * "\"")
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
    if inc_tag === :other && should_method_complete(partial)
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
    # The case where dot and start pos is equal could look like: "(""*"").d","". or  CompletionFoo.test_y_array[1].y
    # This case can be handled by finding the beginning of the expression. This is done below.
    if dotpos == startpos
        i = prevind(string, startpos)
        while 0 < i
            c = string[i]
            if c in [')', ']']
                if c==')'
                    c_start='('; c_end=')'
                elseif c==']'
                    c_start='['; c_end=']'
                end
                frange, end_of_identifier = find_start_brace(string[1:prevind(string, i)], c_start=c_start, c_end=c_end)
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
    return sort_suggestions!(suggestions), (dotpos+1):pos, false
end

@inline sort_suggestions!(suggestions) = sort!(suggestions, by=score, rev=true)

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
