using FuzzyCompletions, Test

# simple tests
comps(s, m = Main) = completion_text.(completions(s, lastindex(s), m)[1])
function comp(s, m = Main)
    cs = comps(s, m)
    @test length(cs) ≥ 1
    return cs[1]
end

@testset "KeywordCompletion" begin
@test comp("begin") == "begin"
@test comp("beg") == "begin"
@test comp("being") == "begin"
@test "begin" ∉ comps("abegin") # strict first character match
end

@testset "PathCompletion" begin
let cwd = pwd()
    cd(dirname(pathof(FuzzyCompletions)))
    @test comps("\"")[1] == "FuzzyCompletions.jl\""
    @test comps("\"f")[1] == "FuzzyCompletions.jl\""
    cd(cwd)
end
end

@testset "ModuleCompletion" begin
@test comp("sin") == "sin"
@test "sin" in comps("sn")
@test "sin" in comps("sinn")
end

@testset "ModuleCompletion (in a context module)" begin
@test comp("fuzzy", FuzzyCompletions) == "fuzzyscore"
@test comp("fuzzyscore", FuzzyCompletions) == "fuzzyscore"
@test "fuzzyscore" in comps("fuzysore", FuzzyCompletions)
@test "fuzzyscore" in comps("FuzzyScore", FuzzyCompletions)
end

@testset "PackageCompletion" begin
@test comp("using REPL") == "REPL"
@test comp("using REP") == "REPL"
@test comp("using repl") == "REPL"
@test comp("using RPLE") == "REPL"
end

@testset "PropertyCompletion" begin
let m = Core.eval(@__MODULE__, :(
    module $(gensym(:FuzzyCompletionsTest))
        r = r"regex"
    end
    ))

    @test "pattern" in comps("r.pattern", m)
    @test "pattern" in comps("r.patternn", m)
    @test "pattern" in comps("r.ptn", m)
        @test "pattern" in comps("r.", m)
end
end

@testset "FieldCompletion" begin
@test "offset" in comps("split(\"\", ' ')[1].")
@test "offset" in comps("split(\"\", ' ')[1].offset")
@test "offset" in comps("split(\"\", ' ')[1].offsett")
@test "offset" in comps("split(\"\", ' ')[1].ofst")
@test "offset" in comps("split(\"\", ' ')[1].")
end

# NOTE:
# `FuzzyCompletions.MethodCompletion` is generated in a exactly same way as `REPLCompletion.MethodCompletion`
# so, don't test against them

@testset "DictCompletion" begin
let m = Core.eval(@__MODULE__, :(
    module $(gensym(:FuzzyCompletionsTest))
        d = Dict(:a => 1, :abc => 2)
    end
    ))

    @test ":a" == comp("d[:a", m)
    @test ":abc" == comp("d[:abc", m)
    @test ":abc" == comp("d[:ab", m)
    @test ":abc" == comp("d[:ac", m)
    @test ":a" == comp("d[:ad", m) # not empty
end
end
