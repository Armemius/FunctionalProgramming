using Test
using Labwork

@testset "Project Euler Tests" begin
    @test Labwork.Prob5.prob5(10) == 2520
    @test Labwork.Prob5.prob5(20) == 232792560
    @test Labwork.Prob25.prob25(3) == 12
    @test Labwork.Prob25.prob25(1000) == 4782
end
