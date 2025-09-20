module Prob5

export prob5

function prob5(n)
    result = 1
    for i in 1:n
        result = lcm(result, i)
    end
    return result
end

end
