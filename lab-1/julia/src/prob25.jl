module Prob25

export prob25

function prob25(ndigits)
    a, b = BigInt(1), BigInt(1)
    index = 2
    while length(string(b)) < ndigits
        a, b = b, a + b
        index += 1
    end
    return index
end

end
