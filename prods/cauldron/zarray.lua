-- from www.github.com/danielpower/zStarlocal zArray = {}

function zArray.set(array, x, y, value)
    if not array[x] then array[x] = {} end
    array[x][y] = value
end

function zArray.check(array, x, y)
    if array[x] then
        return(array[x][y])
    end
end

function zArray.length(array)
    local len = 0
    for x in pairs(array) do
        for y in pairs(array[x]) do
            if array[x][y] ~= nil then
                len = len + 1
            end
        end
    end
    return(len)
end

_zArray = zArray
return(zArray)

