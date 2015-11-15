-- Name: unpack.lua

function unpack2(...)
    local arg = {...}
    local t = {}
    for i,v in ipairs(arg) do
        if type(v) == "table" then
            for i2,v2 in ipairs(v) do
                table.insert(t, v2)
            end
        else
            table.insert(t, v)
        end
    end
    return table.unpack(t)
end
