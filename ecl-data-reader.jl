# pseudocode by Baurzhan Muftakhidinov
# Read Eclipse sim software output formatted in Fortran Arrays
# seek(s, 0)

function readlong(file)
    return ntoh(read(file, Int32))
end
    
function readdouble(file)
    return ntoh(read(file, Float64))
end

function readreal(file)
    return ntoh(read(file, Float32))
end

function readchar(file)
    # 4 chars to form a CHAR
    return strip(String(read(file, 4)))
end
    
function readstring(file)
    # 8 chars to form a STRING
    return strip(String(read(file, 8)))
end

struct EclSection
    kwd::String
    dim::Int32
    dtype::String
    endi2::Int32
    data::Vector{}
    function EclSection(file::IOStream)
        kwd = readstring(file)
        #println("DBG : Section found: $kwd")
        # Dimension, number of data points
        dim = readlong(file)
        dtype = readchar(file)
        # endianness sign
        endi2 = readlong(file)
        data = []
        while length(data) < dim
            tmp = []
            NBYTES = readlong(file)
            if dtype == "CHAR"
                num = Int(NBYTES/8)
                fun = readstring
            elseif dtype == "INTE"
                num = Int(NBYTES/4)
                fun = readlong
            elseif dtype == "DOUB"
                num = Int(NBYTES/8)
                fun = readdouble
            elseif dtype == "REAL"
                num = Int(NBYTES/4)
                fun = readreal
            elseif dtype == "LOGI"
                num = Int(NBYTES/4)
                fun = readlong
            else
                println("Unknown dtype", dtype)
                exit
            end
            for i in 1:num
                push!(tmp, fun(file))
            end
            NBYTES2 = readlong(file)  # extra endianness sign
            append!(data, tmp)
        end
        new(kwd, dim, dtype, endi2, data)
    end
end

struct EclFile
    data::Vector{EclSection}
    function EclFile(filename::String)
        s = open(filename, "r")
        data = EclSection[]
        
        while ! eof(s)
            readlong(s)  #endianness sign
            push!(data, EclSection(s))
        end

        #readlong(s)
        close(s)
        new(data)
    end
end

#a = EclFile("NORNE/NORNE_ATW2013.SMSPEC")
#b = EclFile("NORNE/NORNE_ATW2013.UNSMRY")
#c = EclFile("NORNE_ATW2013.UNRST")
#println(c.data[23]) #.data[390:400])

