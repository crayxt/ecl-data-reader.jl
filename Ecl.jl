# pseudocode by Baurzhan Muftakhidinov
# Read Eclipse sim software output formatted in Fortran Arrays
# seek(s, 0)

#using DataFrames

function readlong(file)
    return ntoh(read(file, Int32))
end
    
function readstring(file, num=8)
    # 4 chars to form a CHAR
    # 8 chars to form a STRING
    return strip(String(read(file, num)))
end

struct EclSection
    kwd::String
    dim::Int32
    dtype::String
    endi2::Int32
    data::Array{}
    function EclSection(file::IOStream)
        kwd = readstring(file)
        #println("DBG : Section found: $kwd")
        # Dimension, number of data points
        dim = readlong(file)
        #println("DBG : Dimension: $dim")
        dtype = readstring(file, 4)
        #println("DBG : Data type: $dtype")
        # endianness sign
        endi2 = readlong(file)
        if dtype == "CHAR" || dtype == "MESS"
            stp   = String
            nsize = 8
        elseif dtype == "INTE" || dtype == "LOGI"
            stp   = Int32
            nsize = 4
        elseif dtype == "DOUB"
            stp   = Float64
            nsize = 8
        elseif dtype == "REAL"
            stp   = Float32
            nsize = 4
        elseif startswith(dtype, "C0") # Very long strings
            stp   = String
            nsize = parse(Int32, dtype[2:end])
        else
            println("Unknown dtype", dtype)
            exit
        end
        data = Array{stp}(undef, dim)
        counter = 1
        while counter <= dim
            NBYTES = readlong(file)
            num = Int(NBYTES/nsize)
            if dtype == "CHAR" || dtype == "MESS" || startswith(dtype, "C0")
                #buf = String(read(file, NBYTES))
                buf = read(file, NBYTES)
                data[counter:counter+num-1] = strip.(String.([buf[(i-1)*nsize+1:i*nsize] for i in 1:num]))
            else
                data[counter:counter+num-1] = ntoh.(reinterpret(stp, read(file, NBYTES)))
            end
            counter += num
            NBYTES2 = readlong(file)  # extra endianness sign
        end
        new(kwd, dim, dtype, endi2, data);
    end
end

struct EclFile
    filename::String
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
        new(filename, data);
    end
end

function fdescribe(k::EclFile)
   # List sections of EclFile in form: Keyword, Data Type, Dimension.
   println("INFO: Sections of file: $(k.filename)")
   for s in k.data
      println(rpad(s.kwd, 10), rpad(s.dtype, 10), lpad(s.dim, 12))
   end
end

function flen(k::EclFile)
    counter = 0
    for s in k.data
        if s.kwd == "PARAMS"
            counter += 1
        end
    end
    return counter;
end

function fwidth(k::EclFile)
    for s in k.data
        if s.kwd == "PARAMS"
            return length(s.data);
        end
    end
    return 0;
end

function fget(k::EclFile, section)
    # Returns the first occurence of section by its name.
    for s in k.data
        if s.kwd == section
            return s;
        end
    end
end

function fstart(k::EclFile)
    # Returns the first occurence of section by its name.
    return join(fget(k, "STARTDAT").data[3:-1:1], "-");
end

function fdata(k::EclFile, idx::Array{Int64,1}=[])
    # Iteratively scans all data rows and collects items given by indexes.
    if length(idx) == 0
        idx = collect(1:fwidth(k))
    end
    return hcat([b.data[idx] for b in k.data if b.kwd == "PARAMS"]...)';
end

function fdata(k::EclFile, idx::UnitRange)
    idx = collect(idx)
    return fdata(k, idx);
end

function fdata(k::EclFile)
    return fdata(k, 1:fwidth(k));
end

function grep(str, data; exact::Bool=false, invert::Bool=false)
    # c = findall(x -> match(r"^W", x), fget(a, "KEYWORDS").data);
    if exact == true
        check = isequal
    else
        check = startswith
    end
    if invert == true
        return findall(x -> !check(x, str), data);
    else
        return findall(x -> check(x, str), data);
    end
end

function fdf(k, l, kwd, exact::Bool=false)
   # Build a DataFrame.
   # k - SMSPEC, l - UMSMRY, kwd like "WBHP"
   vecs = fget(k, "KEYWORDS")
   objs = fget(k, "WGNAMES")
   if isnothing(objs)
      # Handle Intersect section name change.
      objs = fget(k, "NAMES")
   end
   kwd_idx = grep(kwd, vecs.data, exact=exact)
   time_idx = grep(r"^TIME$", vecs.data)
   reals = grep(r"^:+", objs.data, invert=true)
   real_kwd_idx = intersect(kwd_idx, reals)
   kwd_and_time = vcat(time_idx, real_kwd_idx)
   kwd_data = fdata(l, kwd_and_time)
   # Get column names
   cols = objs.data[kwd_and_time]
   cols[1] = "TIME"
   df = DataFrame(kwd_data, cols, makeunique=true)
   select!(df, unique(cols))
   return(df);
end

function read_results(smspec_file, kwd)
   # Full path without extension. Does not support multiple UNSMRY files yet.
   println("INFO: Reading results from file: $smspec_file")
   filemask    = splitext(smspec_file)[1]
   unmsry_file = filemask * ".UNSMRY"
   k = EclFile(smspec_file)
   println("INFO: Start date is: ", fstart(k))
   l = EclFile(unmsry_file)
   df = fdf(k, l, kwd)
   return(df);
end

function read_all_results(smspec_file, mask)
   # Full path without extension. Does not support multiple UNSMRY files yet.
   # mask is like r"^W"
   println("INFO: Reading all results from file: $smspec_file")
   filemask    = splitext(smspec_file)[1]
   unmsry_file = filemask * ".UNSMRY"
   k = EclFile(smspec_file)
   println("INFO: Start date is: ", fstart(k))
   l = EclFile(unmsry_file)
   vecs = fget(k, "KEYWORDS")
   objs = fget(k, "WGNAMES")
   if isnothing(objs)
      # Handle Intersect section name change.
      objs = fget(k, "NAMES")
   end
   kwd_idx = grep(mask, vecs.data)
   kwds = vecs.data[kwd_idx]
   dfs = []
   for kwd in unique(kwds)
       println("INFO: Reading vector $kwd")
       df_tmp = fdf(k, l, kwd)
       df_tmp = stack(df_tmp, Not([:TIME]), variable_name="WELL", value_name=kwd)
       push!(dfs, df_tmp)
   end
   println("INFO: Concatenating results...")
   df = outerjoin(dfs..., on=[:TIME, :WELL])
   return(df);
end

