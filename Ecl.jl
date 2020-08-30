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
        dim = readlong(file)::Int32
        #println("DBG : Dimension: $dim")
        dtype = readstring(file, 4)
        #println("DBG : Data type: $dtype")
        # endianness sign
        endi2 = readlong(file)::Int32
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
        counter::Int64 = 1
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

function fdescribe(k::EclFile, name::String="")
   # List sections of EclFile in form: Keyword, Data Type, Dimension.
   # If name is specified, filter sections with kwd == `name`.
   println("INFO: Sections of file: $(k.filename)")
   if name == ""
       data = k.data
   else
       data = [ i for i in k.data if i.kwd == name ]
   end
   for s in data
      println(rpad(s.kwd, 10), rpad(s.dtype, 10), lpad(s.dim, 12))
   end
end

function fcount(k::EclFile, name::String="")
    # Count number of sections named `name`, for example "PARAMS"
    # If name is not specified, count all sections.
    if name == ""
        data = k.data
    else
        data = [ i for i in k.data if i.kwd == name ]
    end
    return length(data);
end

function fwidth(k::EclFile)
    for s in k.data
        if s.kwd == "PARAMS"
            return length(s.data);
        end
    end
    return 0;
end

function fget(k::EclFile, section::String)
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

function grep(str::Union{String,Regex}, data; exact::Bool=false, invert::Bool=false)
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

function fdf(k::EclFile, l::EclFile, kwd::String, exact::Bool=false)
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

function read_results(smspec_file::String, kwd::String)
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

function read_all_results(smspec_file::String, mask::Regex)
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
   unique_kwds = unique(vecs.data[kwd_idx])
   time_idx = grep(r"^TIME$", vecs.data)
   reals = grep(r"^:+", objs.data, invert=true)
   real_kwd_idx = intersect(kwd_idx, reals)
   kwd_and_time = vcat(time_idx, real_kwd_idx)
   kwds = vecs.data[kwd_and_time]
   println("kwds: ", length(kwds))
   # Get all data combined row-wise from UNSMRY PARAMS sections.
   kwd_data = fdata(l, kwd_and_time)
   println("rows: ", size(kwd_data)[1])
   println("cols: ", size(kwd_data)[2])
   cols = objs.data[kwd_and_time]
   cols[1] = "TIME"
   # Get column names
   cols = objs.data[kwd_and_time]
   cols[1] = "TIME"
   dfs = []
   for vec in unique_kwds
       println("INFO: Collecting vector: $vec")
       vec_idx = grep(vec, kwds, exact=true)
       vec_idx = vcat(time_idx, vec_idx)
       #print(vec_idx)
       columns = cols[vec_idx]
       #print(col_idx)
       df_tmp = DataFrame(kwd_data[:,vec_idx], columns, makeunique=true)
       # Drop duplicated columns
       select!(df_tmp, unique(columns))
       df_tmp = stack(df_tmp, Not([:TIME]), variable_name="WELL", value_name=vec)
       push!(dfs, df_tmp)
   end
   println("INFO: Concatenating results...")
   df = outerjoin(dfs..., on=[:TIME, :WELL])
   return(df);
end

