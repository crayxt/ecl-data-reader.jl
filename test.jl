# C:\apps\Julia_1.5.0\bin\julia.exe

include("Ecl.jl")
#
#julia> @time a = EclFile("C:\\Test\\NORNE_ATW2013.SMSPEC");
#  0.037358 seconds (197.04 k allocations: 12.778 MiB)
#
#julia> @time a = EclFile("C:\\Test\\NORNE_ATW2013.UNSMRY");
#  1.472356 seconds (7.19 M allocations: 326.285 MiB, 25.26% gc time)
#

using Dates

function timedelta(delta, origin)
    # delta in days
    Dates.DateTime(origin) + Dates.Second(delta * 24 * 60 * 60)
end

function to_datetime(arr, origin)
    orig = Dates.DateTime(origin)
    #tmp = Array{Dates.DateTime}
    tmp = Dates.DateTime[]
    for delta in arr
        push!(tmp, orig + Dates.Second(delta * 24 * 60 * 60))
    end
    return tmp
end

#times = df["TIME::+:+:+:+"]
#
#[timedelta(i, "1997-11-06") for i in times]

insertcols!(df, 1, :DATETIME => to_datetime(df["TIME::+:+:+:+"], "1997-11-06") )
#to_datetime(df["TIME::+:+:+:+"], "1997-11-06")