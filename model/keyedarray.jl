using StaticArrays

struct KeyedArray{D,V}
    vals::V
end

function keys2index(::KeyedArray{D}, keys) where D
    map(D, keys) do d, k
        i = findfirst(isequal(k), d)
        isnothing(i) && throw(ArgumentError("Key $k not found in dim $d"))
        i
    end
end

Base.getindex(ka::KeyedArray, keys...) = ka.vals[keys2index(ka, keys)...]
Base.setindex!(ka::KeyedArray, c, keys...) = setindex!(ka.vals, c, keys2index(ka, keys)...)
Base.setindex(ka::A, c, keys...) where A<:KeyedArray = A(
    setindex(ka.vals, c, keys2index(ka, keys)...)
)

# Pull up interface from StaticArrays
Base.iterate(ka::KeyedArray) = iterate(ka.vals)
Base.iterate(ka::KeyedArray, state) = iterate(ka.vals, state)
Base.length(ka::KeyedArray) = length(ka.vals)
Base.size(ka::KeyedArray) = size(ka.vals)
Base.size(ka::KeyedArray, d) = size(ka.vals, d)
Base.eltype(ka::KeyedArray) = eltype(ka.vals)
Base.ndims(ka::KeyedArray) = ndims(ka.vals)

function Base.zeros(::Type{KeyedArray{D,V}}) where {D, V}
    KeyedArray{D,V}(zeros(V))
end

function Base.ones(::Type{KeyedArray{D,V}}) where {D, V}
    KeyedArray{D,V}(ones(V))
end

function keyed_array_type(T::Type, D...; mutable=true)
    XArray = mutable ? MArray : SArray
    S = Tuple{length.(D)...}
    N = length(D)
    L = prod(length.(D))
    V = XArray{S,T,N,L}
    KeyedArray{D,V}
end
