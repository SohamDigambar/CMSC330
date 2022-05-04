def fib(n)
    if n == 0 then
        return []
    elsif n == 1 then 
        return [0]
    elsif n == 2 then 
        return [0, 1]
    end 

    arr = [0, 1]

    for i in 2..(n - 1)
        arr.push(arr[i - 1] + arr[i - 2])
    end 

    return arr
end

def isPalindrome(n)
    n = n.to_s

    return n == n.reverse
end

def nthmax(n, a)
    if n > a.length - 1 then
        return nil
    end

    a = a.sort
    a = a.reverse
    return a[n]

end

def freq(s)
    if s.length == 0 then 
        return ""
    end

    freq = {}

    for i in 0..(s.length - 1)
        if freq.has_key?(s[i]) then
            freq[s[i]] += 1
        else 
            freq[s[i]] = 0 
        end
    end
    
    return freq.key(freq.values.max)
end


def zipHash(arr1, arr2)
    if arr1.length != arr2.length then
        return nil
    end 

    hash = {}

    for i in 0..(arr1.length - 1)
        hash[arr1[i]] = arr2[i]
    end

    return hash
end

def hashToArray(hash)
    arr = []

    hash.each do |key, value|
        temp = [key, value]
        arr.push(temp)
    end

    return arr
end