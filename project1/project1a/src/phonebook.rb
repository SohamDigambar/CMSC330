class PhoneBook
    def initialize
        @phonebook_listed = {}
        @phonebook_unlisted = {}
    end

    def add(name, number, is_listed)
        arr = number.split("-")

        if arr[0].length != 3 or arr[1].length != 3 or arr[2].length != 4 then 
            return false
        end

        if @phonebook_unlisted.has_key?(name) or @phonebook_listed.has_key?(name) then
            return false
        end

        if is_listed == true then
            if @phonebook_listed.has_value?(number) then
                return false
            end

            @phonebook_listed[name] = number
        else   
            @phonebook_unlisted[name] = number
        end

        return true

    end

    def lookup(name)
        if @phonebook_listed.keys.include?(name) == true then
            return @phonebook_listed[name]
        elsif @phonebook_listed.keys.include?(name) == true then 
            return @phonebook_unlisted[name]
        else
            return nil
        end
    end

    def lookupByNum(number)
        if @phonebook_listed.values.include?(number) == true then
            return @phonebook_listed.key(number)
        elsif @phonebook_listed.values.include?(number) == true then 
            return @phonebook_unlisted.key(number)
        else
            return nil
        end 
    end

    def namesByAc(areacode)
        arr = []

        @phonebook_listed.each do |contact_name, contact_number|
            if contact_number[0..2] == areacode then 
                arr.append(contact_name)
            end
        end

        @phonebook_unlisted.each do |contact_name, contact_number|
            if contact_number[0..2] == areacode then 
                arr.append(contact_name)
            end
        end

        return arr
    end
end