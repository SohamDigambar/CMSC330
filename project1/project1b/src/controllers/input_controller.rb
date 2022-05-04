require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    #checking for invalid file argument
    return nil unless File.file?(path)

    gameboard = GameBoard.new(10, 10)

    #reading line and using RegEx to string match, and then adding ship to game board
    num_ships = 0
    read_file_lines(path) { |curr_line|
        if (curr_line =~ /^\((\d+),(\d+)\), (Up|Down|Left|Right), ([1-5])$/) then
            start_pos_row, start_pos_col = $1.to_i, $2.to_i 
            orientation = $3
            size = $4.to_i
    
            if num_ships == 5 then break end
    
            start_pos = Position.new(start_pos_row, start_pos_col)
            ship = Ship.new(start_pos, orientation, size)
            
            if gameboard.add_ship(ship) then num_ships += 1 end
        end
    }

    #checking if there are a valid number of ships
    return nil unless num_ships == 5

    return gameboard
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    #checking for invalid file argument
    return nil unless File.file?(path)

    #reading attack file with regex
    arr = []
    read_file_lines(path) { |curr_line|
        if (curr_line =~ /^\((\d+),(\d+)\)$/) then
            arr.append(Position.new($1.to_i, $2.to_i))
        end
    }

    return arr
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
