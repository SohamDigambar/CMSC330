class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    # .  --> empty cell
    # A --> attacked cell
    # S --> ship cell
    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @gameboard = Array.new(max_column){Array.new(max_row, ". ")}
        @num_successful_attacks = 0
        @total_ship_cells = 0
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        ship_row = ship.start_position.row - 1
        ship_col = ship.start_position.column - 1
        ship_dir = ship.orientation
        ship_size = ship.size

        #checking if start position is valid
        if (ship_row < 0 or ship_col < 0) or (ship_row >= @max_row or ship_col >= @max_column) or 
            (ship_size < 1 or ship_size > 5) then 
            return false
        end

        #checking if position orientation is valid with respect to start position
        if ship_dir == "Up" then
            if ship_row - (ship_size - 1) < 0 then
                return false
            end
        elsif ship_dir == "Down" then
            if ship_row + (ship_size - 1) >= @max_row then
                return false
            end 
        elsif ship_dir == "Left" then
            if ship_col - (ship_size - 1) < 0 then
                return false
            end
        elsif ship_dir == "Right" then
            if ship_col + (ship_size - 1) >= @max_column then
                return false
            end 
        end

        #checking to see if any other ships are in this ship's range on gameboard and then placing ship
        while ship_size > 0
            if @gameboard[ship_row][ship_col] == "S " then
                return false
            end

            @gameboard[ship_row][ship_col] = "S "
            @total_ship_cells += 1

            if ship_dir == "Up" then
                ship_row -= 1
            elsif ship_dir == "Down" then
                ship_row += 1
            elsif ship_dir == "Left" then
                ship_col -= 1
            elsif ship_dir == "Right" then
                ship_col += 1
            end

            ship_size -= 1
        end

        return true
    end

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        pos_row = position.row - 1
        pos_col = position.column - 1

        #checking invalid input
        if (pos_row < 0 or pos_col < 0) or (pos_row >= @max_column or pos_col >= @max_row) then
            return nil
        end

        #if it hits a ship cell
        if @gameboard[pos_row][pos_col] == "S " then 
            @num_successful_attacks += 1
            @gameboard[pos_row][pos_col] = "SA "
        elsif @gameboard[pos_row][pos_col] == "SA " then
            return false
        else 
            @gameboard[pos_row][pos_col] = "A " 
            return false
        end

        return true
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @num_successful_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        if @total_ship_cells > 0 then
            return @num_successful_attacks == @total_ship_cells
        else
            return false
        end
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        for i in 0..@max_row - 1
            for j in 0..@max_column - 1
                print(@gameboard[i][j])
            end
            print("\n")
        end
    end

end

# GENERAL TESTS

# gameboard = GameBoard.new(10, 10)
# print("\nBefore adding...\n")
# gameboard.to_s

# print("\n After adding...\n")

# ship_pos2 = Position.new(8, 3)
# ship2 = Ship.new(ship_pos2, "DOWN", 4)
# gameboard.add_ship(ship2)

# gameboard.to_s

# print("\n Attack 1...\n")

# if gameboard.attack_pos(Position.new(4, 4)) == true then
#     print("\nsuccessful attacks is: #{gameboard.num_successful_attacks}\n")
#     print("total ship cells: #{gameboard.total_ship_cells}\n")
# end
# gameboard.to_s

# print("\n Attack 2...\n")
# if gameboard.attack_pos(Position.new(5, 4)) == true then
#     print("\nsuccessful attacks is: #{gameboard.num_successful_attacks}\n")
#     print("total ship cells: #{gameboard.total_ship_cells}\n")
# end
# gameboard.to_s

# print("\n Attack 3...\n")
# if gameboard.attack_pos(Position.new(6, 4)) == true then
#     print("\nsuccessful attacks is: #{gameboard.num_successful_attacks}\n")
#     print("total ship cells: #{gameboard.total_ship_cells}\n")
# end
# gameboard.to_s

# print("\n Attack 4...\n")
# if gameboard.attack_pos(Position.new(6, 4)) == true then
#     print("\nsuccessful attacks is: #{gameboard.num_successful_attacks}\n")
#     print("total ship cells: #{gameboard.total_ship_cells}\n")
# end
# gameboard.to_s

# if gameboard.all_sunk? then 
#     puts "\n***GAME OVER***\n" 
# end

######################################################################################
# PUBLIC TESTS

# p1_ships = []
# p1_perf_atk = []
# p2_ships = []
# p2_perf_atk = []
# for i, size in [1,2,3,4].zip([4,5,3,2])
#     pos0 = Position.new(i, i)
#     pos1 = Position.new(i + 4, i + 4)
#     p1_ships << Ship.new(pos0, "Right", size)
#     p2_ships << Ship.new(pos1, "Right", size)
#     for j in 0..(size - 1)
#         p2_perf_atk << Position.new(i, i + j)
#         p1_perf_atk << Position.new(i + 4, i + j + 4)
#     end
# end

# test_board = GameBoard.new 10, 10

# puts "p1 ships are:\n"
# puts p1_ships
# puts "\n"
# puts "p2 ships are:\n"
# puts p2_ships
# puts "\n"

# # Property: A ship can be added in the bounds on an empty game_board 
# sngl_test_ret = test_board.add_ship(p1_ships[0])
# if sngl_test_ret == true then 
#     puts "Ship 1 was added :)\n"
# else 
#     puts "SHIP 1 WAS NOT ADDED\n"
# end

# puts "\n"

# counter = 2
# for shp in p1_ships[1..] 
#     add_shp_ret = test_board.add_ship(shp)
#     if sngl_test_ret == true then 
#         puts "Ship #{counter} was added :)\n"
#     else 
#         puts "SHIP #{counter} WAS NOT ADDED\n"
#     end
#     counter += 1
# end

# puts "\n"

# # Property: A ship will be hit if attacked
# for i in p2_perf_atk
#     if test_board.attack_pos(i) == false then
#         puts "PERFECT ATTACK SHOULD HAVE HIT BUT DID NOT\n"
#     end
# end

# puts "\n"

# # Property: Nothing will change for a miss
# if test_board.attack_pos(Position.new(2, 1)) == true then
#     puts "ATTACK SHOULD HAVE MISSED BUT ACTUALLY HIT\n" 
# end

# test_board.to_s