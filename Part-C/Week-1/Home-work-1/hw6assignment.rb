# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces.concat([rotations([[0, 0], [1, 0], [1, 1], [1, 0]]),
                                     [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # long (only needs two)
                                      [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
                                     rotations(
                                         [[0, 0], [-1, 0], [1, 0], [0, 1], [-1, 1]]
  )])
  Cheat_Piece = [[0,0]]
  # your enhancements here
  def initialize (p, b)
    super(p, b)
  end

  def self.next_piece (board)
    puts(board.cheat)
    if board.cheat
      board.cheat_failing = true
      board.cheat = false
      piece = Cheat_Piece
    else
      piece = All_My_Pieces.sample
    end
    MyPiece.new(piece , board)
  end
end

class MyBoard < Board
  # your enhancements here
  attr_accessor :cheat, :cheat_failing
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @score = 0
    @game = game
    @delay = 500
    @current_block = MyPiece.next_piece(self)
    @cheat = false
    @cheat_failing = false
  end

  def cheat
    @cheat
  end

  def nextcheat
    if @score > 100 and @cheat.equal?(false)
      @cheat = true
      @score -= 100
    end
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  def store_current
    if @cheat_failing
      remove_filled
      @cheat_failing = false
    else
      super
    end
  end
end

class MyTetris < Tetris

  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super()
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.nextcheat})
  end
end


