

SIZE=4

class State
  attr_accessor :board, :sum
  def initialize(board, sum)
    @board=board
    @sum=sum
  end

end

def check_state(pos)
  history = [] #история всех состояний доски
  cur_states = []
  cur_states.push(pos)
  res = 0
  final_pos = ".brbbrbrrbrbbrbr"
  loop do
    next_states = Array.new()
    cur_states.each do |s|
      index = s.board.index(".")
      c_x, c_y = index%SIZE, index/SIZE
      if(s.board==final_pos)
        res+=s.sum
        return res
      end

      moves = ["L","R","U","D"]
      if(c_x>=SIZE-1)
        moves.delete("L")
      end
      if(c_x<=0)
        moves.delete("R")
      end
      if(c_y<=0)
        moves.delete("D")
      end
      if(c_y>=SIZE-1)
        moves.delete("U")
      end
      moves.each do |m|
        n_x, n_y =  c_x, c_y
        if(m=="L")
          n_x+=1
        elsif(m=="R")
          n_x-=1
        elsif(m=="D")
          n_y-=1
        elsif(m=="U")
          n_y+=1
        end
        m_board = s.board.dup
        m_board[c_y*SIZE+c_x], m_board[n_y*SIZE+n_x] =m_board[n_y*SIZE+n_x], m_board[c_y*SIZE+c_x]
        next if (history.include?(m_board))
        history.push(m_board)
        next_states.push(State.new(m_board, (s.sum*243+m.ord)%100_000_007))
      end

    end
    cur_states = next_states.dup
  end
  return res
end


p check_state(State.new(".rbbrrbbrrbbrrbb", 0))










