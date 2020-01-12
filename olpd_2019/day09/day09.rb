#!/usr/bin/ruby

require 'test/unit'

OP_ADD = 1
OP_MUL = 2
OP_INPUT = 3
OP_OUTPUT = 4
OP_JUMP_IF_TRUE = 5
OP_JUMP_IF_FALSE = 6
OP_LESS_THAN = 7
OP_EQUALS = 8
OP_ADJ_RELBASE = 9
OP_END = 99

MODE_POS = 0
MODE_IMM = 1
MODE_REL = 2

class Intcode

  attr_reader :prog
  attr_reader :input_pending
  attr_accessor :input
  attr_reader :output_pending
  attr_reader :output
  attr_reader :last_opcode

  def init()
    @pc = 0
    @relbase = 0
    @last_opcode = 0

    @input_pending = false
    @input = 0

    @output_pending = false
    @output = 0
  end

  def initialize()
    init()
  end

  def load_from_file(filename)
    init()
    @prog = File.read(filename).split(/,/)
              .map.with_index {|x, i| [i, x.to_i]}.to_h
  end

  def read(op, m)
    if m == MODE_POS
      return @prog[op] || 0
    elsif m == MODE_IMM
      return op
    elsif m == MODE_REL
      return @prog[op + @relbase] || 0
    else
      raise "Invalid mode #{m}"
    end
  end

  def write(op, m, val)
    if m == MODE_POS
      @prog[op] = val
    elsif m == MODE_REL
      @prog[op + @relbase] = val
    else
      raise "Invalid mode #{m}"
    end
  end

  def execute()
    while true
      op0 = @prog[@pc] % 100
      @last_opcode = op0

      # Modifiers
      m1 = (@prog[@pc] / 100) % 10
      m2 = (@prog[@pc] / 1000) % 10
      m3 = (@prog[@pc] / 10000) % 10

      # Operands
      op1 = @prog[@pc + 1]
      op2 = @prog[@pc + 2]
      op3 = @prog[@pc + 3]

      # Emulator loop
      case op0
      when OP_ADD
        a = read(op1, m1)
        b = read(op2, m2)
        write(op3, m3, a + b)
        @pc += 4
      when OP_MUL
        a = read(op1, m1)
        b = read(op2, m2)
        write(op3, m3, a * b)
        @pc += 4
      when OP_JUMP_IF_TRUE
        @pc = (read(op1, m1) != 0) ? read(op2, m2) : (@pc + 3)
      when OP_JUMP_IF_FALSE
        @pc = (read(op1, m1) == 0) ? read(op2, m2) : (@pc + 3)
      when OP_LESS_THAN
        a = read(op1, m1)
        b = read(op2, m2)
        write(op3, m3, (a < b) ? 1 : 0)
        @pc += 4
      when OP_EQUALS
        a = read(op1, m1)
        b = read(op2, m2)
        write(op3, m3, (a == b) ? 1 : 0)
        @pc += 4
      when OP_ADJ_RELBASE
        @relbase += read(op1, m1)
        @pc += 2
      when OP_INPUT
        if @input_pending
          # Caller has provided input
          write(op1, m1, @input)
          @input_pending = false
          @pc += 2
        else
          # Return to caller for input
          @input_pending = true
          return
        end
      when OP_OUTPUT
        if @output_pending
          # Caller has handled output
          @output_pending = false
          @pc += 2
        else
          # Return to caller to handle output
          @output = read(op1, m1)
          @output_pending = true
          return
        end
      when OP_END
        return
      else
        raise
      end
    end
  end
end

def day09(input)
  ic = Intcode.new()
  ic.load_from_file("../inputs/input09.txt")
  while true
    ic.execute()
    case ic.last_opcode
    when OP_END
      break
    when OP_INPUT
      ic.input = input
    when OP_OUTPUT
      if ic.output != 0
        return ic.output
      end
    end
  end
end

class Day09 < Test::Unit::TestCase
  def test_part1()
    assert_equal(2594708277, day09(1))
  end

  def test_part2()
    assert_equal(87721, day09(2))
  end
end
