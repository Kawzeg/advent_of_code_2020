defmodule Solution do
  use Memoize
  import Enum

  def part1(input) do
    input = input
    |> map(fn x -> elem(Integer.parse(x), 0) end)
    |> sort
    {x, y} = [0|input]
    |> chunk_every(2, 1, [max(input)+3])
    |> map(fn [a,b]->b-a end)
    |> split_with(fn a -> a == 3 end)
    length(x) * length(y)
  end

  def part2(input) do
    input = input |> map(fn x -> elem(Integer.parse(x), 0) end) |> sort
    arrgs([0|input]++[max(input)+3])
  end

  defmemo arrgs([_,_]) do
    1
  end

  # Takes a sorted list, so a < b
  defmemo arrgs([a,b,c,d|tail]) when (a + 3 >= d) do
    arrgs([d|tail]) + arrgs([b,c,d|tail]) + arrgs([c,d|tail])
  end

  defmemo arrgs([a,b,c|tail]) when (a + 3 >= c) do
    arrgs([b,c|tail]) + arrgs([c|tail])
  end

  defmemo arrgs([_|tail]) do
    arrgs(tail)
  end
end
