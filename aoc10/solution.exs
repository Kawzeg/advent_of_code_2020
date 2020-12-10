import Enum

defmodule Solution do
  # Part 1
  def part1(input) do
    xs = input
    |> map(fn x -> elem(Integer.parse(x), 0) end)
    |> sort
    {x, y} = [0|xs]
    |> chunk_every(2, 1, [max(xs)+3])
    |> map(fn [a,b]->b-a end)
    |> split_with(fn a -> a == 3 end)
    length(x) * length(y)
  end

  def arrgs([a,b]) do
    1
  end

  # Takes a sorted list, so a < b
  def arrgs([a,b,c,d|tail]) when (a + 3 >= d) do
    arrgs([d|tail]) + arrgs([b,c,d|tail]) + arrgs([c,d|tail])
  end

  def arrgs([a,b,c|tail]) when (a + 3 >= c) do
    arrgs([b,c|tail]) + arrgs([c|tail])
  end

  def arrgs([a,b|tail]) when (a + 3 >= b) do
    arrgs([b|tail])
  end

  def arrgs(xs) do
    xs
  end
end
