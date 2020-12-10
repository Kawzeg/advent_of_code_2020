defmodule SolutionTest do
  use ExUnit.Case
  doctest Solution

  test "Part 1" do
    IO.puts "Part 1"
    File.read!("input")
    |> String.split("\n", trim: true)
    |> Solution.part1
    |> IO.puts
  end
  test "Part 2" do
    IO.puts "Part 2"
    File.read!("input")
    |> String.split("\n", trim: true)
    |> Solution.part2
    |> IO.inspect
  end
end
