# this is a comment
defmodule Math do
  @pi 3.14159

  def add(a, b) do
    a + b
  end
end

result = Math.add(5, 3)
IO.puts("Result: #{result}")
