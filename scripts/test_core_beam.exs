#!/usr/bin/env elixir
# Test Core Erlang compilation to BEAM

dir = "/tmp/nova_core"
files = File.ls!(dir) |> Enum.filter(&String.ends_with?(&1, ".core")) |> Enum.sort()

results = for f <- files do
  path = Path.join(dir, f)
  source = File.read!(path)

  case :core_scan.string(String.to_charlist(source)) do
    {:ok, tokens, _line} ->
      case :core_parse.parse(tokens) do
        {:ok, ast} ->
          case :compile.forms(ast, [:from_core, :binary, :return_errors]) do
            {:ok, _mod, bin} ->
              {:ok, f, byte_size(bin)}
            {:ok, _mod, bin, _warnings} ->
              {:ok, f, byte_size(bin)}
            {:error, errors, _warnings} ->
              {:lint, f, inspect(errors) |> String.slice(0..200)}
            :error ->
              {:lint, f, "compile.forms returned :error"}
          end
        {:error, err} ->
          {:parse, f, inspect(err) |> String.slice(0..100)}
      end
    {:error, err, _} ->
      {:scan, f, inspect(err) |> String.slice(0..100)}
  end
end

# Summary
ok = Enum.filter(results, fn {s, _, _} -> s == :ok end)
lint = Enum.filter(results, fn {s, _, _} -> s == :lint end)
parse = Enum.filter(results, fn {s, _, _} -> s == :parse end)
scan = Enum.filter(results, fn {s, _, _} -> s == :scan end)

IO.puts("=== BEAM Compilation Results ===")
IO.puts("OK: #{length(ok)} files")
IO.puts("LINT errors: #{length(lint)} files")
IO.puts("PARSE errors: #{length(parse)} files")
IO.puts("SCAN errors: #{length(scan)} files")
IO.puts("")

if length(ok) > 0 do
  total_size = Enum.reduce(ok, 0, fn {:ok, _, size}, acc -> acc + size end)
  IO.puts("=== Successful (#{length(ok)} files, #{div(total_size, 1024)} KB total) ===")
  for {:ok, f, size} <- ok do
    IO.puts("  #{f}: #{size} bytes")
  end
  IO.puts("")
end

if length(lint) > 0 do
  IO.puts("=== LINT Errors (#{length(lint)}) ===")
  for {:lint, f, err} <- lint do
    IO.puts("  #{f}:")
    IO.puts("    #{err}")
  end
  IO.puts("")
end

if length(parse) > 0 do
  IO.puts("=== Parse Errors (#{length(parse)}) ===")
  for {:parse, f, err} <- parse do
    IO.puts("  #{f}: #{err}")
  end
end

if length(scan) > 0 do
  IO.puts("=== Scan Errors (#{length(scan)}) ===")
  for {:scan, f, err} <- scan do
    IO.puts("  #{f}: #{err}")
  end
end
