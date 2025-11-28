# Run all Nova compiler tests
# Usage: mix run test/run_all.exs

IO.puts("╔══════════════════════════════════════════════════════════════╗")
IO.puts("║         Nova Compiler Test Suite (Stage 2 Elixir)           ║")
IO.puts("╚══════════════════════════════════════════════════════════════╝\n")

{time, _} = :timer.tc(fn ->
  # Run parser tests
  Code.require_file("parser_test.exs", __DIR__)
  IO.puts("")

  # Run codegen tests
  Code.require_file("codegen_test.exs", __DIR__)
end)

IO.puts("\n══════════════════════════════════════════════════════════════")
IO.puts("Total test time: #{Float.round(time/1_000_000, 2)}s")
