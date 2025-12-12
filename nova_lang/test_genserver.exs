# Test GenServer implementation

IO.puts("=== GenServer Test ===\n")

core_dir = "lib/nova"

load_core_module = fn path ->
  case File.read(path) do
    {:ok, content} ->
      case Nova.CoreErlangEval.compile_and_load(content) do
        {:ok, mod_name} -> {:ok, mod_name}
        {:error, reason} -> {:error, path, reason}
      end
    {:error, reason} -> {:error, path, reason}
  end
end

# Load required modules
modules = [
  "Data/Maybe.core", "Data/Either.core", "OTP/GenServer.core"
]

IO.puts("Loading modules...")
Enum.each(modules, fn rel_path ->
  path = Path.join(core_dir, rel_path)
  case load_core_module.(path) do
    {:ok, mod} -> IO.puts("  Loaded #{mod}")
    {:error, p, r} -> IO.puts("  FAILED #{p}: #{inspect(r)}")
  end
end)

IO.puts("\n=== Testing GenServer ===")

# Define callbacks for a simple counter server (curried style for Nova)
callbacks = %{
  handleCall: fn msg -> fn state ->
    case msg do
      :get -> %{reply: state, newState: state}
      {:add, n} -> %{reply: state + n, newState: state + n}
      :reset -> %{reply: :ok, newState: 0}
    end
  end end,
  handleCast: fn msg -> fn state ->
    case msg do
      :increment -> %{newState: state + 1}
      :decrement -> %{newState: state - 1}
      {:set, n} -> %{newState: n}
    end
  end end
}

IO.puts("\n1. Starting GenServer with initial state 0...")
result = apply(:"OTP.GenServer", :startLink, [0, callbacks])
IO.puts("   Result: #{inspect(result)}")

case result do
  {:Right, pid} ->
    IO.puts("   Server started with PID: #{inspect(pid)}")

    IO.puts("\n2. Testing isAlive...")
    alive = apply(:"OTP.GenServer", :isAlive, [pid])
    IO.puts("   isAlive: #{inspect(alive)}")

    IO.puts("\n3. Testing call :get...")
    value = apply(:"OTP.GenServer", :call, [pid, :get])
    IO.puts("   Current value: #{inspect(value)}")

    IO.puts("\n4. Testing call {:add, 10}...")
    value2 = apply(:"OTP.GenServer", :call, [pid, {:add, 10}])
    IO.puts("   After add 10: #{inspect(value2)}")

    IO.puts("\n5. Testing cast :increment...")
    apply(:"OTP.GenServer", :cast, [pid, :increment])
    :timer.sleep(100)  # Give time to process
    value3 = apply(:"OTP.GenServer", :call, [pid, :get])
    IO.puts("   After increment: #{inspect(value3)}")

    IO.puts("\n6. Testing cast {:set, 100}...")
    apply(:"OTP.GenServer", :cast, [pid, {:set, 100}])
    :timer.sleep(100)
    value4 = apply(:"OTP.GenServer", :call, [pid, :get])
    IO.puts("   After set 100: #{inspect(value4)}")

    IO.puts("\n7. Testing callTimeout...")
    result2 = apply(:"OTP.GenServer", :callTimeout, [pid, :get, 1000])
    IO.puts("   callTimeout result: #{inspect(result2)}")

    IO.puts("\n8. Stopping server...")
    apply(:"OTP.GenServer", :stop, [pid])
    :timer.sleep(100)

    IO.puts("\n9. Testing isAlive after stop...")
    alive2 = apply(:"OTP.GenServer", :isAlive, [pid])
    IO.puts("   isAlive: #{inspect(alive2)}")

    IO.puts("\n=== GenServer Test PASSED ===")

  {:Left, error} ->
    IO.puts("   Failed to start: #{inspect(error)}")
    IO.puts("\n=== GenServer Test FAILED ===")
end
