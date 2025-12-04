defmodule Nova.Sandbox do
  @moduledoc """
  Sandboxed BEAM subprocess for safe code evaluation.

  Spawns a separate Elixir node that can be used to:
  - Evaluate Nova code without affecting the main compiler
  - Load and test compiled modules safely
  - Run user code in isolation

  The sandbox node is completely separate from the main MCP server,
  so even if user code crashes or corrupts state, the main server
  is unaffected.

  ## Architecture

  ```
  Main MCP Server (nova@host)
       │
       ├── Nova.Sandbox.Manager (GenServer)
       │       │
       │       └── manages ──> sandbox@host-{id} (separate OS process)
       │                           │
       │                           └── Nova.Sandbox.Worker (GenServer)
       │                                   - executes code
       │                                   - manages sandbox state
       │                                   - can be killed/restarted
  ```

  ## Usage

      # Start a sandbox
      {:ok, sandbox} = Nova.Sandbox.start()

      # Evaluate code
      {:ok, result} = Nova.Sandbox.eval(sandbox, "1 + 2")

      # Load a module
      :ok = Nova.Sandbox.load_module(sandbox, "defmodule Foo do ... end")

      # Call a function in the sandbox
      {:ok, result} = Nova.Sandbox.call(sandbox, Foo, :bar, [1, 2])

      # Stop the sandbox
      :ok = Nova.Sandbox.stop(sandbox)
  """

  use GenServer
  require Logger

  @default_timeout 30_000
  @node_startup_timeout 10_000

  defstruct [
    :id,
    :node,
    :port,
    :os_pid,
    :status,
    :started_at,
    loaded_modules: []
  ]

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Start a new sandbox subprocess.

  Returns `{:ok, sandbox_pid}` on success.
  """
  def start(opts \\ []) do
    GenServer.start(__MODULE__, opts)
  end

  @doc """
  Start a linked sandbox subprocess.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Evaluate an Elixir expression in the sandbox.

  The expression is evaluated in a clean environment within the sandbox node.
  """
  def eval(sandbox, code, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(sandbox, {:eval, code}, timeout)
  end

  @doc """
  Evaluate Nova source code in the sandbox.

  Compiles the Nova code to Elixir and evaluates it.
  """
  def eval_nova(sandbox, nova_code, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(sandbox, {:eval_nova, nova_code}, timeout)
  end

  @doc """
  Load an Elixir module into the sandbox.

  The module will be compiled and loaded in the sandbox node only.
  """
  def load_module(sandbox, module_code, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(sandbox, {:load_module, module_code}, timeout)
  end

  @doc """
  Load compiled Nova code (as Elixir) into the sandbox.
  """
  def load_compiled(sandbox, elixir_code, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(sandbox, {:load_compiled, elixir_code}, timeout)
  end

  @doc """
  Call a function in the sandbox.
  """
  def call(sandbox, module, function, args, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(sandbox, {:call, module, function, args}, timeout)
  end

  @doc """
  Get the status of the sandbox.
  """
  def status(sandbox) do
    GenServer.call(sandbox, :status)
  end

  @doc """
  List modules loaded in the sandbox.
  """
  def list_modules(sandbox) do
    GenServer.call(sandbox, :list_modules)
  end

  @doc """
  Reset the sandbox (restart the node with clean state).
  """
  def reset(sandbox) do
    GenServer.call(sandbox, :reset, @node_startup_timeout + 5_000)
  end

  @doc """
  Stop the sandbox and terminate the subprocess.
  """
  def stop(sandbox) do
    GenServer.stop(sandbox, :normal)
  end

  # ============================================================================
  # GenServer Implementation
  # ============================================================================

  @impl true
  def init(_opts) do
    case start_sandbox_node() do
      {:ok, state} ->
        {:ok, state}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_call({:eval, code}, _from, state) do
    result = rpc_call(state.node, Nova.Sandbox.Worker, :eval, [code])
    {:reply, result, state}
  end

  def handle_call({:eval_nova, nova_code}, _from, state) do
    result = rpc_call(state.node, Nova.Sandbox.Worker, :eval_nova, [nova_code])
    {:reply, result, state}
  end

  def handle_call({:load_module, module_code}, _from, state) do
    case rpc_call(state.node, Nova.Sandbox.Worker, :load_module, [module_code]) do
      {:ok, module_name} ->
        new_state = %{state | loaded_modules: [module_name | state.loaded_modules]}
        {:reply, {:ok, module_name}, new_state}
      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:load_compiled, elixir_code}, _from, state) do
    case rpc_call(state.node, Nova.Sandbox.Worker, :load_compiled, [elixir_code]) do
      {:ok, modules} when is_list(modules) ->
        new_state = %{state | loaded_modules: modules ++ state.loaded_modules}
        {:reply, {:ok, modules}, new_state}
      {:ok, module_name} ->
        new_state = %{state | loaded_modules: [module_name | state.loaded_modules]}
        {:reply, {:ok, module_name}, new_state}
      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:call, module, function, args}, _from, state) do
    result = rpc_call(state.node, Nova.Sandbox.Worker, :call_function, [module, function, args])
    {:reply, result, state}
  end

  def handle_call(:status, _from, state) do
    node_alive = Node.ping(state.node) == :pong
    status = %{
      id: state.id,
      node: state.node,
      alive: node_alive,
      started_at: state.started_at,
      loaded_modules: state.loaded_modules,
      uptime_seconds: DateTime.diff(DateTime.utc_now(), state.started_at)
    }
    {:reply, {:ok, status}, state}
  end

  def handle_call(:list_modules, _from, state) do
    {:reply, {:ok, state.loaded_modules}, state}
  end

  def handle_call(:reset, _from, state) do
    # Stop the current node
    stop_sandbox_node(state)

    # Start a fresh one
    case start_sandbox_node() do
      {:ok, new_state} ->
        {:reply, :ok, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info({:DOWN, _ref, :port, _port, reason}, state) do
    Logger.warning("Sandbox node port closed: #{inspect(reason)}")
    {:stop, :sandbox_crashed, state}
  end

  def handle_info({_port, {:exit_status, status}}, state) do
    Logger.warning("Sandbox node exited with status: #{status}")
    {:stop, :sandbox_exited, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    stop_sandbox_node(state)
    :ok
  end

  # ============================================================================
  # Private Helpers
  # ============================================================================

  defp start_sandbox_node do
    id = :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    node_name = :"sandbox_#{id}@127.0.0.1"

    # Ensure epmd is running and we have a node name
    ensure_distribution()

    # Build the command to start a new Elixir node
    cookie = Node.get_cookie()
    elixir_path = System.find_executable("elixir")

    # Write the worker module to a temp file
    tmp_dir = System.tmp_dir!()
    worker_file = Path.join(tmp_dir, "nova_sandbox_worker_#{id}.exs")
    File.write!(worker_file, sandbox_worker_code())

    # Start the node loading the worker file
    cmd_args = [
      "--name", to_string(node_name),
      "--cookie", to_string(cookie),
      "--no-halt",
      worker_file
    ]

    port = Port.open({:spawn_executable, elixir_path}, [
      :binary,
      :exit_status,
      :use_stdio,
      :stderr_to_stdout,
      args: cmd_args
    ])

    # Wait for the node to come up and connect
    case wait_for_node(node_name, @node_startup_timeout) do
      :ok ->
        os_pid = case Port.info(port, :os_pid) do
          {:os_pid, pid} -> pid
          _ -> nil
        end

        # Give the node time to load the module before cleaning up
        Process.sleep(500)
        File.rm(worker_file)

        state = %__MODULE__{
          id: id,
          node: node_name,
          port: port,
          os_pid: os_pid,
          status: :running,
          started_at: DateTime.utc_now(),
          loaded_modules: []
        }

        {:ok, state}

      {:error, :timeout} ->
        Port.close(port)
        File.rm(worker_file)
        {:error, :node_startup_timeout}
    end
  end

  defp stop_sandbox_node(%{port: port, node: node}) when not is_nil(port) do
    # Gracefully stop the node
    try do
      :rpc.call(node, :init, :stop, [], 1000)
    catch
      _, _ -> :ok
    end

    # Close the port
    try do
      Port.close(port)
    catch
      _, _ -> :ok
    end

    :ok
  end
  defp stop_sandbox_node(_state), do: :ok

  defp wait_for_node(node_name, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_for_node(node_name, deadline)
  end

  defp do_wait_for_node(node_name, deadline) do
    if System.monotonic_time(:millisecond) > deadline do
      {:error, :timeout}
    else
      case Node.ping(node_name) do
        :pong -> :ok
        :pang ->
          Process.sleep(100)
          do_wait_for_node(node_name, deadline)
      end
    end
  end

  defp rpc_call(node, module, function, args) do
    case :rpc.call(node, module, function, args, @default_timeout) do
      {:badrpc, reason} -> {:error, {:rpc_failed, reason}}
      result -> result
    end
  end

  defp ensure_distribution do
    unless Node.alive?() do
      # Start distribution with a unique name
      id = :crypto.strong_rand_bytes(4) |> Base.encode16(case: :lower)
      node_name = :"nova_mcp_#{id}@127.0.0.1"
      {:ok, _} = Node.start(node_name)
      Node.set_cookie(:nova_sandbox_cookie)
    end
    :ok
  end

  # The worker module code that runs inside the sandbox
  defp sandbox_worker_code do
    """
    # Nova Sandbox Worker Module
    # This runs in an isolated BEAM node

    defmodule Nova.Sandbox.Worker do
      def eval(code) do
        try do
          {result, _bindings} = Code.eval_string(code, [], __ENV__)
          {:ok, result}
        rescue
          e -> {:error, {:exception, Exception.message(e)}}
        catch
          kind, reason -> {:error, {kind, reason}}
        end
      end

      def eval_nova(_nova_code) do
        # This requires Nova compiler to be available in the sandbox
        {:error, :nova_compiler_not_loaded}
      end

      def load_module(module_code) do
        try do
          [{module_name, _}] = Code.compile_string(module_code)
          {:ok, module_name}
        rescue
          e -> {:error, {:compile_error, Exception.message(e)}}
        catch
          kind, reason -> {:error, {kind, reason}}
        end
      end

      def load_compiled(elixir_code) do
        try do
          modules = Code.compile_string(elixir_code)
          module_names = Enum.map(modules, fn {name, _} -> name end)
          {:ok, module_names}
        rescue
          e -> {:error, {:compile_error, Exception.message(e)}}
        catch
          kind, reason -> {:error, {kind, reason}}
        end
      end

      def call_function(module, function, args) do
        try do
          result = apply(module, function, args)
          {:ok, result}
        rescue
          e -> {:error, {:exception, Exception.message(e)}}
        catch
          kind, reason -> {:error, {kind, reason}}
        end
      end
    end

    # Keep the node alive
    receive do
      :stop -> :ok
    end
    """
  end
end
