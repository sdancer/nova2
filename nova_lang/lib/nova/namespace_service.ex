defmodule Nova.NamespaceService do
  @moduledoc """
  Namespace Service for Nova Compiler.

  This GenServer manages namespaces where code elements (declarations) can be
  added, edited, queried, and removed dynamically. It replaces file-based
  modules with namespace-based organization for LSP-like functionality.

  ## Features

  - Dynamic declaration management (add/update/remove)
  - Incremental type checking with dependency tracking
  - Query API for types, diagnostics, and completions
  - Namespace isolation with cross-namespace imports

  ## Usage

      {:ok, svc} = Nova.NamespaceService.start_link()
      :ok = Nova.NamespaceService.create_namespace(svc, "MyApp.Core")
      {:ok, id} = Nova.NamespaceService.add_declaration(svc, "MyApp.Core", "add x y = x + y")
      {:ok, type} = Nova.NamespaceService.get_type(svc, id)
  """

  use GenServer

  # ============================================================================
  # Types
  # ============================================================================

  defmodule DeclStatus do
    @moduledoc "Status of a declaration"
    @type t :: :fresh | :valid | :invalid | :stale
  end

  defmodule DeclMetadata do
    @moduledoc "Metadata for a managed declaration"
    defstruct [
      :decl_id,
      :namespace,
      :name,
      :kind,
      version: 1,
      status: :fresh,
      dependencies: MapSet.new(),
      dependents: MapSet.new()
    ]

    @type t :: %__MODULE__{
      decl_id: String.t(),
      namespace: String.t(),
      name: String.t(),
      kind: :function | :datatype | :typealias | :typeclass | :instance | :foreign,
      version: non_neg_integer(),
      status: DeclStatus.t(),
      dependencies: MapSet.t(String.t()),
      dependents: MapSet.t(String.t())
    }
  end

  defmodule Diagnostic do
    @moduledoc "A structured diagnostic (error or warning)"
    defstruct [
      :severity,       # :error | :warning | :info | :hint
      :code,           # Error code like "E001" or category like "type_mismatch"
      :message,        # Human-readable message
      :location,       # %{line: int, column: int, end_line: int, end_column: int} or nil
      :source_snippet, # The relevant source code lines
      :suggestions,    # List of suggested fixes
      :related         # List of related locations/info
    ]

    @type location :: %{
      line: non_neg_integer(),
      column: non_neg_integer(),
      end_line: non_neg_integer() | nil,
      end_column: non_neg_integer() | nil
    }

    @type t :: %__MODULE__{
      severity: :error | :warning | :info | :hint,
      code: String.t() | nil,
      message: String.t(),
      location: location() | nil,
      source_snippet: String.t() | nil,
      suggestions: [String.t()],
      related: [%{message: String.t(), location: location()}]
    }
  end

  defmodule ManagedDecl do
    @moduledoc "A declaration managed by the namespace service"
    defstruct [
      :meta,
      :decl,           # Parsed AST
      :source_text,    # Original source
      :inferred_type,  # Cached type after type-check
      errors: [],      # Cached errors (legacy string format)
      diagnostics: []  # Structured diagnostics
    ]

    @type t :: %__MODULE__{
      meta: DeclMetadata.t(),
      decl: term(),
      source_text: String.t(),
      inferred_type: term() | nil,
      errors: [String.t()],
      diagnostics: [Diagnostic.t()]
    }
  end

  defmodule NamespaceState do
    @moduledoc "State of a single namespace"
    defstruct [
      name: "",
      declarations: %{},    # decl_id -> ManagedDecl
      name_index: %{},      # name -> decl_id (for lookup)
      type_env: nil,        # Current type environment
      imports: []           # List of imported namespaces
    ]

    @type t :: %__MODULE__{
      name: String.t(),
      declarations: %{String.t() => ManagedDecl.t()},
      name_index: %{String.t() => String.t()},
      type_env: term() | nil,
      imports: [String.t()]
    }
  end

  defmodule State do
    @moduledoc "Global service state"
    defstruct [
      namespaces: %{},     # namespace_name -> NamespaceState
      global_counter: 0,   # For generating unique IDs
      dep_graph: %{        # Global dependency graph
        forward: %{},      # decl_id -> MapSet of decl_ids it depends on
        reverse: %{}       # decl_id -> MapSet of decl_ids that depend on it
      }
    ]

    @type t :: %__MODULE__{
      namespaces: %{String.t() => NamespaceState.t()},
      global_counter: non_neg_integer(),
      dep_graph: %{forward: map(), reverse: map()}
    }
  end

  # ============================================================================
  # Client API
  # ============================================================================

  @doc "Start the namespace service"
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  @doc "Create a new namespace"
  def create_namespace(server, name) when is_binary(name) do
    GenServer.call(server, {:create_namespace, name})
  end

  @doc "Delete a namespace and all its declarations"
  def delete_namespace(server, name) when is_binary(name) do
    GenServer.call(server, {:delete_namespace, name})
  end

  @doc "List all namespaces"
  def list_namespaces(server) do
    GenServer.call(server, :list_namespaces)
  end

  @doc "Add a declaration to a namespace"
  def add_declaration(server, namespace, source) when is_binary(namespace) and is_binary(source) do
    GenServer.call(server, {:add_declaration, namespace, source})
  end

  @doc "Update an existing declaration"
  def update_declaration(server, decl_id, source) when is_binary(decl_id) and is_binary(source) do
    GenServer.call(server, {:update_declaration, decl_id, source})
  end

  @doc "Remove a declaration"
  def remove_declaration(server, decl_id) when is_binary(decl_id) do
    GenServer.call(server, {:remove_declaration, decl_id})
  end

  @doc "Get type of a declaration"
  def get_type(server, decl_id) when is_binary(decl_id) do
    GenServer.call(server, {:get_type, decl_id})
  end

  @doc "Get diagnostics for a namespace"
  def get_diagnostics(server, namespace) when is_binary(namespace) do
    GenServer.call(server, {:get_diagnostics, namespace})
  end

  @doc "Get diagnostics for a specific declaration"
  def get_decl_diagnostics(server, decl_id) when is_binary(decl_id) do
    GenServer.call(server, {:get_decl_diagnostics, decl_id})
  end

  @doc "Get all declarations in a namespace"
  def list_declarations(server, namespace) when is_binary(namespace) do
    GenServer.call(server, {:list_declarations, namespace})
  end

  @doc "Get declaration by name in namespace"
  def get_declaration(server, namespace, name) when is_binary(namespace) and is_binary(name) do
    GenServer.call(server, {:get_declaration, namespace, name})
  end

  @doc "Get status of a declaration"
  def get_status(server, decl_id) when is_binary(decl_id) do
    GenServer.call(server, {:get_status, decl_id})
  end

  @doc "Validate all stale/fresh declarations in a namespace"
  def validate_namespace(server, namespace) when is_binary(namespace) do
    GenServer.call(server, {:validate_namespace, namespace}, :infinity)
  end

  @doc "Get completions for a prefix in a namespace"
  def get_completions(server, namespace, prefix) when is_binary(namespace) and is_binary(prefix) do
    GenServer.call(server, {:get_completions, namespace, prefix})
  end

  @doc "Add an import to a namespace"
  def add_import(server, namespace, imported_namespace) when is_binary(namespace) and is_binary(imported_namespace) do
    GenServer.call(server, {:add_import, namespace, imported_namespace})
  end

  @doc "Remove an import from a namespace"
  def remove_import(server, namespace, imported_namespace) when is_binary(namespace) and is_binary(imported_namespace) do
    GenServer.call(server, {:remove_import, namespace, imported_namespace})
  end

  @doc "List imports of a namespace"
  def list_imports(server, namespace) when is_binary(namespace) do
    GenServer.call(server, {:list_imports, namespace})
  end

  @doc "Export all namespaces for session persistence"
  def export_state(server) do
    GenServer.call(server, :export_state)
  end

  @doc "Import namespaces from saved session data"
  def import_state(server, session_data, merge \\ false) do
    GenServer.call(server, {:import_state, session_data, merge})
  end

  @doc "Clear all namespaces"
  def clear_all(server) do
    GenServer.call(server, :clear_all)
  end

  # ============================================================================
  # Server Callbacks
  # ============================================================================

  @impl true
  def init(_opts) do
    {:ok, %State{}}
  end

  @impl true
  def handle_call({:create_namespace, name}, _from, state) do
    if Map.has_key?(state.namespaces, name) do
      {:reply, {:error, :already_exists}, state}
    else
      ns = %NamespaceState{name: name}
      new_state = %{state | namespaces: Map.put(state.namespaces, name, ns)}
      {:reply, :ok, new_state}
    end
  end

  def handle_call({:delete_namespace, name}, _from, state) do
    if Map.has_key?(state.namespaces, name) do
      # Remove all declarations from dependency graph
      ns = state.namespaces[name]
      decl_ids = Map.keys(ns.declarations)
      new_graph = Enum.reduce(decl_ids, state.dep_graph, fn decl_id, graph ->
        Nova.Dependencies.remove_from_graph(graph, decl_id)
      end)
      new_state = %{state |
        namespaces: Map.delete(state.namespaces, name),
        dep_graph: new_graph
      }
      {:reply, :ok, new_state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:list_namespaces, _from, state) do
    {:reply, {:ok, Map.keys(state.namespaces)}, state}
  end

  def handle_call({:add_declaration, namespace, source}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        case parse_declaration(source) do
          {:error, reason} ->
            {:reply, {:error, {:parse_error, reason}}, state}

          {:ok, decl, name, kind} ->
            # Generate unique ID
            counter = state.global_counter + 1
            decl_id = make_decl_id(namespace, kind, name, counter)

            # Check for duplicate name
            if Map.has_key?(ns.name_index, name) do
              {:reply, {:error, {:duplicate_name, name}}, state}
            else
              # Extract dependencies from the declaration
              raw_deps = Nova.Dependencies.get_dependencies(decl)

              # Resolve dependency names to decl_ids
              resolved_deps = resolve_dependencies(state, namespace, raw_deps)

              # Create managed declaration with dependencies
              meta = %DeclMetadata{
                decl_id: decl_id,
                namespace: namespace,
                name: name,
                kind: kind,
                version: 1,
                status: :fresh,
                dependencies: resolved_deps
              }
              managed = %ManagedDecl{
                meta: meta,
                decl: decl,
                source_text: source
              }

              # Update namespace - add declaration name and any method names (for type classes)
              updated_name_index = ns.name_index
                |> Map.put(name, decl_id)
                |> add_method_names_to_index(decl, decl_id)

              new_ns = %{ns |
                declarations: Map.put(ns.declarations, decl_id, managed),
                name_index: updated_name_index
              }

              # Update dependency graph
              new_graph = Nova.Dependencies.add_to_graph(state.dep_graph, decl_id, resolved_deps)

              new_state = %{state |
                namespaces: Map.put(state.namespaces, namespace, new_ns),
                global_counter: counter,
                dep_graph: new_graph
              }

              {:reply, {:ok, decl_id}, new_state}
            end
        end
    end
  end

  def handle_call({:update_declaration, decl_id, source}, _from, state) do
    case find_declaration(state, decl_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {ns_name, ns, managed} ->
        case parse_declaration(source) do
          {:error, reason} ->
            {:reply, {:error, {:parse_error, reason}}, state}

          {:ok, decl, name, _kind} ->
            # Check name matches (or handle rename)
            if name != managed.meta.name do
              {:reply, {:error, {:name_mismatch, managed.meta.name, name}}, state}
            else
              # Extract new dependencies
              raw_deps = Nova.Dependencies.get_dependencies(decl)
              resolved_deps = resolve_dependencies(state, ns_name, raw_deps)

              # Update metadata with new dependencies
              new_meta = %{managed.meta |
                version: managed.meta.version + 1,
                status: :fresh,
                dependencies: resolved_deps
              }
              new_managed = %{managed |
                meta: new_meta,
                decl: decl,
                source_text: source,
                inferred_type: nil,
                errors: []
              }

              # Mark dependents as stale
              dependents = Nova.Dependencies.get_dependents(state.dep_graph, decl_id)
              new_state = mark_stale(state, dependents)

              # Update dependency graph (remove old, add new)
              new_graph = state.dep_graph
              |> Nova.Dependencies.remove_from_graph(decl_id)
              |> Nova.Dependencies.add_to_graph(decl_id, resolved_deps)

              # Update namespace
              new_ns = %{ns | declarations: Map.put(ns.declarations, decl_id, new_managed)}
              new_state = %{new_state |
                namespaces: Map.put(new_state.namespaces, ns_name, new_ns),
                dep_graph: new_graph
              }

              {:reply, {:ok, decl_id}, new_state}
            end
        end
    end
  end

  def handle_call({:remove_declaration, decl_id}, _from, state) do
    case find_declaration(state, decl_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {ns_name, ns, managed} ->
        # Mark dependents as stale
        dependents = Nova.Dependencies.get_dependents(state.dep_graph, decl_id)
        new_state = mark_stale(state, dependents)

        # Remove from graph
        new_graph = Nova.Dependencies.remove_from_graph(new_state.dep_graph, decl_id)

        # Remove from namespace
        new_ns = %{ns |
          declarations: Map.delete(ns.declarations, decl_id),
          name_index: Map.delete(ns.name_index, managed.meta.name)
        }

        new_state = %{new_state |
          namespaces: Map.put(new_state.namespaces, ns_name, new_ns),
          dep_graph: new_graph
        }

        {:reply, :ok, new_state}
    end
  end

  def handle_call({:get_type, decl_id}, _from, state) do
    case find_declaration(state, decl_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {_ns_name, _ns, managed} ->
        case managed.meta.status do
          :valid -> {:reply, {:ok, managed.inferred_type}, state}
          :invalid -> {:reply, {:error, {:type_errors, managed.errors}}, state}
          status -> {:reply, {:error, {:not_validated, status}}, state}
        end
    end
  end

  def handle_call({:get_diagnostics, namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        diagnostics = ns.declarations
        |> Map.values()
        |> Enum.flat_map(fn managed ->
          # Use structured diagnostics if available, fall back to legacy errors
          if managed.diagnostics != [] do
            Enum.map(managed.diagnostics, fn diag ->
              %{
                decl_id: managed.meta.decl_id,
                name: managed.meta.name,
                severity: diag.severity,
                code: diag.code,
                message: diag.message,
                location: diag.location,
                source_snippet: diag.source_snippet,
                suggestions: diag.suggestions,
                related: diag.related
              }
            end)
          else
            Enum.map(managed.errors, fn error ->
              %{
                decl_id: managed.meta.decl_id,
                name: managed.meta.name,
                severity: :error,
                code: nil,
                message: error,
                location: nil,
                source_snippet: nil,
                suggestions: [],
                related: []
              }
            end)
          end
        end)
        {:reply, {:ok, diagnostics}, state}
    end
  end

  def handle_call({:get_decl_diagnostics, decl_id}, _from, state) do
    case find_declaration(state, decl_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {_ns_name, _ns, managed} ->
        {:reply, {:ok, managed.errors}, state}
    end
  end

  def handle_call({:list_declarations, namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        decls = ns.declarations
        |> Map.values()
        |> Enum.map(fn managed ->
          %{
            decl_id: managed.meta.decl_id,
            name: managed.meta.name,
            kind: managed.meta.kind,
            status: managed.meta.status,
            version: managed.meta.version
          }
        end)
        {:reply, {:ok, decls}, state}
    end
  end

  def handle_call({:get_declaration, namespace, name}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        case Map.get(ns.name_index, name) do
          nil -> {:reply, {:error, :not_found}, state}
          decl_id ->
            managed = ns.declarations[decl_id]
            {:reply, {:ok, managed}, state}
        end
    end
  end

  def handle_call({:get_status, decl_id}, _from, state) do
    case find_declaration(state, decl_id) do
      nil -> {:reply, {:error, :not_found}, state}
      {_ns_name, _ns, managed} -> {:reply, {:ok, managed.meta.status}, state}
    end
  end

  def handle_call({:validate_namespace, namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        # First, ensure all imported namespaces are validated
        case validate_imports(state, ns.imports) do
          {:error, reason} ->
            {:reply, {:error, {:import_validation_failed, reason}}, state}

          {:ok, state_with_validated_imports} ->
            # Perform incremental validation
            case incremental_validate(state_with_validated_imports, namespace) do
              {:ok, new_state, validated_count} ->
                {:reply, {:ok, validated_count}, new_state}

              {:error, errors, new_state} ->
                {:reply, {:error, {:type_errors, errors}}, new_state}
            end
        end
    end
  end

  # Incremental validation - only validate dirty (fresh/stale) declarations
  defp incremental_validate(state, namespace) do
    ns = state.namespaces[namespace]

    # Find declarations that need validation
    dirty_decls = ns.declarations
    |> Enum.filter(fn {_id, m} -> m.meta.status in [:fresh, :stale] end)
    |> Map.new()

    if map_size(dirty_decls) == 0 do
      # Nothing to validate, everything is already valid
      {:ok, state, 0}
    else
      # Get all decl_ids that need validation
      dirty_ids = Map.keys(dirty_decls)

      # Sort in dependency order using topological sort
      sorted_ids = Nova.Dependencies.topological_sort(state.dep_graph, dirty_ids)

      # Build base environment from:
      # 1. Imported namespaces
      # 2. Already-valid declarations in this namespace
      base_env = Nova.Compiler.Types.empty_env()
      env_with_imports = build_env_from_imports(state, ns.imports, base_env)
      env_with_valid = build_env_from_valid_decls(ns, env_with_imports, dirty_ids)

      # Validate each dirty declaration incrementally
      validate_declarations_incrementally(state, namespace, sorted_ids, env_with_valid, [], 0)
    end
  end

  # Build environment from already-valid declarations (excluding dirty ones)
  defp build_env_from_valid_decls(ns, env, dirty_ids) do
    dirty_set = MapSet.new(dirty_ids)

    ns.declarations
    |> Enum.filter(fn {id, m} ->
      m.meta.status == :valid and not MapSet.member?(dirty_set, id)
    end)
    |> Enum.reduce(env, fn {_id, managed}, acc_env ->
      case managed.inferred_type do
        nil -> acc_env
        type_scheme -> add_to_env(acc_env, managed.meta.name, type_scheme)
      end
    end)
  end

  # Validate declarations one by one in dependency order
  defp validate_declarations_incrementally(state, namespace, [], env, errors, count) do
    if errors == [] do
      # Store the final environment in the namespace for use by imports
      ns = state.namespaces[namespace]
      new_ns = %{ns | type_env: env}
      new_state = %{state | namespaces: Map.put(state.namespaces, namespace, new_ns)}
      {:ok, new_state, count}
    else
      {:error, Enum.reverse(errors), state}
    end
  end

  defp validate_declarations_incrementally(state, namespace, [decl_id | rest], env, errors, count) do
    ns = state.namespaces[namespace]
    managed = ns.declarations[decl_id]

    # Type check this single declaration
    case validate_single_declaration(env, managed) do
      {:ok, new_env, updated_managed} ->
        # Update the declaration in state
        new_ns = %{ns | declarations: Map.put(ns.declarations, decl_id, updated_managed)}
        new_state = %{state | namespaces: Map.put(state.namespaces, namespace, new_ns)}
        validate_declarations_incrementally(new_state, namespace, rest, new_env, errors, count + 1)

      {:error, error_msg, updated_managed} ->
        # Mark as invalid but continue with others
        new_ns = %{ns | declarations: Map.put(ns.declarations, decl_id, updated_managed)}
        new_state = %{state | namespaces: Map.put(state.namespaces, namespace, new_ns)}
        validate_declarations_incrementally(new_state, namespace, rest, env, [error_msg | errors], count + 1)
    end
  end

  # Validate a single declaration and return updated environment
  defp validate_single_declaration(env, managed) do
    decl = managed.decl

    # Wrap single declaration in a list for check_module
    case Nova.Compiler.TypeChecker.check_module(env, [decl]) do
      {:left, err} ->
        error_msg = format_type_error(err)
        diagnostic = build_diagnostic(err, managed.source_text, managed.meta.name)
        updated = %{managed |
          meta: %{managed.meta | status: :invalid},
          inferred_type: nil,
          errors: [error_msg],
          diagnostics: [diagnostic]
        }
        {:error, error_msg, updated}

      {:right, new_env} ->
        # Extract the inferred type from the new environment
        inferred_type = extract_type_for_decl(new_env, decl)
        updated = %{managed |
          meta: %{managed.meta | status: :valid},
          inferred_type: inferred_type,
          errors: [],
          diagnostics: []
        }
        {:ok, new_env, updated}
    end
  end

  # Extract the inferred type for a declaration from the environment
  defp extract_type_for_decl(env, decl) do
    case decl do
      {:decl_function, f} ->
        case Nova.Compiler.Types.lookup_env(env, f.name) do
          {:just, scheme} -> scheme
          :nothing -> nil
        end

      {:decl_data_type, d} ->
        # Data types add constructor types - return the first one as representative
        case d.constructors do
          {:cons, c, _} ->
            case Nova.Compiler.Types.lookup_env(env, c.name) do
              {:just, scheme} -> scheme
              :nothing -> nil
            end
          [c | _] ->
            case Nova.Compiler.Types.lookup_env(env, c.name) do
              {:just, scheme} -> scheme
              :nothing -> nil
            end
          :nil -> nil
          nil -> nil
          [] -> nil
        end

      _ -> nil
    end
  end

  # Validate all imported namespaces (recursively)
  defp validate_imports(state, []), do: {:ok, state}
  defp validate_imports(state, [import_ns | rest]) do
    case Map.get(state.namespaces, import_ns) do
      nil ->
        {:error, {:namespace_not_found, import_ns}}

      ns ->
        # Check if namespace needs validation
        needs_validation = ns.declarations
        |> Map.values()
        |> Enum.any?(fn m -> m.meta.status in [:fresh, :stale] end)

        if needs_validation do
          # Recursively validate this import's imports first
          case validate_imports(state, ns.imports) do
            {:error, reason} ->
              {:error, reason}

            {:ok, state2} ->
              # Use incremental validation for imported namespace too
              case incremental_validate(state2, import_ns) do
                {:ok, state3, _count} ->
                  validate_imports(state3, rest)

                {:error, errors, _state3} ->
                  {:error, {:type_check_failed, import_ns, hd(errors)}}
              end
          end
        else
          validate_imports(state, rest)
        end
    end
  end

  # Build a combined type environment from imported namespaces
  defp build_env_from_imports(state, imports, base_env) do
    Enum.reduce(imports, base_env, fn import_ns, env ->
      case Map.get(state.namespaces, import_ns) do
        nil -> env
        ns ->
          # If namespace has a validated type_env, use all its bindings
          # This includes data constructors and other types not stored in managed.inferred_type
          case ns.type_env do
            nil ->
              # Fallback: add declarations individually (for unvalidated namespaces)
              add_declarations_to_env(env, ns, import_ns)

            validated_env ->
              # Use all bindings from the validated environment
              merge_env_bindings(env, validated_env, ns, import_ns)
          end
      end
    end)
  end

  # Add declarations to environment (fallback when no type_env is available)
  defp add_declarations_to_env(env, ns, import_ns) do
    Enum.reduce(ns.declarations, env, fn {_id, managed}, acc_env ->
      case managed.inferred_type do
        nil -> acc_env
        type_scheme ->
          name = managed.meta.name
          qualified_name = "#{import_ns}.#{name}"

          acc_env
          |> add_to_env(name, type_scheme)
          |> add_to_env(qualified_name, type_scheme)
      end
    end)
  end

  # Merge all bindings from a validated environment
  # Only exports names that were declared in this namespace (functions + data constructors)
  defp merge_env_bindings(target_env, source_env, ns, namespace) do
    # Get all names defined in this namespace:
    # 1. Function/type alias/etc names from declarations
    # 2. Data constructor names from data type declarations
    defined_names = collect_defined_names(ns)

    source_bindings = source_env.bindings

    # Only add bindings for names defined in this namespace
    Enum.reduce(source_bindings, target_env, fn {name, scheme}, acc_env ->
      if MapSet.member?(defined_names, name) do
        qualified_name = "#{namespace}.#{name}"
        acc_env
        |> add_to_env(name, scheme)
        |> add_to_env(qualified_name, scheme)
      else
        acc_env
      end
    end)
  end

  # Collect all names defined in a namespace (functions, data constructors, type aliases)
  defp collect_defined_names(ns) do
    Enum.reduce(ns.declarations, MapSet.new(), fn {_id, managed}, acc ->
      case managed.decl do
        {:decl_function, f} ->
          MapSet.put(acc, f.name)

        {:decl_data_type, d} ->
          # Add both the type name and all constructor names
          acc = MapSet.put(acc, d.name)
          constructors = Nova.List.to_list(d.constructors)
          Enum.reduce(constructors, acc, fn c, a ->
            MapSet.put(a, c.name)
          end)

        {:decl_type_alias, a} ->
          MapSet.put(acc, a.name)

        {:decl_type_class, c} ->
          MapSet.put(acc, c.name)

        {:decl_foreign_import, f} ->
          MapSet.put(acc, f.function_name)

        _ ->
          acc
      end
    end)
  end

  # For type classes, add method names to the name_index so dependencies can be resolved
  defp add_method_names_to_index(index, {:decl_type_class, tc}, decl_id) do
    methods = Nova.List.to_list(tc.methods)
    Enum.reduce(methods, index, fn method, acc ->
      Map.put(acc, method.name, decl_id)
    end)
  end
  defp add_method_names_to_index(index, _decl, _decl_id), do: index

  # Helper to add a binding to the environment
  defp add_to_env(env, name, scheme) do
    Nova.Compiler.Types.extend_env(env, name, scheme)
  end

  # Helper to mark all declarations as invalid
  defp mark_all_invalid(ns, error_msg) do
    new_decls = ns.declarations
    |> Enum.map(fn {id, managed} ->
      new_managed = %{managed |
        meta: %{managed.meta | status: :invalid},
        errors: [error_msg]
      }
      {id, new_managed}
    end)
    |> Map.new()

    %{ns | declarations: new_decls}
  end

  # Format type error for display
  defp format_type_error({:unify_err, {:occurs_check, v, t}}) do
    "Occurs check failed: #{inspect(v)} in #{inspect(t)}"
  end
  defp format_type_error({:unify_err, {:mismatch, t1, t2}}) do
    "Type mismatch: expected #{format_type(t1)}, got #{format_type(t2)}"
  end
  defp format_type_error({:not_found, name}) do
    "Unknown identifier: #{name}"
  end
  defp format_type_error(other) do
    "Type error: #{inspect(other)}"
  end

  # Format a type for display
  defp format_type({:ty_con, %{name: name, args: []}}), do: name
  defp format_type({:ty_con, %{name: name, args: args}}) do
    arg_strs = Enum.map(args, &format_type/1) |> Enum.join(" ")
    "#{name} #{arg_strs}"
  end
  defp format_type({:ty_var, %{name: name}}), do: name
  defp format_type({:ty_arrow, from, to}), do: "#{format_type(from)} -> #{format_type(to)}"
  defp format_type({:ty_record, %{fields: fields}}) do
    field_strs = Enum.map(fields, fn {k, v} -> "#{k} :: #{format_type(v)}" end) |> Enum.join(", ")
    "{ #{field_strs} }"
  end
  defp format_type(other), do: inspect(other)

  # Build a structured diagnostic from a type error
  defp build_diagnostic(err, source_text, decl_name) do
    {code, message, suggestions} = classify_error(err)
    location = find_error_location(err, source_text)
    snippet = extract_snippet(source_text, location)

    %Diagnostic{
      severity: :error,
      code: code,
      message: message,
      location: location,
      source_snippet: snippet,
      suggestions: suggestions,
      related: []
    }
  end

  # Classify error and generate helpful message with suggestions
  defp classify_error({:unbound_variable, name}) do
    suggestions = suggest_similar_names(name)
    suggestion_text = if suggestions != [], do: " Did you mean: #{Enum.join(suggestions, ", ")}?", else: ""
    {
      "E001",
      "Unknown identifier '#{name}'.#{suggestion_text}",
      suggestions
    }
  end

  defp classify_error({:unify_err, {:type_mismatch, t1, t2}}) do
    {
      "E002",
      "Type mismatch: expected '#{format_type(t1)}' but got '#{format_type(t2)}'.",
      generate_type_mismatch_suggestions(t1, t2)
    }
  end

  defp classify_error({:unify_err, {:occurs_check, v, t}}) do
    {
      "E003",
      "Infinite type detected: '#{v.name}' occurs within '#{format_type(t)}'. This usually means a function is being applied to itself incorrectly.",
      ["Check for recursive calls without a base case", "Ensure function arguments have the correct types"]
    }
  end

  defp classify_error({:unify_err, {:arity_mismatch, name, expected, got}}) do
    {
      "E004",
      "Arity mismatch for '#{name}': expected #{expected} type arguments but got #{got}.",
      []
    }
  end

  defp classify_error({:unify_err, {:record_field_mismatch, field}}) do
    {
      "E005",
      "Record field mismatch: field '#{field}' has incompatible types.",
      []
    }
  end

  defp classify_error({:not_implemented, feature}) do
    {
      "E099",
      "Feature not yet implemented: #{feature}",
      []
    }
  end

  defp classify_error(other) do
    {
      "E000",
      "Type error: #{inspect(other)}",
      []
    }
  end

  # Find location in source where error occurred
  # For now, use line 1 as default since we don't have position tracking in type errors yet
  defp find_error_location({:unbound_variable, name}, source_text) do
    # Try to find the variable in the source
    case find_identifier_position(source_text, name) do
      nil -> %{line: 1, column: 1, end_line: nil, end_column: nil}
      pos -> pos
    end
  end

  defp find_error_location(_err, _source_text) do
    # Default to first line
    %{line: 1, column: 1, end_line: nil, end_column: nil}
  end

  # Find position of an identifier in source text
  defp find_identifier_position(source_text, name) do
    lines = String.split(source_text, "\n")
    # Build a regex that matches the identifier as a whole word
    pattern = ~r/\b#{Regex.escape(name)}\b/

    Enum.with_index(lines, 1)
    |> Enum.find_value(fn {line, line_num} ->
      case Regex.run(pattern, line, return: :index) do
        [{start, len}] ->
          %{
            line: line_num,
            column: start + 1,
            end_line: line_num,
            end_column: start + len + 1
          }
        _ -> nil
      end
    end)
  end

  # Extract a source snippet around the error location
  defp extract_snippet(source_text, nil), do: nil
  defp extract_snippet(source_text, %{line: line}) do
    lines = String.split(source_text, "\n")
    # Get 2 lines before and after for context
    start_line = max(1, line - 2)
    end_line = min(length(lines), line + 2)

    lines
    |> Enum.with_index(1)
    |> Enum.filter(fn {_, idx} -> idx >= start_line and idx <= end_line end)
    |> Enum.map(fn {text, idx} ->
      marker = if idx == line, do: "> ", else: "  "
      "#{marker}#{String.pad_leading(Integer.to_string(idx), 3)} | #{text}"
    end)
    |> Enum.join("\n")
  end

  # Suggest similar names for unbound variable errors
  defp suggest_similar_names(name) do
    # Get common builtins that might be misspelled
    builtins = ["map", "filter", "foldl", "foldr", "head", "tail", "length", "reverse",
                "concat", "take", "drop", "show", "pure", "identity", "const",
                "Just", "Nothing", "Left", "Right", "True", "False"]

    builtins
    |> Enum.filter(fn builtin ->
      String.jaro_distance(String.downcase(name), String.downcase(builtin)) > 0.8
    end)
    |> Enum.take(3)
  end

  # Generate suggestions for type mismatch errors
  defp generate_type_mismatch_suggestions(t1, t2) do
    cond do
      # Int vs String
      match?({:ty_con, %{name: "Int"}}, t1) and match?({:ty_con, %{name: "String"}}, t2) ->
        ["Use 'show' to convert Int to String", "Use 'Int.fromString' to parse String to Int"]

      match?({:ty_con, %{name: "String"}}, t1) and match?({:ty_con, %{name: "Int"}}, t2) ->
        ["Use 'Int.fromString' to parse String to Int", "Use 'show' to convert Int to String"]

      # Maybe vs bare value
      match?({:ty_con, %{name: "Maybe"}}, t1) ->
        ["The value might be wrapped in Maybe - use pattern matching or 'fromMaybe'"]

      match?({:ty_con, %{name: "Maybe"}}, t2) ->
        ["Wrap the value with 'Just' or handle Nothing case"]

      # Array vs single value
      match?({:ty_con, %{name: "Array"}}, t1) ->
        ["Expected an Array - wrap single value in brackets: [value]"]

      match?({:ty_con, %{name: "Array"}}, t2) ->
        ["Got an Array but expected single value - use 'head' or index"]

      true ->
        []
    end
  end

  def handle_call({:get_completions, namespace, prefix}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        completions = ns.declarations
        |> Map.values()
        |> Enum.filter(fn m -> String.starts_with?(m.meta.name, prefix) end)
        |> Enum.map(fn m ->
          %{
            name: m.meta.name,
            kind: m.meta.kind,
            type: m.inferred_type
          }
        end)
        {:reply, {:ok, completions}, state}
    end
  end

  def handle_call({:add_import, namespace, imported_namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        # Check that imported namespace exists
        case Map.get(state.namespaces, imported_namespace) do
          nil ->
            {:reply, {:error, {:import_not_found, imported_namespace}}, state}

          _imported_ns ->
            # Check for circular imports
            if has_circular_import?(state, imported_namespace, namespace) do
              {:reply, {:error, {:circular_import, namespace, imported_namespace}}, state}
            else
              # Add import if not already present
              if imported_namespace in ns.imports do
                {:reply, {:ok, :already_imported}, state}
              else
                new_ns = %{ns | imports: [imported_namespace | ns.imports]}
                # Mark all declarations as stale since imports changed
                new_ns = mark_all_stale(new_ns)
                new_state = %{state | namespaces: Map.put(state.namespaces, namespace, new_ns)}
                {:reply, :ok, new_state}
              end
            end
        end
    end
  end

  def handle_call({:remove_import, namespace, imported_namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        if imported_namespace in ns.imports do
          new_ns = %{ns | imports: List.delete(ns.imports, imported_namespace)}
          # Mark all declarations as stale since imports changed
          new_ns = mark_all_stale(new_ns)
          new_state = %{state | namespaces: Map.put(state.namespaces, namespace, new_ns)}
          {:reply, :ok, new_state}
        else
          {:reply, {:error, :import_not_found}, state}
        end
    end
  end

  def handle_call({:list_imports, namespace}, _from, state) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:reply, {:error, :namespace_not_found}, state}

      ns ->
        {:reply, {:ok, ns.imports}, state}
    end
  end

  def handle_call(:export_state, _from, state) do
    # Export namespaces in a serializable format
    # Only save source_text - can re-parse and re-validate on load
    exported = Enum.map(state.namespaces, fn {ns_name, ns} ->
      declarations = Enum.map(ns.declarations, fn {_decl_id, managed} ->
        %{
          name: managed.meta.name,
          kind: Atom.to_string(managed.meta.kind),
          source: managed.source_text
        }
      end)

      %{
        name: ns_name,
        imports: ns.imports,
        declarations: declarations
      }
    end)

    {:reply, {:ok, exported}, state}
  end

  def handle_call({:import_state, session_data, merge}, _from, state) do
    # Clear existing state unless merging
    base_state = if merge, do: state, else: %State{}

    # Rebuild namespaces from session data
    result = Enum.reduce_while(session_data, {:ok, base_state}, fn ns_data, {:ok, acc_state} ->
      ns_name = ns_data["name"] || ns_data[:name]
      imports = ns_data["imports"] || ns_data[:imports] || []
      declarations = ns_data["declarations"] || ns_data[:declarations] || []

      # Create namespace
      ns = %NamespaceState{name: ns_name, imports: imports}
      acc_state = %{acc_state | namespaces: Map.put(acc_state.namespaces, ns_name, ns)}

      # Add declarations
      final_result = Enum.reduce_while(declarations, {:ok, acc_state}, fn decl_data, {:ok, inner_state} ->
        source = decl_data["source"] || decl_data[:source]
        case add_declaration_internal(inner_state, ns_name, source) do
          {:ok, _decl_id, new_state} -> {:cont, {:ok, new_state}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

      case final_result do
        {:ok, new_state} -> {:cont, {:ok, new_state}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)

    case result do
      {:ok, new_state} ->
        {:reply, {:ok, length(session_data)}, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:clear_all, _from, _state) do
    {:reply, :ok, %State{}}
  end

  # ============================================================================
  # Private Functions
  # ============================================================================

  # Internal version of add_declaration that works with state directly
  defp add_declaration_internal(state, namespace, source) do
    case Map.get(state.namespaces, namespace) do
      nil ->
        {:error, :namespace_not_found}

      ns ->
        case parse_declaration(source) do
          {:error, reason} ->
            {:error, {:parse_error, reason}}

          {:ok, decl, name, kind} ->
            decl_id = make_decl_id(namespace, kind, name, state.global_counter)

            meta = %DeclMetadata{
              decl_id: decl_id,
              namespace: namespace,
              name: name,
              kind: kind,
              status: :fresh
            }

            managed = %ManagedDecl{
              meta: meta,
              decl: decl,
              source_text: source
            }

            new_ns = %{ns |
              declarations: Map.put(ns.declarations, decl_id, managed),
              name_index: Map.put(ns.name_index, name, decl_id)
            }

            new_state = %{state |
              namespaces: Map.put(state.namespaces, namespace, new_ns),
              global_counter: state.global_counter + 1
            }

            {:ok, decl_id, new_state}
        end
    end
  end

  defp make_decl_id(namespace, kind, name, counter) do
    kind_str = Atom.to_string(kind)
    "#{namespace}:#{kind_str}:#{name}:#{counter}"
  end

  defp parse_declaration(source) do
    # Use the real Nova parser
    tokens = Nova.Compiler.Tokenizer.tokenize(source)

    case Nova.Compiler.Parser.parse_declaration(tokens) do
      {:left, err} ->
        {:error, err}

      {:right, {:tuple, decl, _rest}} ->
        case extract_decl_info(decl) do
          {:ok, name, kind} -> {:ok, decl, name, kind}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  # Extract name and kind from a parsed declaration
  defp extract_decl_info({:decl_function, f}) do
    {:ok, f.name, :function}
  end

  defp extract_decl_info({:decl_data_type, d}) do
    {:ok, d.name, :datatype}
  end

  defp extract_decl_info({:decl_type_alias, a}) do
    {:ok, a.name, :typealias}
  end

  defp extract_decl_info({:decl_type_class, c}) do
    {:ok, c.name, :typeclass}
  end

  defp extract_decl_info({:decl_type_class_instance, i}) do
    # Instance names are derived from class name + type
    instance_name = "#{i.class_name}_#{type_expr_to_string(i.ty)}"
    {:ok, instance_name, :instance}
  end

  defp extract_decl_info({:decl_foreign_import, f}) do
    {:ok, f.function_name, :foreign}
  end

  defp extract_decl_info({:decl_type_sig, s}) do
    {:ok, s.name, :typesig}
  end

  defp extract_decl_info({:decl_import, _}) do
    {:error, "Import declarations should not be added as namespace declarations"}
  end

  defp extract_decl_info({:decl_module, _}) do
    {:error, "Module headers should not be added as namespace declarations"}
  end

  defp extract_decl_info({:decl_type, t}) do
    {:ok, t.name, :typealias}
  end

  defp extract_decl_info(other) do
    {:error, "Unknown declaration type: #{inspect(other)}"}
  end

  # Convert type expression to string for instance naming
  defp type_expr_to_string({:ty_expr_con, name}), do: name
  defp type_expr_to_string({:ty_expr_var, name}), do: name
  defp type_expr_to_string({:ty_expr_app, t1, t2}) do
    "#{type_expr_to_string(t1)}_#{type_expr_to_string(t2)}"
  end
  defp type_expr_to_string({:ty_expr_parens, t}), do: type_expr_to_string(t)
  defp type_expr_to_string(_), do: "complex"

  defp find_declaration(state, decl_id) do
    # Parse namespace from decl_id
    case String.split(decl_id, ":", parts: 2) do
      [namespace | _] ->
        case Map.get(state.namespaces, namespace) do
          nil -> nil
          ns ->
            case Map.get(ns.declarations, decl_id) do
              nil -> nil
              managed -> {namespace, ns, managed}
            end
        end
      _ -> nil
    end
  end

  # Resolve raw dependency names to declaration IDs
  defp resolve_dependencies(state, namespace, raw_deps) do
    raw_deps
    |> MapSet.to_list()
    |> Enum.map(fn name -> Nova.Dependencies.resolve_name(state, namespace, name) end)
    |> Enum.filter(& &1)  # Remove nils (unresolved deps are builtins or external)
    |> MapSet.new()
  end

  defp mark_stale(state, decl_ids) do
    Enum.reduce(decl_ids, state, fn decl_id, acc_state ->
      case find_declaration(acc_state, decl_id) do
        nil -> acc_state
        {ns_name, ns, managed} ->
          new_managed = %{managed | meta: %{managed.meta | status: :stale}}
          new_ns = %{ns | declarations: Map.put(ns.declarations, decl_id, new_managed)}
          %{acc_state | namespaces: Map.put(acc_state.namespaces, ns_name, new_ns)}
      end
    end)
  end

  # Mark all declarations in a namespace as stale
  defp mark_all_stale(ns) do
    new_decls = ns.declarations
    |> Enum.map(fn {id, managed} ->
      new_managed = %{managed | meta: %{managed.meta | status: :stale}}
      {id, new_managed}
    end)
    |> Map.new()

    %{ns | declarations: new_decls}
  end

  # Check if adding an import would create a circular dependency
  # from_namespace is being imported into to_namespace
  # A cycle exists if from_namespace (directly or indirectly) already imports to_namespace
  defp has_circular_import?(state, from_namespace, to_namespace) do
    # Check if from_namespace directly or indirectly imports to_namespace
    visited = MapSet.new()
    check_import_cycle(state, from_namespace, to_namespace, visited)
  end

  defp check_import_cycle(state, current, target, visited) do
    cond do
      current == target ->
        true

      MapSet.member?(visited, current) ->
        false

      true ->
        case Map.get(state.namespaces, current) do
          nil -> false
          ns ->
            visited = MapSet.put(visited, current)
            Enum.any?(ns.imports, fn imp ->
              check_import_cycle(state, imp, target, visited)
            end)
        end
    end
  end

end
