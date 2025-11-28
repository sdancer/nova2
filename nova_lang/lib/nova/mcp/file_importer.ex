defmodule Nova.MCP.FileImporter do
  @moduledoc """
  File import utilities for the Nova MCP server.

  Handles importing Nova source files into namespaces, supporting:
  - Single file imports
  - Directory-based imports with namespace derivation from file paths
  """

  @doc """
  Import a Nova source file into a namespace.

  Parses the file and adds each declaration to the namespace.
  Creates the namespace if it doesn't exist.

  ## Parameters
  - `server` - The NamespaceService process
  - `path` - Path to the Nova source file
  - `namespace` - Target namespace name

  ## Returns
  - `{:ok, results}` - List of `{:ok, decl_id}` or `{:error, reason}` for each declaration
  - `{:error, reason}` - If file read or parse fails
  """
  def import_file(server, path, namespace) do
    case File.read(path) do
      {:error, :enoent} ->
        {:error, :file_not_found}

      {:error, reason} ->
        {:error, {:file_read_error, reason}}

      {:ok, source} ->
        import_source(server, source, namespace)
    end
  end

  @doc """
  Import Nova source code (as a string) into a namespace.

  ## Parameters
  - `server` - The NamespaceService process
  - `source` - Nova source code string
  - `namespace` - Target namespace name

  ## Returns
  - `{:ok, results}` - List of results for each declaration
  - `{:error, reason}` - If parsing fails
  """
  def import_source(server, source, namespace) do
    tokens = Nova.Compiler.Tokenizer.tokenize(source)

    case Nova.Compiler.Parser.parse_module(tokens) do
      {:left, err} ->
        {:error, {:parse_error, err}}

      {:right, {:tuple, mod, _rest}} ->
        # Ensure namespace exists
        Nova.NamespaceService.create_namespace(server, namespace)

        # Process imports from the module
        process_imports(server, namespace, mod.declarations)

        # Filter out import declarations and module header
        declarations = mod.declarations
        |> Enum.filter(fn decl ->
          case decl do
            {:decl_import, _} -> false
            {:decl_module, _} -> false
            _ -> true
          end
        end)

        # Add each declaration
        results = Enum.map(declarations, fn decl ->
          # Extract source text for this declaration
          decl_source = extract_declaration_source(source, decl)
          Nova.NamespaceService.add_declaration(server, namespace, decl_source)
        end)

        {:ok, results}
    end
  end

  @doc """
  Import all Nova files from a directory into namespaces.

  File paths determine namespace names. For example:
  - `src/Math/Utils.nova` with prefix `MyApp` -> `MyApp.Math.Utils`
  - `src/Core.nova` with prefix `MyApp` -> `MyApp.Core`

  ## Parameters
  - `server` - The NamespaceService process
  - `dir_path` - Path to the directory to scan
  - `namespace_prefix` - Prefix for generated namespace names

  ## Returns
  - `{:ok, results}` - Map of file path to declaration count
  - `{:error, reason}` - If directory doesn't exist
  """
  def import_directory(server, dir_path, namespace_prefix) do
    if File.dir?(dir_path) do
      results =
        Path.wildcard(Path.join(dir_path, "**/*.nova"))
        |> Enum.map(fn file_path ->
          namespace = file_to_namespace(file_path, dir_path, namespace_prefix)
          case import_file(server, file_path, namespace) do
            {:ok, decl_results} ->
              success_count = Enum.count(decl_results, fn
                {:ok, _} -> true
                _ -> false
              end)
              {Path.relative_to(file_path, dir_path), success_count}

            {:error, _reason} ->
              {Path.relative_to(file_path, dir_path), 0}
          end
        end)

      {:ok, results}
    else
      {:error, :directory_not_found}
    end
  end

  @doc """
  Convert a file path to a namespace name.

  ## Examples

      iex> file_to_namespace("src/Math/Utils.nova", "src", "MyApp")
      "MyApp.Math.Utils"

      iex> file_to_namespace("lib/Core.nova", "lib", "App")
      "App.Core"
  """
  def file_to_namespace(file_path, base_dir, prefix) do
    relative = Path.relative_to(file_path, base_dir)
    without_ext = Path.rootname(relative)

    # Convert path separators to dots
    parts = Path.split(without_ext)
    namespace_suffix = Enum.join(parts, ".")

    if prefix == "" or prefix == nil do
      namespace_suffix
    else
      "#{prefix}.#{namespace_suffix}"
    end
  end

  # Private functions

  # Process import declarations and set up namespace imports
  defp process_imports(server, namespace, declarations) do
    declarations
    |> Enum.filter(fn
      {:decl_import, _} -> true
      _ -> false
    end)
    |> Enum.each(fn {:decl_import, imp} ->
      # Import module name is the namespace to import from
      import_ns = imp.module_name

      # Create the imported namespace if it doesn't exist
      Nova.NamespaceService.create_namespace(server, import_ns)

      # Add the import
      Nova.NamespaceService.add_import(server, namespace, import_ns)
    end)
  end

  # Extract source text for a single declaration
  # For now, we use a simple approach: re-generate the source from AST
  # This could be improved with source location tracking
  defp extract_declaration_source(_source, decl) do
    # Use pattern matching to format each declaration type
    case decl do
      {:decl_function, f} ->
        format_function(f)

      {:decl_data_type, d} ->
        format_data_type(d)

      {:decl_type_alias, a} ->
        format_type_alias(a)

      {:decl_type_class, c} ->
        format_type_class(c)

      {:decl_type_class_instance, i} ->
        format_instance(i)

      {:decl_foreign_import, f} ->
        format_foreign(f)

      {:decl_type_sig, s} ->
        format_type_sig(s)

      {:decl_infix, i} ->
        format_infix(i)

      {:decl_newtype, n} ->
        format_newtype(n)

      _ ->
        # Fallback: inspect the AST
        inspect(decl)
    end
  end

  defp format_function(f) do
    # Handle both :params and :parameters field names
    params_list = Map.get(f, :params) || Map.get(f, :parameters) || []
    params = Enum.map(params_list, &format_pattern/1) |> Enum.join(" ")
    body = format_expr(f.body)

    where_decls = Map.get(f, :where_decls) || []
    where_clause = if where_decls != [] do
      where_str = where_decls
      |> Enum.map(&format_where_decl/1)
      |> Enum.join("\n  ")
      "\n  where\n  #{where_str}"
    else
      ""
    end

    "#{f.name} #{params} = #{body}#{where_clause}"
  end

  defp format_where_decl({:decl_function, f}), do: format_function(f)
  defp format_where_decl(decl), do: inspect(decl)

  defp format_data_type(d) do
    type_vars = Map.get(d, :type_vars) || Map.get(d, :type_params) || []
    type_params = Enum.join(type_vars, " ")
    type_params_str = if type_params == "", do: "", else: " #{type_params}"

    constructors = d.constructors
    |> Enum.map(fn c ->
      args = Enum.map(c.fields, &format_type_expr/1) |> Enum.join(" ")
      if args == "", do: c.name, else: "#{c.name} #{args}"
    end)
    |> Enum.join(" | ")

    "data #{d.name}#{type_params_str} = #{constructors}"
  end

  defp format_type_alias(a) do
    type_vars = Map.get(a, :type_vars) || Map.get(a, :type_params) || []
    type_params = Enum.join(type_vars, " ")
    type_params_str = if type_params == "", do: "", else: " #{type_params}"
    "type #{a.name}#{type_params_str} = #{format_type_expr(a.ty)}"
  end

  defp format_type_class(c) do
    constraints_str = if c.constraints && c.constraints != [] do
      cs = Enum.map(c.constraints, fn {class, var} -> "#{class} #{var}" end) |> Enum.join(", ")
      "(#{cs}) => "
    else
      ""
    end

    methods_str = c.methods
    |> Enum.map(fn m -> "  #{m.name} :: #{format_type_expr(m.ty)}" end)
    |> Enum.join("\n")

    "class #{constraints_str}#{c.name} #{c.type_var} where\n#{methods_str}"
  end

  defp format_instance(i) do
    constraints_str = if i.constraints && i.constraints != [] do
      cs = Enum.map(i.constraints, fn {class, ty} -> "#{class} #{format_type_expr(ty)}" end) |> Enum.join(", ")
      "(#{cs}) => "
    else
      ""
    end

    methods_str = i.methods
    |> Enum.map(fn m -> "  " <> format_function(m) end)
    |> Enum.join("\n")

    "instance #{constraints_str}#{i.class_name} #{format_type_expr(i.ty)} where\n#{methods_str}"
  end

  defp format_foreign(f) do
    "foreign import #{f.function_name} :: #{format_type_expr(f.type_expr)}"
  end

  defp format_type_sig(s) do
    "#{s.name} :: #{format_type_expr(s.ty)}"
  end

  defp format_infix(i) do
    dir = case i.assoc do
      :left -> "infixl"
      :right -> "infixr"
      _ -> "infix"
    end
    "#{dir} #{i.precedence} #{i.op}"
  end

  defp format_newtype(n) do
    type_vars = Map.get(n, :type_vars) || Map.get(n, :type_params) || []
    type_params = Enum.join(type_vars, " ")
    type_params_str = if type_params == "", do: "", else: " #{type_params}"
    field = format_type_expr(hd(n.constructor.fields))
    "newtype #{n.name}#{type_params_str} = #{n.constructor.name} #{field}"
  end

  # Pattern formatting
  defp format_pattern({:pat_var, name}), do: name
  defp format_pattern({:pat_wildcard}), do: "_"
  defp format_pattern({:pat_lit, lit}), do: format_literal(lit)
  defp format_pattern({:pat_con, %{name: name, patterns: []}}), do: name
  defp format_pattern({:pat_con, %{name: name, patterns: ps}}) do
    args = Enum.map(ps, &format_pattern/1) |> Enum.join(" ")
    "(#{name} #{args})"
  end
  defp format_pattern({:pat_record, %{fields: fields}}) do
    fs = Enum.map(fields, fn {k, v} -> "#{k}: #{format_pattern(v)}" end) |> Enum.join(", ")
    "{ #{fs} }"
  end
  defp format_pattern({:pat_list, patterns}) do
    ps = Enum.map(patterns, &format_pattern/1) |> Enum.join(", ")
    "[#{ps}]"
  end
  defp format_pattern({:pat_parens, p}), do: "(#{format_pattern(p)})"
  defp format_pattern(other), do: inspect(other)

  # Expression formatting
  defp format_expr({:expr_var, name}), do: name
  defp format_expr({:expr_lit, lit}), do: format_literal(lit)
  defp format_expr({:expr_app, f, arg}), do: "(#{format_expr(f)} #{format_expr(arg)})"
  defp format_expr({:expr_lambda, %{params: ps, body: body}}) do
    params = Enum.map(ps, &format_pattern/1) |> Enum.join(" ")
    "\\#{params} -> #{format_expr(body)}"
  end
  defp format_expr({:expr_if, %{cond: c, then_expr: t, else_expr: e}}) do
    "if #{format_expr(c)} then #{format_expr(t)} else #{format_expr(e)}"
  end
  defp format_expr({:expr_let, %{bindings: bs, body: body}}) do
    bindings = Enum.map(bs, fn {name, expr} -> "#{name} = #{format_expr(expr)}" end) |> Enum.join("; ")
    "let #{bindings} in #{format_expr(body)}"
  end
  defp format_expr({:expr_case, %{expr: e, branches: bs}}) do
    branches = Enum.map(bs, fn b ->
      pattern = format_pattern(b.pattern)
      guard = if b.guard, do: " | #{format_expr(b.guard)}", else: ""
      "#{pattern}#{guard} -> #{format_expr(b.body)}"
    end) |> Enum.join("; ")
    "case #{format_expr(e)} of #{branches}"
  end
  defp format_expr({:expr_do, stmts}) do
    formatted = Enum.map(stmts, &format_stmt/1) |> Enum.join("; ")
    "do #{formatted}"
  end
  defp format_expr({:expr_record, %{fields: fields}}) do
    fs = Enum.map(fields, fn {k, v} -> "#{k}: #{format_expr(v)}" end) |> Enum.join(", ")
    "{ #{fs} }"
  end
  defp format_expr({:expr_record_access, %{record: r, field: f}}) do
    "#{format_expr(r)}.#{f}"
  end
  defp format_expr({:expr_list, exprs}) do
    es = Enum.map(exprs, &format_expr/1) |> Enum.join(", ")
    "[#{es}]"
  end
  defp format_expr({:expr_tuple, exprs}) do
    es = Enum.map(exprs, &format_expr/1) |> Enum.join(", ")
    "(#{es})"
  end
  defp format_expr({:expr_parens, e}), do: "(#{format_expr(e)})"
  defp format_expr({:expr_typed, %{expr: e, ty: t}}) do
    "(#{format_expr(e)} :: #{format_type_expr(t)})"
  end
  defp format_expr({:expr_infix, %{left: l, op: op, right: r}}) do
    "#{format_expr(l)} #{op} #{format_expr(r)}"
  end
  defp format_expr({:expr_negate, e}), do: "-#{format_expr(e)}"
  defp format_expr(other), do: inspect(other)

  defp format_stmt({:stmt_bind, %{pattern: p, expr: e}}) do
    "#{format_pattern(p)} <- #{format_expr(e)}"
  end
  defp format_stmt({:stmt_let, %{name: n, expr: e}}) do
    "let #{n} = #{format_expr(e)}"
  end
  defp format_stmt({:stmt_expr, e}), do: format_expr(e)
  defp format_stmt(other), do: inspect(other)

  # Literal formatting
  defp format_literal({:lit_int, n}), do: Integer.to_string(n)
  defp format_literal({:lit_float, n}), do: Float.to_string(n)
  defp format_literal({:lit_string, s}), do: "\"#{s}\""
  defp format_literal({:lit_char, c}), do: "'#{c}'"
  defp format_literal({:lit_bool, true}), do: "true"
  defp format_literal({:lit_bool, false}), do: "false"
  defp format_literal(other), do: inspect(other)

  # Type expression formatting
  defp format_type_expr({:ty_expr_con, name}), do: name
  defp format_type_expr({:ty_expr_var, name}), do: name
  defp format_type_expr({:ty_expr_app, t1, t2}) do
    "#{format_type_expr(t1)} #{format_type_expr(t2)}"
  end
  defp format_type_expr({:ty_expr_arrow, from, to}) do
    "#{format_type_expr(from)} -> #{format_type_expr(to)}"
  end
  defp format_type_expr({:ty_expr_parens, t}), do: "(#{format_type_expr(t)})"
  defp format_type_expr({:ty_expr_record, %{fields: fields}}) do
    fs = Enum.map(fields, fn {k, v} -> "#{k} :: #{format_type_expr(v)}" end) |> Enum.join(", ")
    "{ #{fs} }"
  end
  defp format_type_expr({:ty_expr_tuple, types}) do
    ts = Enum.map(types, &format_type_expr/1) |> Enum.join(", ")
    "(#{ts})"
  end
  defp format_type_expr({:ty_expr_list, t}), do: "[#{format_type_expr(t)}]"
  defp format_type_expr({:ty_expr_forall, vars, t}) do
    "forall #{Enum.join(vars, " ")}. #{format_type_expr(t)}"
  end
  defp format_type_expr({:ty_expr_constrained, constraints, t}) do
    cs = Enum.map(constraints, fn {class, var} -> "#{class} #{var}" end) |> Enum.join(", ")
    "(#{cs}) => #{format_type_expr(t)}"
  end
  defp format_type_expr(other), do: inspect(other)
end
