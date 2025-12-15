defmodule Nova.HTTPServer do
  @moduledoc """
  Simple HTTP server wrapper for Nova.
  Uses :gen_tcp with http_bin packet mode for simplicity.
  """

  def start(port, handler_module) do
    case :gen_tcp.listen(port, [:binary, packet: :http_bin, active: false, reuseaddr: true, ip: {0, 0, 0, 0}]) do
      {:ok, listen_socket} ->
        IO.puts("[INFO] Nova HTTP server listening on http://0.0.0.0:#{port}")
        accept_loop(listen_socket, handler_module)

      {:error, reason} ->
        IO.puts("[ERROR] Failed to start server: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp accept_loop(listen_socket, handler_module) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, socket} ->
        spawn(fn -> handle_connection(socket, handler_module) end)
        accept_loop(listen_socket, handler_module)

      {:error, :closed} ->
        :ok

      {:error, reason} ->
        IO.puts("[ERROR] Accept failed: #{inspect(reason)}")
        :error
    end
  end

  defp handle_connection(socket, handler_module) do
    case read_request(socket) do
      {:ok, request} ->
        response = call_handler(handler_module, request)
        send_response(socket, response)
        :gen_tcp.close(socket)

      {:error, _reason} ->
        :gen_tcp.close(socket)
    end
  end

  defp read_request(socket) do
    case read_headers(socket, %{method: "GET", path: "/", headers: %{}, content_length: 0}) do
      {:ok, req} ->
        body = read_body(socket, req.content_length)
        {:ok, Map.put(req, :body, body)}

      error ->
        error
    end
  end

  defp read_headers(socket, acc) do
    case :gen_tcp.recv(socket, 0, 5000) do
      {:ok, {:http_request, method, {:abs_path, path}, _version}} ->
        read_headers(socket, %{acc | method: to_string(method), path: to_string(path)})

      {:ok, {:http_header, _, :"Content-Length", _, value}} ->
        len = String.to_integer(to_string(value))
        read_headers(socket, %{acc | content_length: len})

      {:ok, {:http_header, _, name, _, value}} ->
        headers = Map.put(acc.headers, to_string(name), to_string(value))
        read_headers(socket, %{acc | headers: headers})

      {:ok, :http_eoh} ->
        {:ok, acc}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp read_body(_socket, 0), do: ""

  defp read_body(socket, length) do
    :inet.setopts(socket, packet: :raw)

    case :gen_tcp.recv(socket, length, 5000) do
      {:ok, data} -> to_string(data)
      _ -> ""
    end
  end

  defp call_handler(handler_module, request) do
    method = request.method
    path = request.path
    body = request.body

    try do
      # Call Nova handler: handler_module.handle(method, path, body)
      # Nova uses binary strings
      result = apply(handler_module, :handle, [method, path, body])
      normalize_response(result)
    rescue
      e ->
        IO.puts("[ERROR] Handler error: #{inspect(e)} < #{method} #{path}")
        IO.puts("[ERROR] Request body: #{inspect(body)}")
        IO.puts("[ERROR] Stacktrace:")
        Exception.format(:error, e, __STACKTRACE__)
        |> String.split("\n")
        |> Enum.take(15)
        |> Enum.each(&IO.puts("[ERROR]   #{&1}"))
        {500, "text/plain", "Internal Server Error: #{inspect(e)}"}
    catch
      kind, reason ->
        IO.puts("[ERROR] Handler #{kind}: #{inspect(reason)} < #{method} #{path}")
        IO.puts("[ERROR] Request body: #{inspect(body)}")
        IO.puts("[ERROR] Stacktrace:")
        Exception.format(kind, reason, __STACKTRACE__)
        |> String.split("\n")
        |> Enum.take(15)
        |> Enum.each(&IO.puts("[ERROR]   #{&1}"))
        {500, "text/plain", "Internal Server Error: #{inspect(reason)}"}
    end
  end

  defp normalize_response({status, content_type, body}) when is_integer(status) do
    {status, to_string(content_type), to_string(body)}
  end

  # Handle Nova records (Erlang maps) with status/contentType/body fields
  defp normalize_response(%{status: status, contentType: ct, body: body}) when is_integer(status) do
    {status, to_string(ct), to_string(body)}
  end

  # Also handle atom keys
  defp normalize_response(map) when is_map(map) do
    status = Map.get(map, :status) || Map.get(map, "status") || 200
    ct = Map.get(map, :contentType) || Map.get(map, "contentType") || "text/html"
    body = Map.get(map, :body) || Map.get(map, "body") || ""
    {status, to_string(ct), to_string(body)}
  end

  defp normalize_response(body) when is_binary(body) or is_list(body) do
    {200, "text/html", to_string(body)}
  end

  defp normalize_response(_) do
    {500, "text/plain", "Invalid response"}
  end

  defp send_response(socket, {status, content_type, body}) do
    status_text = status_text(status)
    body_bytes = if is_list(body), do: :erlang.list_to_binary(body), else: body
    content_length = byte_size(body_bytes)

    response = """
    HTTP/1.1 #{status} #{status_text}\r
    Content-Type: #{content_type}\r
    Content-Length: #{content_length}\r
    Connection: close\r
    \r
    """

    :gen_tcp.send(socket, response)
    :gen_tcp.send(socket, body_bytes)
  end

  defp status_text(200), do: "OK"
  defp status_text(201), do: "Created"
  defp status_text(400), do: "Bad Request"
  defp status_text(404), do: "Not Found"
  defp status_text(500), do: "Internal Server Error"
  defp status_text(_), do: "Unknown"
end
