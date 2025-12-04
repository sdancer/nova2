#!/usr/bin/env python3
"""
TCP to stdio proxy for Nova MCP server.

Connects to the Nova MCP server on TCP port 9999 and proxies
JSON-RPC messages between stdio and the TCP socket.

Usage:
    ./nova-mcp-proxy.py [--port PORT] [--host HOST]
"""

import socket
import sys
import threading
import argparse


def tcp_to_stdout(sock):
    """Read from TCP socket and write to stdout."""
    buffer = b""
    while True:
        try:
            data = sock.recv(4096)
            if not data:
                break
            buffer += data
            # Process complete lines
            while b"\n" in buffer:
                line, buffer = buffer.split(b"\n", 1)
                sys.stdout.write(line.decode("utf-8") + "\n")
                sys.stdout.flush()
        except (ConnectionResetError, BrokenPipeError, OSError):
            break


def stdin_to_tcp(sock):
    """Read from stdin and write to TCP socket."""
    while True:
        try:
            line = sys.stdin.readline()
            if not line:
                break
            sock.sendall(line.encode("utf-8"))
        except (ConnectionResetError, BrokenPipeError, OSError):
            break


def main():
    parser = argparse.ArgumentParser(description="TCP to stdio proxy for Nova MCP")
    parser.add_argument("--host", default="localhost", help="Host to connect to")
    parser.add_argument("--port", type=int, default=9999, help="Port to connect to")
    args = parser.parse_args()

    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((args.host, args.port))
    except ConnectionRefusedError:
        sys.stderr.write(f"Error: Could not connect to {args.host}:{args.port}\n")
        sys.stderr.write("Make sure the Nova MCP server is running: ./nova mcp\n")
        sys.exit(1)

    # Start reader thread (TCP -> stdout)
    reader = threading.Thread(target=tcp_to_stdout, args=(sock,), daemon=True)
    reader.start()

    # Run writer in main thread (stdin -> TCP)
    try:
        stdin_to_tcp(sock)
    except KeyboardInterrupt:
        pass
    finally:
        sock.close()


if __name__ == "__main__":
    main()
