#!/usr/bin/env elixir
# Test script for Nova reference tracking (refFrom/refTo)

defmodule RefTest do
  def load_modules do
    core_dir = "priv/core"

    modules = [
      "Data/Maybe.core",
      "Data/Either.core",
      "Data/Array.core",
      "Nova/Prelude.core",
      "OTP/Ets.core",
      "Nova/NamespaceService.core",
    ]

    IO.puts("Loading required modules...")

    Enum.each(modules, fn rel_path ->
      path = Path.join(core_dir, rel_path)
      mod_name = rel_path |> String.replace("/", ".") |> String.replace(".core", "")

      case File.read(path) do
        {:ok, content} ->
          case Nova.CoreErlangEval.compile_and_load(content) do
            {:ok, _mod} ->
              IO.puts("  ✓ #{mod_name}")
            {:error, reason} ->
              IO.puts("  ✗ #{mod_name}: #{inspect(reason)}")
              System.halt(1)
          end
        {:error, reason} ->
          IO.puts("  ✗ #{mod_name}: file error #{inspect(reason)}")
          System.halt(1)
      end
    end)

    IO.puts("")
  end

  def run do
    load_modules()
    IO.puts("=== Testing Nova Reference Tracking ===\n")

    # Initialize namespace service
    IO.puts("1. Initializing NamespaceService...")
    case apply(:"Nova.NamespaceService", :init, [:unit]) do
      {:Right, state} ->
        IO.puts("   OK - Service initialized\n")
        run_tests(state)

      {:Left, err} ->
        IO.puts("   FAILED: #{inspect(err)}")
        System.halt(1)
    end
  end

  def run_tests(st) do
    passed = 0
    failed = 0

    # Test 2: Create namespaces and add declarations
    IO.puts("2. Creating namespaces and adding declarations...")
    {:Right, _} = apply(:"Nova.NamespaceService", :createNamespace, [st, "Test"])
    {:Right, _} = apply(:"Nova.NamespaceService", :createNamespace, [st, "Other"])
    {:Right, _} = apply(:"Nova.NamespaceService", :addDecl, [st, "Test", "foo", "foo x = x + 1", :FunctionDecl])
    {:Right, _} = apply(:"Nova.NamespaceService", :addDecl, [st, "Test", "bar", "bar y = foo y", :FunctionDecl])
    {:Right, _} = apply(:"Nova.NamespaceService", :addDecl, [st, "Test", "baz", "baz z = bar (foo z)", :FunctionDecl])
    {:Right, _} = apply(:"Nova.NamespaceService", :addDecl, [st, "Other", "helper", "helper = 42", :FunctionDecl])
    IO.puts("   OK - 2 namespaces, 4 declarations added\n")

    # Test 3: Add references
    IO.puts("3. Adding references...")
    # bar calls foo
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.bar", "Test.foo", :CallRef])
    # baz calls bar and foo
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.baz", "Test.bar", :CallRef])
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.baz", "Test.foo", :CallRef])
    # Add a type reference
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.foo", "Prelude.Int", :TypeRef])
    IO.puts("   OK - 4 references added\n")

    # Test 4: getRefsFrom
    IO.puts("4. Testing getRefsFrom...")
    refs_from_baz = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.baz"])

    {passed, failed} = if length(refs_from_baz) == 2 do
      IO.puts("   OK - Test.baz has 2 outgoing refs")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 2 refs, got #{length(refs_from_baz)}")
      {passed, failed + 1}
    end

    refs_from_bar = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.bar"])
    {passed, failed} = if length(refs_from_bar) == 1 do
      IO.puts("   OK - Test.bar has 1 outgoing ref")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 1 ref, got #{length(refs_from_bar)}")
      {passed, failed + 1}
    end

    # Test 5: getRefsTo
    IO.puts("\n5. Testing getRefsTo...")
    refs_to_foo = apply(:"Nova.NamespaceService", :getRefsTo, [st, "Test.foo"])

    {passed, failed} = if length(refs_to_foo) == 2 do
      IO.puts("   OK - Test.foo is referenced by 2 decls")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 2 refs to foo, got #{length(refs_to_foo)}")
      {passed, failed + 1}
    end

    refs_to_bar = apply(:"Nova.NamespaceService", :getRefsTo, [st, "Test.bar"])
    {passed, failed} = if length(refs_to_bar) == 1 do
      IO.puts("   OK - Test.bar is referenced by 1 decl")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 1 ref to bar, got #{length(refs_to_bar)}")
      {passed, failed + 1}
    end

    # Test 6: Verify ref details
    IO.puts("\n6. Verifying reference details...")
    [ref | _] = refs_from_bar
    {passed, failed} = if ref.sourceId == "Test.bar" and ref.destId == "Test.foo" and ref.refType == :CallRef do
      IO.puts("   OK - Ref from bar has correct sourceId, destId, refType")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Ref details incorrect: #{inspect(ref)}")
      {passed, failed + 1}
    end

    # Test 7: getRefFromCount / getRefToCount
    IO.puts("\n7. Testing ref counts...")
    from_count = apply(:"Nova.NamespaceService", :getRefFromCount, [st, "Test.baz"])
    {passed, failed} = if from_count == 2 do
      IO.puts("   OK - getRefFromCount(Test.baz) = 2")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected count 2, got #{from_count}")
      {passed, failed + 1}
    end

    to_count = apply(:"Nova.NamespaceService", :getRefToCount, [st, "Test.foo"])
    {passed, failed} = if to_count == 2 do
      IO.puts("   OK - getRefToCount(Test.foo) = 2")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected count 2, got #{to_count}")
      {passed, failed + 1}
    end

    # Test 8: clearRefsFrom
    IO.puts("\n8. Testing clearRefsFrom...")
    :unit = apply(:"Nova.NamespaceService", :clearRefsFrom, [st, "Test.baz"])
    refs_from_baz_after = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.baz"])
    {passed, failed} = if length(refs_from_baz_after) == 0 do
      IO.puts("   OK - Refs from Test.baz cleared")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 0 refs after clear, got #{length(refs_from_baz_after)}")
      {passed, failed + 1}
    end

    # Verify refTo was also updated
    refs_to_foo_after = apply(:"Nova.NamespaceService", :getRefsTo, [st, "Test.foo"])
    {passed, failed} = if length(refs_to_foo_after) == 1 do
      IO.puts("   OK - RefTo index updated (foo now has 1 ref)")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 1 ref to foo after clear, got #{length(refs_to_foo_after)}")
      {passed, failed + 1}
    end

    # Test 9: addRefs (batch add)
    IO.puts("\n9. Testing addRefs (batch)...")
    batch_refs = [
      %{destId: "Test.foo", refType: :CallRef},
      %{destId: "Test.bar", refType: :CallRef},
      %{destId: "Prelude.show", refType: :CallRef}
    ]
    :unit = apply(:"Nova.NamespaceService", :addRefs, [st, "Test.baz", batch_refs])
    refs_from_baz_batch = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.baz"])
    {passed, failed} = if length(refs_from_baz_batch) == 3 do
      IO.puts("   OK - Batch added 3 refs")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected 3 refs after batch, got #{length(refs_from_baz_batch)}")
      {passed, failed + 1}
    end

    # Test 10: Different RefTypes
    IO.puts("\n10. Testing different RefTypes...")
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.typedFn", "Data.Maybe.Maybe", :TypeRef])
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.patternFn", "Data.Maybe.Just", :PatternRef])
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.ctorFn", "Data.Maybe.Nothing", :ConstructorRef])
    :unit = apply(:"Nova.NamespaceService", :addRef, [st, "Test.Main", "Data.Maybe", :ImportRef])

    refs = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.typedFn"])
    type_ref = hd(refs)
    {passed, failed} = if type_ref.refType == :TypeRef do
      IO.puts("   OK - TypeRef stored correctly")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected TypeRef, got #{inspect(type_ref.refType)}")
      {passed, failed + 1}
    end

    refs = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.patternFn"])
    pat_ref = hd(refs)
    {passed, failed} = if pat_ref.refType == :PatternRef do
      IO.puts("   OK - PatternRef stored correctly")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected PatternRef, got #{inspect(pat_ref.refType)}")
      {passed, failed + 1}
    end

    refs = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.ctorFn"])
    ctor_ref = hd(refs)
    {passed, failed} = if ctor_ref.refType == :ConstructorRef do
      IO.puts("   OK - ConstructorRef stored correctly")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected ConstructorRef, got #{inspect(ctor_ref.refType)}")
      {passed, failed + 1}
    end

    refs = apply(:"Nova.NamespaceService", :getRefsFrom, [st, "Test.Main"])
    import_ref = hd(refs)
    {passed, failed} = if import_ref.refType == :ImportRef do
      IO.puts("   OK - ImportRef stored correctly")
      {passed + 1, failed}
    else
      IO.puts("   FAILED - Expected ImportRef, got #{inspect(import_ref.refType)}")
      {passed, failed + 1}
    end

    # Summary
    IO.puts("\n=== Test Summary ===")
    IO.puts("Passed: #{passed}")
    IO.puts("Failed: #{failed}")

    if failed > 0 do
      System.halt(1)
    else
      IO.puts("\nAll tests passed!")
    end
  end
end

RefTest.run()
