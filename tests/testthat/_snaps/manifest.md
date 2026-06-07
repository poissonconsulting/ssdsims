# manifest: read errors when no manifest exists

    Code
      ssd_read_manifest("no-such-results-dir")
    Condition
      Error:
      ! No manifest found at "no-such-results-dir/manifest.json".

