### Breaking

- `getDiffusionPipeliningSupport` :: `DiffusionPipeliningSupport` was added to `NodeKernelArgs`
  and `NodeKernel` to enable gracefully handling some types of protocol errors when diffusion
  is ran with pipelining enabled.
