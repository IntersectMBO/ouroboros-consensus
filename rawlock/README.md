# RAWLock

A writer-biased RAW lock.

It allows for multiple readers to run concurrently with at most one appender, or
a single writer running on isolation.

The code is safe in the presence of async exceptions, meaning that each actor
will cleanup after itself if an exception is received.

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests.

Before contributing, please ensure that:

- Code is well-documented
- Tests are written for new features or bug fixes

## License

This project is licensed under the terms of the [Apache 2.0](./LICENSE) license.
